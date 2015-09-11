{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses               #-}
{-# LANGUAGE OverloadedStrings, RankNTypes, TemplateHaskell, TypeFamilies #-}
module Language.ONScripter.Off.Compiler.NScript
       (compileNScript, writeNScript, NScript, encodeNScript) where
import Language.ONScripter.Off.Syntax
import Language.ONScripter.Off.Types
import Language.ONScripter.Off.Utils

import           Control.Lens
import           Control.Monad.Operational
import           Control.Monad.RWS
import           Control.Monad.Writer      (runWriterT)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.Char
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IS
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.Typeable
import           GHC.IO.Encoding
import           System.IO

type Compiler = RWS () Compiled OffState
type Compiled = ([T.Text], [T.Text], HashMap Text [T.Text])

newtype NScript  = NScript { runNScript :: T.Text }
                 deriving (Eq, Ord, Typeable)

instance Show NScript where
  show (NScript scr) = T.unpack scr

data OffState = OffState { _flags       :: IntSet
                         , _vars        :: IntSet
                         , _subroutines :: HashMap T.Text Int
                         }

defaultState :: OffState
defaultState = OffState { _flags = IS.fromList [0..999]
                        , _vars  = IS.fromList [0..999]
                        , _subroutines = HM.empty
                        }

makeLenses ''OffState
makePrisms ''OffState

addDef :: MonadWriter Compiled m => Text -> m ()
addDef  t = tell ([t], [], HM.empty)

cmd_ :: MonadWriter Compiled m => (() -> m a) -> Text -> [Text] -> m a
cmd_ k t xs = k =<< cmd t xs

cmd :: MonadWriter Compiled m => Text -> [Text] -> m ()
cmd t [] = tell ([], [t], HM.empty)
cmd t ts = tell ([], [t <> " " <> T.intercalate "," ts], HM.empty)

sanitize :: Text -> Text
sanitize =
  T.filter (\c -> (c == '_' || isAlphaNum c) && isAscii c)
  . T.dropWhile (not . isAlpha)

defSubM :: (MonadWriter Compiled m, MonadState OffState m)
       => Text -> Off () -> m Subroutine
defSubM name0 act = do
  let seed = sanitize name0
  Just i <- subroutines . at seed <%= Just . maybe 0 succ
  let name = "sub_" <> seed <> "_" <> tshow i
  bs <- skim act
  tell ([], [], HM.singleton name bs)
  return $ Subroutine name

attain :: (MonadWriter Compiled m, MonadState OffState m)
       => Lens' OffState IntSet -> Prism' a Int -> m a
attain l mk = do
  (k, u) <- uses l IS.deleteFindMin
  l .= u
  return $ k ^. re mk

free :: (MonadWriter Compiled m, MonadState OffState m)
     => Lens' OffState IntSet -> Iso' a Int -> a -> m ()
free l mk v = l %= IS.insert (v ^. mk)

with :: (MonadWriter Compiled m, MonadState OffState m)
     => Lens' OffState IntSet -> Prism' a Int -> (a -> m ()) -> m ()
with l mk act = do
  (k, u) <- uses l IS.deleteFindMin
  l .= u
  act (k ^. re mk)
  l %= IS.insert k

allocVar :: (MonadWriter Compiled m, MonadState OffState m) => m Var
allocVar = attain vars _Var

freeVar :: (MonadWriter Compiled m, MonadState OffState m) => Var -> m ()
freeVar = free vars _Var

withVar :: (MonadWriter Compiled m, MonadState OffState m)
        => (Var -> m ()) -> m ()
withVar = with vars _Var

allocFlag :: (MonadWriter Compiled m, MonadState OffState m) => m (Flag a)
allocFlag = attain flags _Flag

freeFlag :: (MonadWriter Compiled m, MonadState OffState m) => Flag a -> m ()
freeFlag = free flags _Flag

withFlag :: (MonadWriter Compiled m, MonadState OffState m)
         => (Flag a -> m ()) -> m ()
withFlag = with flags _Flag

skim :: (MonadWriter Compiled m, MonadState OffState m)
     => Off () -> m [Text]
skim act = do
  ((), (a,bs,c)) <- runWriterT $ compileM act
  tell (a,[],c)
  return bs

compileM :: (MonadWriter Compiled m, MonadState OffState m) => Off () -> m ()
compileM = interpret return loop . liftProgram
  where
    loop :: (MonadWriter Compiled m, MonadState OffState m)
         => Inst a -> (a -> m b) -> m b
    loop (Puttext t)   k = cmd_ k "puttext" [pretty t]
    loop (Selnum f ts) k = cmd_ k "selnum" (pretty f : map pretty ts)
    loop (Skip d)      k = cmd_ k "skip" [pretty d]
    loop (TextSpeed d) k = cmd_ k "textspeed" [pretty d]
    loop Click         k = cmd_ k "@" []
    loop Newpage       k = cmd_ k "\165" []
    loop NewFlag       k = k =<< allocFlag
    loop (GetFlag f)   k = k (DerefF f)
    loop (SetFlag f v) k = cmd_ k "mov" [pretty f, pretty v]
    loop NewVar        k = k =<< allocVar
    loop (GetVar f)    k = k (DerefV f)
    loop (SetVar f v)  k = cmd_ k "mov" [pretty f, pretty v]
    loop (If p v Nothing)      k = do
      s <- defSubM "if_cond" v
      cmd_ k ("if " <> pretty p <> " gosub " <> pretty s) []
    loop (If p v (Just e))     k = do
      s <- defSubM "if_cond" v
      end <- skim e
      cmd_ k ("if " <> pretty p <> " gosub " <> pretty s <> " : " <> T.intercalate ":" end) []
    loop (NotIf p v)      k = do
      s <- defSubM "if_cond" v
      cmd_ k ("notif " <> pretty p <> " gosub " <> pretty s) []
    loop (Ifte p t f) k = k =<< do
      compileM $ do
        whenFinally p t (skip 2)
        unless_ p f
    loop (Gosub n) k = cmd_ k "gosub" [pretty n]
    loop (Rnd f n) k = cmd_ k "rnd" [pretty f, pretty n]
    loop (Rnd2 f n m) k = cmd_ k "rnd2" [pretty f, pretty n, pretty m]
    loop (Sub f n) k = cmd_ k "sub" [pretty f, pretty n]
    loop (Add f n) k = cmd_ k "add" [pretty f, pretty n]
    loop (Inc f)   k = cmd_ k "inc" [pretty f]
    loop (Dec f)   k = cmd_ k "dec" [pretty f]
    loop (Mul_ f n) k = cmd_ k "mul" [pretty f, pretty n]
    loop (Div_ f n) k = cmd_ k "div" [pretty f, pretty n]
    loop (Mod_ f n) k = cmd_ k "mod" [pretty f, pretty n]
    loop (Cmp f s t) k =
      cmd_ k "cmp" [pretty f, pretty s, pretty t]
    loop (RMode t) k = cmd_ k "rmode" [pretty t]
    loop ResetTimer k = cmd_ k "resettimer" []
    loop (WaitTimer t) k = cmd_ k "waittimer" [pretty t]
    loop (GetTimer t) k = cmd_ k  "gettimer"  [pretty t]
    loop Reset k = cmd_ k "reset" []
    loop DefineReset k = cmd_ k "definereset" []
    loop (Delay n@(Number _)) k = cmd_ k ("d" <> pretty n) []
    loop (Delay d)            k = cmd_ k "delay " [pretty d]
    loop (Wait n@(Number _)) k = cmd_ k ("w" <> pretty n) []
    loop (Wait d)            k = cmd_ k "wait " [pretty d]
    loop LookBackFlush k = cmd_ k "lookbackflush" []
    loop (InputStr f msg len z Nothing) k =
      cmd_ k "inputstr" [pretty f, pretty msg, pretty len, pretty z]
    loop (InputStr f msg len z (Just (x,y,mx,my))) k =
      cmd_ k "inputstr"  [pretty f, pretty msg, pretty len, pretty z
                         ,pretty x, pretty y, pretty mx, pretty my
                         ]
    loop (ClickPos fx fy) k = cmd_ k "clickpos" [pretty fx, pretty fy]
    loop (BtnDef t) k = cmd_ k "btndef" [pretty t]
    loop (Locate x y) k = cmd_ k "locate " [pretty x, pretty y]
    loop (DefSubr t off) k = k =<< defSubM t off
    loop (SelGosub cs) k = cmd_ k "selgosub" $ concat [[pretty t, pretty l] | (t, l) <- cs]

compileNScript :: Off () -> NScript
compileNScript src = formatScript $ snd $ evalRWS (compileM src) () defaultState

formatBlock :: Text -> [Text] -> Text
formatBlock name blk = T.unlines $ ("*" <> name) : blk

formatScript :: Compiled -> NScript
formatScript (defs, body, subs) =
  NScript $
  T.unlines $ ["*define", T.unlines defs, "game", ""
              ,formatBlock "start" (body ++ ["end"])]
              ++ ifoldMap
                   (\s b -> [formatBlock s (b ++ ["return"])])
                   subs

encodeNScript :: NScript -> ByteString
encodeNScript = encodeShiftJIS . T.replace "\n" "\r\n" . runNScript

writeNScript :: FilePath -> NScript -> IO ()
writeNScript fp = BS.writeFile fp . encodeNScript

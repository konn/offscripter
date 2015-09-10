{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ParallelListComp     #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving            #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances            #-}
{-# OPTIONS_GHC -Wall #-}
module Language.ONScripter.Off where
import           Control.Lens
import           Control.Monad
import           Control.Monad.Operational
import           Control.Monad.RWS
import           Control.Monad.Writer      (runWriterT)
import           Data.Char
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IS
import           Data.Maybe
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.Typeable
import           GHC.IO.Encoding
import           System.IO

type Duration = Int

-- | Variables for string values
newtype Var = Var { varId :: Int }
            deriving (Read, Show, Eq, Ord, Typeable)

makePrisms ''Var

-- | Other variables, encoded as integer values
newtype Flag a = Flag { flagId :: Int }
               deriving (Read, Show, Eq, Ord, Typeable)

makePrisms ''Flag

class Printable a where
  pretty :: a -> T.Text

type Subroutine = Text

data Inst a where
  Display :: T.Text -> Inst ()
  TextSpeed   :: Int -> Inst ()
  Click   :: Inst ()
  Newpage :: Inst ()
  NewFlag :: Inst (Flag a)
  GetFlag :: Flaggable a => Flag a -> Inst (Expr a)
  SetFlag :: Flaggable a => Flag a -> Expr a -> Inst ()
  NewVar  :: Inst Var
  GetVar  :: Var -> Inst (Expr T.Text)
  SetVar  :: Var -> Expr T.Text -> Inst ()
  If      :: Expr Bool -> Off () -> Maybe (Off ()) -> Inst ()
  NotIf   :: Expr Bool -> Off () -> Inst ()
  Ifte    :: Expr Bool -> Off () -> Off () -> Inst ()
  Gosub   :: Subroutine -> Inst ()
  Selnum  :: Flag Int -> [Expr Text] -> Inst ()
  Skip    :: Expr Int -> Inst ()
  Seq     :: Inst () -> Inst () -> Inst ()
  Rnd     :: Flag Int -> Expr Int -> Inst ()
  Rnd2    :: Flag Int -> Expr Int -> Expr Int -> Inst ()
  Sub     :: Flag Int -> Expr Int -> Inst ()
  Add     :: Flag Int -> Expr Int -> Inst ()
  Inc     :: Expr Int -> Inst ()
  Dec     :: Expr Int -> Inst ()
  Mul     :: Flag Int -> Expr Int -> Inst ()
  Div     :: Flag Int -> Expr Int -> Inst ()
  Mod     :: Flag Int -> Expr Int -> Inst ()
  Cmp     :: Flag Int -> Expr Text -> Expr Text -> Inst ()
  RMode   :: Flag Bool -> Inst ()
  ResetTimer :: Inst ()
  WaitTimer  :: Expr Int -> Inst ()
  GetTimer   :: Flag Int -> Inst ()
  Reset :: Inst ()
  DefineReset :: Inst ()
  Delay :: Expr Int -> Inst ()
  Wait  :: Expr Int -> Inst ()
  LookBackFlush :: Inst ()
  InputStr :: Var -> Expr Text -> Expr Int -> Flag Bool
           -> Maybe (Expr Int)
           -> Maybe (Expr Int)
           -> Maybe (Expr Int)
           -> Maybe (Expr Int)
           -> Inst ()
  ClickPos :: Flag Int -> Flag Int -> Inst ()
  BtnDef   :: Expr Text -> Inst ()

data Expr a where
  Number  :: Int  -> Expr Int
  String  :: Text     -> Expr Text
  DerefV  :: Var      -> Expr Text
  DerefF  :: Flag a -> Expr a
  RawFlag :: Flaggable a => a -> Expr a
  FCheck  :: FilePath -> Expr Bool
  (:<)    :: Expr Int -> Expr Int -> Expr Bool
  (:>)    :: Expr Int -> Expr Int -> Expr Bool
  (:=)    :: Expr Int -> Expr Int -> Expr Bool
  (:>=)   :: Expr Int -> Expr Int -> Expr Bool
  (:<=)   :: Expr Int -> Expr Int -> Expr Bool
  (:!=)   :: Expr Int -> Expr Int -> Expr Bool

setFlag :: Flaggable a => Flag a -> Expr a -> Off ()
setFlag = (singleton .) . SetFlag

newFlag :: Off (Flag a)
newFlag = singleton NewFlag

deflag :: Flag a -> Flag Int
deflag (Flag a) = Flag a

ifte :: Expr Bool -> Off () -> Off () -> Off ()
ifte p t f = singleton $ Ifte p t f

when_ :: Expr Bool -> Off () -> Off ()
when_ p f = singleton $ If p f Nothing

whenAtEnd :: Expr Bool -> Off () -> Off () -> Off ()
whenAtEnd p t f = do
  singleton $ If p t (Just f)

unless_ :: Expr Bool -> Off () -> Off ()
unless_ = (singleton .) . NotIf

selnum :: Flag Int -> [Expr Text] -> Off ()
selnum = (singleton .) . Selnum

candidates :: Flaggable a => pxy a -> [a]
candidates _ = [minBound .. maxBound]

selflag :: Flaggable a => Flag a -> [(a, Expr Text)] -> Off ()
selflag f cands = selnum (deflag f) dic
  where
    dic = [ fromMaybe "" $ lookup i cands
          | i <- candidates f]

click :: Off ()
click = singleton Click

skip :: Expr Int -> Off ()
skip = singleton . Skip

newpage :: Off ()
newpage = singleton Newpage

type Off  = Program Inst
type OffT = ProgramT Inst

instance (a ~ ()) => IsString (Program Inst a) where
  fromString = singleton . Display . T.pack

someFunc :: IO ()
someFunc = putStrLn "someFunc"

interpret :: Monad m => (a -> m b)
          -> (forall c. instr c -> (c -> m b) -> m b)
          -> ProgramT instr m a
          -> m b
interpret ret bind = loop <=< viewT
  where
    loop (Return a)     = ret a
    loop (instr :>>= k) = bind instr (loop <=< viewT . k)

class    (Eq a, Show a, Enum a, Bounded a) => Flaggable a
instance (Eq a, Show a, Enum a, Bounded a) => Flaggable a

tshow :: Show a => a -> Text
tshow = T.pack . show

instance (a ~ Int) => Num (Expr a) where
  fromInteger = Number . fromInteger
  (+) = error "No arithmetic expression supported for Expr"
  (-) = error "No arithmetic expression supported for Expr"
  (*) = error "No arithmetic expression supported for Expr"
  abs = error "No arithmetic expression supported for Expr"
  signum = error "No arithmetic expression supported for Expr"

instance Printable Var where
  pretty (Var i) = "$" <> tshow i

instance Printable (Flag a) where
  pretty (Flag i) = "%" <> tshow i

instance Printable (Expr a) where
  pretty (Number i)        = tshow i
  pretty (String s)        = "\"" <> escapeStr s <> "\""
  pretty (DerefV v)        = pretty v
  pretty (DerefF f)        = pretty f
  pretty (RawFlag e)       = tshow $ fromEnum e
  pretty (FCheck fp)       = "fchk " <> tshow fp
  pretty (e :< f)          = pretty e <> " < "  <> pretty f
  pretty (e :> f)          = pretty e <> " > "  <> pretty f
  pretty (e := f)          = pretty e <> " == " <> pretty f
  pretty (e :>= f)         = pretty e <> " >= " <> pretty f
  pretty (e :<= f)         = pretty e <> " <= " <> pretty f
  pretty (e :!= f)         = pretty e <> " != " <> pretty f

procQuote :: Text -> Text
procQuote = flip T.snoc '"' . T.cons '\"' . T.drop 1 . T.dropEnd 1

escapeStr :: Text -> Text
escapeStr a = T.map zenkakize a

zenkakize :: Char -> Char
zenkakize c = fromMaybe c $ lookup c dic
  where
    dic = zip ['a'..'z'] ['ａ'..'ｚ']
       ++ zip ['A'..'Z'] ['Ａ'..'Ｚ']
       ++ zip ['0'..'9'] ['０'..'９']
       ++ zip " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
              "　！”＃＄％＆’（）＊＋，ー．／：；＜＝＞？＠［＼］＾＿‘｛｜｝〜"

instance (a ~ Text) => IsString (Expr a) where
  fromString = String . fromString

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

switch :: forall a. Flaggable a => Flag a -> (a -> Off ()) -> Off ()
switch f body = mapM_ singleton conds
  where
    cases = [minBound .. maxBound :: a]
    len   = fromIntegral $ length cases
    conds = [If (DerefF (deflag f) := Number (fromEnum v))
                 (body v) (Just $ when (s > 0) (skip $ Number s))
            | v <- cases
            | s <- [len,len-1..]]

type Compiler = RWS () Compiled OffState
type Compiled = ([T.Text], [T.Text], HashMap T.Text [T.Text])

addDef :: MonadWriter Compiled m => Text -> m ()
addDef  t = tell ([t], [], HM.empty)

cmd :: MonadWriter Compiled m => Text -> m ()
cmd t = tell ([], [t], HM.empty)

sanitize :: Text -> Text
sanitize =
  T.filter (\c -> (c == '_' || isAlphaNum c) && isAscii c)
  . T.dropWhile (not . isAlpha)

defSub :: (MonadWriter Compiled m, MonadState OffState m)
       => Text -> Off () -> m Subroutine
defSub name0 act = do
  let seed = sanitize name0
  Just i <- subroutines . at seed <%= Just . maybe 0 succ
  let name = "sub_" <> seed <> "_" <> tshow i
  bs <- skim act
  tell ([], [], HM.singleton name bs)
  return name

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
    loop (Display t)   k = k =<< cmd (escapeStr t)
    loop (Seq a b)     k = do
      f <- skim $ singleton a
      s <- skim $ singleton b
      cmd $ T.intercalate " : " $ f ++ s
      k ()
    loop (Selnum f ts) k = k =<< cmd ("selnum " <> pretty f <> " "
                                      <> T.intercalate "," (map pretty ts))
    loop (Skip d)      k = k =<< cmd ("skip " <> pretty d)
    loop (TextSpeed d)     k = k =<< cmd ("textspeed " <> tshow d)
    loop Click         k = k =<< cmd "@"
    loop Newpage       k = k =<< cmd "\165"
    loop NewFlag       k = k =<< allocFlag
    loop (GetFlag f)   k = k (DerefF f)
    loop (SetFlag f v) k = k =<< cmd ("mov " <> pretty f <> "," <> pretty v)
    loop NewVar        k = k =<< allocVar
    loop (GetVar f)    k = k (DerefV f)
    loop (SetVar f v)  k = k =<< cmd ("mov " <> pretty f <> "," <> pretty v)
    loop (If p v Nothing)      k = do
      s <- defSub "if_cond" v
      cmd $ "if " <> pretty p <> " gosub *" <> s
      k ()
    loop (If p v (Just e))     k = do
      s <- defSub "if_cond" v
      end <- skim e
      cmd $ "if " <> pretty p <> " gosub *" <> s <> " : " <> T.intercalate ":" end
      k ()
    loop (NotIf p v)      k = do
      s <- defSub "if_cond" v
      cmd $ "notif " <> pretty p <> " gosub *" <> s
      k ()
    loop (Ifte p t f) k = k =<< do
      compileM $ do
        whenAtEnd p t (skip 2)
        unless_ p f
    loop (Gosub n) k = k =<< cmd ("gosub *" <> n)
    loop (Rnd f n) k = k =<< cmd ("rnd " <> pretty f <> "," <> pretty n)
    loop (Rnd2 f n m) k = k =<< cmd ("rnd2 " <> pretty f <> "," <> pretty n
                                             <> "," <> pretty m)
    loop (Sub f n) k = k =<< cmd ("sub " <> pretty f <> "," <> pretty n)
    loop (Add f n) k = k =<< cmd ("add " <> pretty f <> "," <> pretty n)
    loop (Inc f)   k = k =<< cmd ("inc " <> pretty f)
    loop (Dec f)   k = k =<< cmd ("dec " <> pretty f)
    loop (Mul f n) k = k =<< cmd ("mul " <> pretty f <> "," <> pretty n)
    loop (Div f n) k = k =<< cmd ("div " <> pretty f <> "," <> pretty n)
    loop (Mod f n) k = k =<< cmd ("mod " <> pretty f <> "," <> pretty n)
    loop (Cmp t1 t2 t3) k = undefined
    loop (RMode t) k = undefined
    loop ResetTimer k = undefined
    loop (WaitTimer t) k = undefined
    loop (GetTimer t) k = undefined
    loop Reset k = undefined
    loop DefineReset k = undefined
    loop (Delay t) k = undefined
    loop (Wait t) k = undefined
    loop LookBackFlush k = undefined
    loop (InputStr t1 t2 t3 t4 t5 t6 t7 t8) k = undefined
    loop (ClickPos t1 t2) k = undefined
    loop (BtnDef t) k = undefined


compile :: Off () -> Text
compile src = formatScript $ snd $ evalRWS (compileM src) () defaultState

formatBlock :: Subroutine -> [Text] -> Text
formatBlock name blk = T.unlines $ ("*" <> name) : blk

formatScript :: Compiled -> Text
formatScript (defs, body, subs) =
  T.unlines $ ["*define", T.unlines defs, "game", ""
              ,formatBlock "start" (body ++ ["end"])]
              ++ ifoldMap
                   (\s b -> [formatBlock s (b ++ ["return"])])
                   subs

writeScript :: FilePath -> Text -> IO ()
writeScript fp src = withFile fp WriteMode $ \h -> do
  hSetEncoding h =<< mkTextEncoding "Shift_JIS"
  hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
  T.hPutStr h src



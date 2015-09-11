{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs           #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances  #-}
module Language.ONScripter.Off.Types
       (Flaggable,
        Var(..), varId, unsafeVar, _Var,
        Flag(..), flagId, unsafeFlag, _Flag,
        Subroutine(..), label, unsafeSubroutine, _Subroutine,
        Milliseconds,
        Expr(..), Inst(..), Off, OffT, Printable(..)) where
import Language.ONScripter.Off.Utils

import           Control.Lens
import           Control.Monad
import           Control.Monad.Operational
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.IntSet               (IntSet)
import qualified Data.IntSet               as IS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Typeable

class    (Eq a, Show a, Enum a, Bounded a) => Flaggable a
instance (Eq a, Show a, Enum a, Bounded a) => Flaggable a

-- | Variables for string values
newtype Var = Var { _varId :: Int }
            deriving (Read, Show, Eq, Ord, Typeable)

unsafeVar :: Int -> Var
unsafeVar = Var

varId :: Var -> Int
varId = _varId

makePrisms ''Var

-- | Other variables, encoded as integer values
newtype Flag a = Flag { _flagId :: Int }
               deriving (Read, Show, Eq, Ord, Typeable)

flagId :: Flag a -> Int
flagId = _flagId

unsafeFlag :: Int -> Flag a
unsafeFlag = Flag

makePrisms ''Flag

newtype Subroutine = Subroutine { subrId :: Text }
                   deriving (Read, Show, Eq, Ord, Typeable)

makePrisms ''Subroutine

unsafeSubroutine :: Text -> Subroutine
unsafeSubroutine = Subroutine

label :: Subroutine -> Text
label = subrId

instance IsString Subroutine where
  fromString = Subroutine . fromString

type Milliseconds = Int

data Inst a where
  Puttext   :: Expr T.Text -> Inst ()
  TextSpeed :: Expr Int -> Inst ()
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
  SelGosub :: [(Expr Text, Subroutine)] -> Inst ()
  Skip    :: Expr Int -> Inst ()
  Rnd     :: Flag Int -> Expr Int -> Inst ()
  Rnd2    :: Flag Int -> Expr Int -> Expr Int -> Inst ()
  Sub     :: Flag Int -> Expr Int -> Inst ()
  Add     :: Flag Int -> Expr Int -> Inst ()
  Inc     :: Expr Int -> Inst ()
  Dec     :: Expr Int -> Inst ()
  Mul_    :: Flag Int -> Expr Int -> Inst ()
  Div_    :: Flag Int -> Expr Int -> Inst ()
  Mod_     :: Flag Int -> Expr Int -> Inst ()
  Cmp     :: Flag Int -> Expr Text -> Expr Text -> Inst ()
  RMode   :: Flag Bool -> Inst ()
  ResetTimer :: Inst ()
  WaitTimer  :: Expr Milliseconds -> Inst ()
  GetTimer   :: Flag Milliseconds -> Inst ()
  Reset :: Inst ()
  DefineReset :: Inst ()
  Delay :: Expr Milliseconds -> Inst ()
  Wait  :: Expr Milliseconds -> Inst ()
  LookBackFlush :: Inst ()
  InputStr :: Var -> Expr Text -> Expr Int -> Expr Bool
           -> Maybe (Expr Int, Expr Int, Expr Int, Expr Int)
           -> Inst ()
  ClickPos :: Flag Int -> Flag Int -> Inst ()
  BtnDef   :: Expr Text -> Inst ()
  Locate   :: Expr Int -> Expr Int -> Inst ()
  DefSubr  :: Text -> Off () -> Inst Subroutine


instance (a ~ ()) => IsString (Program Inst a) where
  fromString = singleton . Puttext . String . T.pack

type Off  = Program Inst
type OffT = ProgramT Inst

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

instance (a ~ Int) => Num (Expr a) where
  fromInteger = Number . fromInteger
  (+) = error "No arithmetic expression supported for Expr"
  (-) = error "No arithmetic expression supported for Expr"
  (*) = error "No arithmetic expression supported for Expr"
  abs = error "No arithmetic expression supported for Expr"
  signum = error "No arithmetic expression supported for Expr"

instance (a ~ Text) => IsString (Expr a) where
  fromString = String . fromString

class Printable a where
  pretty :: a -> T.Text

instance Printable Var where
  pretty (Var i) = "$" <> tshow i

instance Printable (Flag a) where
  pretty (Flag i) = "%" <> tshow i

instance Printable Subroutine where
  pretty (Subroutine s) = "*" <> s

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


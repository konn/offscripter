{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GADTs  #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ParallelListComp      #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Language.ONScripter.Off.Syntax
       (Flaggable,
        Var, varId, unsafeVar,
        Flag, flagId, unsafeFlag, Milliseconds,
        Subroutine, label, unsafeSubroutine,
        Off, OffT, Expr(..), Printable(..),
        -- * NScript commands
        module Language.ONScripter.Off.Syntax.Combinators,
        setFlag, newFlag, decastFlag, when_,
        whenFinally, unless_, candidates, seldo,
        puts, selflag, defSubr, interpret, switch,
        -- * Types to indicate type-level errors
        OtherType, UnexpectedType
       ) where
import Language.ONScripter.Off.Syntax.Combinators
import Language.ONScripter.Off.Types

import Control.Monad
import Control.Monad.Operational
import Data.Monoid
import Data.Text                 (Text)

setFlag :: Flaggable a => Flag a -> Expr a -> Off ()
setFlag = setflag

newFlag :: Off (Flag a)
newFlag = singleton NewFlag

decastFlag :: Flag a -> Flag Int
decastFlag (Flag a) = Flag a

when_ :: Expr Bool -> Off () -> Off ()
when_ p f = if_ p f Nothing

whenFinally :: Expr Bool -> Off () -> Off () -> Off ()
whenFinally p t f = if_ p t (Just f)

unless_ :: Expr Bool -> Off () -> Off ()
unless_ = notif

candidates :: Flaggable a => pxy a -> [a]
candidates _ = [minBound .. maxBound]

seldo :: [(Expr Text, Off ())] -> Off ()
seldo cs =  selgosub =<< forM cs (\(a, b) -> (,) a <$> defSubr ("seldo_" <> pretty a) b)

puts :: Expr Text -> Off ()
puts = puttext

selflag :: Flaggable a => Flag a -> [(a, Expr Text)] -> Off ()
selflag f opts = seldo $ map (\ (v, t) -> (t, setFlag f $ RawFlag v)) opts

defSubr :: Text -> Off () -> Off Subroutine
defSubr = (singleton .) . DefSubr

interpret :: Monad m => (a -> m b)
          -> (forall c. instr c -> (c -> m b) -> m b)
          -> ProgramT instr m a
          -> m b
interpret ret bind = loop <=< viewT
  where
    loop (Return a)     = ret a
    loop (instr :>>= k) = bind instr (loop <=< viewT . k)

switch :: Flaggable a => Flag a -> (a -> Off ()) -> Off ()
switch f body = mapM_ singleton conds
  where
    cases = candidates Nothing
    len   = fromIntegral $ length cases
    conds = [If (DerefF (decastFlag f) := RawFlag (fromEnum v))
                 (body v) (Just $ when (s > 0) (skip $ RawFlag s))
            | v <- cases
            | s <- [len,len-1..]]

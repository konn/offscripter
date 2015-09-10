{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, GADTs      #-}
{-# LANGUAGE LiberalTypeSynonyms, NoImplicitPrelude, PolyKinds, RankNTypes #-}
{-# LANGUAGE TypeFamilies                                                  #-}
module Language.ONScripter.Off.Prelude (module Language.ONScripter.Off, P.fromInteger, (.<), (.>), (.==), (.>=), (.<=), (./=), module Prelude) where
import           Data.String
import qualified Data.Text               as T
import           Language.ONScripter.Off
import           Prelude                 hiding (Eq (..), Num (..), Ord (..))
import qualified Prelude                 as P

(.<) = (:<)
(.>) = (:>)
(.==) = (:=)
(.>=) = (:>=)
(.<=) = (:<=)
(./=) = (:!=)

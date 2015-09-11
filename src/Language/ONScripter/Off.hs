{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ParallelListComp     #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, StandaloneDeriving            #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances            #-}
{-# OPTIONS_GHC -Wall #-}
module Language.ONScripter.Off
       ( module Language.ONScripter.Off.Syntax
       , module Language.ONScripter.Off.Compiler.NScript
       ) where

import Language.ONScripter.Off.Compiler.NScript
import Language.ONScripter.Off.Syntax

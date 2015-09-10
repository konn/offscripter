{-# LANGUAGE GADTs, LambdaCase, RecordWildCards #-}
module Main where
import           Data.String
import           Language.ONScripter.Off
import qualified Language.ONScripter.Off.Prelude as OPQ
import           Prelude

data Option = Yes | No | Amb
            deriving (Eq, Read, Show, Enum, Bounded)


a :: Off ()
a = do
  n <- newFlag
  selflag n [(Yes, "うん"), (No, "ううん")]
  switch n $ \case
    Yes -> "やったぜ。" >> newpage
    Amb -> "やったか？" >> newpage
    No  -> "やってナイ"
  click
  "これは私の物語。"
  click
  "うっそー！"
  newpage
  "ちゃんちゃん"
  click

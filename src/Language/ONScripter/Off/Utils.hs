{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.ONScripter.Off.Utils where
import           Control.Exception
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.ICU.Convert
import           System.IO.Unsafe

tshow :: Show a => a -> Text
tshow = T.pack . show

procQuote :: Text -> Text
procQuote = flip T.snoc '"' . T.cons '\"' . T.drop 1 . T.dropEnd 1

escapeStr :: Text -> Text
escapeStr a = {- T.map zenkakize -} a

zenkakize :: Char -> Char
zenkakize c = fromMaybe c $ lookup c dic
  where
    dic = zip ['a'..'z'] ['ａ'..'ｚ']
       ++ zip ['A'..'Z'] ['Ａ'..'Ｚ']
       ++ zip ['0'..'9'] ['０'..'９']
       ++ zip " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
              "　！”＃＄％＆’（）＊＋，ー．／：；＜＝＞？＠［＼］＾＿‘｛｜｝〜"

shiftJIS :: Converter
shiftJIS = unsafePerformIO $ open "Shift_JIS" (Just True)
{-# NOINLINE shiftJIS #-}

encodeShiftJIS :: Text -> ByteString
encodeShiftJIS = fromUnicode shiftJIS

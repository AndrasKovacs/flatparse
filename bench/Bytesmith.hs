{-# language RankNTypes #-}

module Bytesmith where

-- module Bytesmith (runSexp, runLongws, runNumcsv, strToByteArray) where

-- import Control.Applicative

-- import GHC.Exts

-- import qualified Data.Bytes.Parser       as P
-- import qualified Data.Bytes.Parser.Ascii as A

-- import Data.Primitive.ByteArray
-- import Data.Char

-- strToByteArray :: String -> ByteArray
-- strToByteArray = fromList . map (fromIntegral . ord)

-- byteArrayToStr :: ByteArray -> String
-- byteArrayToStr = map (chr . fromIntegral) . toList

-- parseByteArray :: (forall s. Parser s) -> ByteArray -> Bool
-- parseByteArray p b = case P.parseByteArray p b of
--   P.Failure{} -> False
--   _           -> True
-- {-# inline parseByteArray #-}

-- data U = U
-- instance Semigroup U where (<>) _ _ = U
-- instance Monoid U    where mempty   = U

-- type Parser s = P.Parser U s ()

-- many_ :: Parser s -> Parser s
-- many_ p = go where
--   go = (p >> go) <|> pure ()
-- {-# inline many_ #-}

-- some_ :: Parser s -> Parser s
-- some_ p = p >> many_ p
-- {-# inline some_ #-}

-- ws :: Parser s
-- ws = many_ do
--   P.any U >>= \case
--     32 -> pure ()      -- ' '
--     10 -> pure ()      -- '\n'
--     _  -> P.fail U

-- open    = A.char U '(' >> ws
-- close   = A.char U ')' >> ws
-- ident   = A.skipAlpha1 U >> ws
-- sexp    = (open >> some_ sexp >> close) <|> ident
-- src     = sexp >> P.endOfInput U
-- runSexp = parseByteArray src

-- longw     = P.cstring U (Ptr "thisisalongkeyword\NUL"#)
-- longws    = some_ (longw >> ws) >> P.endOfInput U
-- runLongws = parseByteArray longws

-- numeral   = A.skipDigits1 U >> ws
-- comma     = A.char U ',' >> ws
-- numcsv    = numeral >> many_ (comma >> numeral) >> P.endOfInput U
-- runNumcsv = parseByteArray numcsv

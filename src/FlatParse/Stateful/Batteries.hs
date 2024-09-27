{-# language Strict, ViewPatterns, ScopedTypeVariables #-}

module FlatParse.Stateful.Batteries where

import Control.Monad
import qualified Data.ByteString as B
import qualified FlatParse.Stateful as FP
import qualified FlatParse.Stateful.Switch as FP
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Parser = FP.Parser Int Error

data Expected
  = Lit String
  | Msg String
  | ExactIndent Int
  | IndentMore Int
  deriving (Eq, Show, Ord)

data Error = Error FP.Pos [Expected] | DontUnbox
  deriving Show

errorPos :: Error -> FP.Pos
errorPos = \case
  Error p _ -> p
  _         -> undefined

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
mergeErrors :: Error -> Error -> Error
mergeErrors e@(Error p es) e'@(Error p' es')
  | p < p'     = e'
  | p > p'     = e
  | [_] <- es  = e
  | [_] <- es' = e'
  | otherwise  = Error p (es ++ es')
mergeErrors _ _ = undefined
{-# noinline mergeErrors #-} -- cold code

-- | Pretty print an error. The `B.ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: B.ByteString -> Error -> String
prettyError b (Error pos es) =

  let ls     = FP.linesUtf8 b
      (l, c) = head $ FP.posLineCols b [pos]
      line   = if l < length ls then ls !! l else ""
      linum  = show l
      lpad   = map (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s
      expected (ExactIndent col) = "expected a token indented to column " ++ show (col + 1)
      expected (IndentMore col) = "expected a token indented to column " ++ show (col + 1) ++ " or more."

      expecteds :: [Expected] -> String
      expecteds []     = error "impossible"
      expecteds [e]    = expected e
      expecteds (e:es) = expected e ++ go es where
        go []     = ""
        go [e]    = " or " ++ expected e
        go (e:es) = ", " ++ expected e ++ go es

  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: expected " ++ expecteds (S.toList $ S.fromList es)
prettyError _ _ = undefined

{-
Batteries included for potentially indentation-sensitive parsing

token parsing scheme:
  1 check indentation
  2 read thing
  3 read ws

input
  - parser for first ident character
  - parser for rest ident character
  - parser for whitespace
  - parser for line comment, multiline open, multiline close, whether multiline is nestable
  - list of keywords

th-generated output
  - ident parser, handles keywords, indentation, returns span
    + strict version
  - symbol parser, handles keywords, idents, indentation, return span
    + strict version
  - case split:
      - handles keywords, ident overlaps, indentation, returns span

non-th exports
  - all the useful stuff
  - error types, printing, handling

-}

--------------------------------------------------------------------------------

getPos :: Parser FP.Pos
getPos = FP.getPos
{-# inline getPos #-}

err :: [Expected] -> Parser a
err es = do
  p <- FP.getPos
  FP.err (Error p es)
{-# inline err #-}

cut :: Parser a -> [Expected] -> Parser a
cut p exps = do
  pos <- getPos
  FP.cutting p (Error pos exps) mergeErrors
{-# inline cut #-}

runParser :: Parser a -> B.ByteString -> FP.Result Error a
runParser p src = FP.runParser p 0 0 src

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser a -> String -> IO ()
testParser p (FP.strToUtf8 -> str) = case runParser p str of
    FP.Err e    -> putStrLn $ prettyError str e
    FP.OK a _ _ -> print a
    FP.Fail     -> putStrLn "parse error"

-- | Query the current indentation level, fail if it's smaller than the current expected level.
lvl :: Parser Int
lvl = do
  lvl <- FP.ask
  currentLvl <- FP.get
  if currentLvl < lvl
    then FP.empty
    else pure currentLvl
{-# inline lvl #-}

-- | Same as `lvl` except we throw an error on mismatch.
lvl' :: Parser Int
lvl' = do
  lvl <- FP.ask
  currentLvl <- FP.get
  if currentLvl < lvl
    then err [IndentMore lvl]
    else pure currentLvl
{-# inline lvl' #-}

-- | Fail if the current level is not the expected one.
exactLvl :: Int -> Parser ()
exactLvl l = do
  l' <- FP.get
  if l == l' then pure () else FP.empty
{-# inline exactLvl #-}

-- | Throw error if the current level is not the expected one.
exactLvl' :: Int -> Parser ()
exactLvl' l = do
  l' <- FP.get
  if l == l' then pure () else err [ExactIndent l]
{-# inline exactLvl' #-}

moreIndented :: Parser a -> (a -> Parser b) -> Parser b
moreIndented pa k = do
  lvl <- FP.get
  a <- pa
  FP.local (\_ -> lvl + 1) (k a)
{-# inline moreIndented #-}

-- | Run a parser with expected indentation level.
localIndentation :: Int -> Parser a -> Parser a
localIndentation n p = FP.local (\_ -> n) p
{-# inline localIndentation #-}

--------------------------------------------------------------------------------

data Config = Config
  (CodeQ (Parser Char))  -- ident start char (Parser Char)
  (CodeQ (Parser Char))  -- ident nonstart char (Parser Char)
  String   -- set of whitespace characters, excluding newline
  String   -- line comment start
  String   -- block comment start
  String   -- block comment end
  [String] -- keywords

-- Working around nested quote limitations
--------------------------------------------------------------------------------

symBody1 :: String -> Q Exp
symBody1 s = [| spanOfToken $(FP.string s) `notFollowedBy` identChar |]

symBody2 :: String -> Q Exp
symBody2 s = [| spanOfToken $(FP.string s) |]

switchBody :: (String -> Bool) -> ([(String, Exp)], Maybe Exp) -> Q Exp
switchBody isIdent (cases, deflt) =
      [| do lvl
            left <- FP.getPos
            $(FP.switch (makeRawSwitch
                (map (\(s, body) -> (s, if isIdent s
                     then [| do {FP.fails identChar; right <- FP.getPos; ws; $(pure body) (FP.Span left right)} |]
                     else [| do {right <- FP.getPos; ws ; $(pure body) (FP.Span left right)} |]))
                     cases)
                ((\deflt -> [| do {right <- FP.getPos; ws ; $(pure deflt) (FP.Span left right)} |]) <$> deflt)))
        |]

--------------------------------------------------------------------------------

chargeBatteries :: Config -> DecsQ
chargeBatteries (Config identStart identRest wsChars lineComment
                        blockCommentStart blockCommentEnd keywords) = do

  let lineCommentLen       = length lineComment
      blockCommentStartLen = length blockCommentStart
      blockCommentEndLen   = length blockCommentEnd

  [d|
    goLineComment :: Parser ()
    goLineComment =
      FP.withOption FP.anyWord8
        (\case 10 -> FP.put 0 >> ws
               _  -> FP.modify (+1) >> goLineComment)
        (pure ())

    blockComment :: Parser ()
    blockComment = go (1 :: Int) where
      go 0 = ws
      go n = $(FP.switch $ makeRawSwitch [
          ("\n"              , [| FP.put 0 >> go n |])
        , (blockCommentStart , [| FP.modify (+ $(lift blockCommentStartLen)) >> go (n - 1) |])
        , (blockCommentEnd   , [| FP.modify (+ $(lift blockCommentEndLen)) >> go (n + 1) |]) ]
       (Just [| FP.branch FP.anyChar (FP.modify (+1) >> go n) (pure ()) |]))

    ws :: Parser ()
    ws = $(FP.switch $ makeRawSwitch
      (
        ("\n", [| FP.put 0 >> ws |])
      : (blockCommentStart, [| FP.modify (+ $(lift blockCommentStartLen)) >> blockComment |])
      : (lineComment, [| goLineComment |])
      : map (\c -> ([c], [| FP.modify (+1) >> ws |])) wsChars
      )
      (Just [| pure () |]))

    token :: Parser a -> Parser a
    token p = FlatParse.Stateful.Batteries.lvl *> p <* ws
    {-# inline token #-}

    token' :: Parser a -> Parser a
    token' p = FlatParse.Stateful.Batteries.lvl' *> p <* ws
    {-# inline token' #-}

    spanOfToken :: Parser a -> Parser FP.Span
    spanOfToken p = lvl *> FP.spanOf p <* ws
    {-# inline spanOfToken #-}

    spanOfToken' :: Parser a -> Parser FP.Span
    spanOfToken' p = lvl' *> FP.spanOf p <* ws
    {-# inline spanOfToken' #-}

    identStartChar :: Parser Char
    identStartChar = $(unTypeCode identStart)

    identChar :: Parser Char
    identChar = $(unTypeCode identRest)

    inlineIdentChar :: Parser Char
    inlineIdentChar = $(unTypeCode identRest)
    {-# inline inlineIdentChar #-}

    scanIdent :: Parser ()
    scanIdent = identStartChar >> FP.skipMany inlineIdentChar

    anyKeyword :: Parser ()
    anyKeyword = $(FP.switch $
      makeRawSwitch
        (map (\s -> (s, [| FP.eof |])) keywords)
        Nothing)

    identBase :: Parser FP.Span
    identBase = FP.withSpan scanIdent \_ span -> do
      FP.fails $ FP.inSpan span anyKeyword
      ws
      pure span

    -- | Parse an identifier.
    ident :: Parser FP.Span
    ident = lvl >> identBase
    {-# inline ident #-}

    -- | Parse an identifier.
    ident' :: Parser FP.Span
    ident' = lvl' >> identBase `cut` [Lit "identifier"]
    {-# inline ident' #-}

    isIdent :: String -> Bool
    isIdent = undefined

    sym :: String -> Q Exp
    sym s | isIdent s = symBody1 s
          | otherwise = symBody2 s

    switch :: Q Exp -> Q Exp
    switch cases = switchBody isIdent =<< FP.parseSwitch cases
    |]

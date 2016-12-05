module Language.Lexer.Lexer where
  import qualified Language.Lexer.Syntax as S
  import Data.String.Utils

  type MatcherResult = Maybe S.Token
  type Matcher = String -> Int -> MatcherResult

  fixedStringMatcher :: String -> (S.SourceLocation -> S.Token) -> Matcher
  fixedStringMatcher token constructor code pos =
    if startswith token code then
      Just (constructor $ S.SourceLocation pos (length token)) else Nothing

  matchers :: [Matcher]
  matchers = [
    -- Lua default keywords.
    fixedStringMatcher "do" (S.Token S.Do),
    fixedStringMatcher "end" (S.Token S.End),
    fixedStringMatcher "while" (S.Token S.While),
    fixedStringMatcher "repeat" (S.Token S.Repeat),
    fixedStringMatcher "until" (S.Token S.Until) ]

  maxSuccess :: [MatcherResult] -> MatcherResult
  maxSuccess [] = Nothing
  maxSuccess (head:tail) = case (head, maxSuccess tail) of
                            (Just a, Just b) -> case compare (S.getTokenLength a) (S.getTokenLength b) of
                                EQ -> Just a
                                GT -> Just a
                                LT -> Just b
                            (Just a, Nothing) -> Just a
                            (Nothing, Just b) -> Just b
                            (Nothing, Nothing) -> Nothing

  type LexingError = Int
  type LexingResult = Either [S.Token] ([S.Token], LexingError)

  skipToken :: S.Token -> Bool
  skipToken (S.Token S.Skip _) = True
  skipToken _ = False

  lex :: String -> LexingResult
  lex code = lex' code 0
    where lex' :: String -> Int -> LexingResult
          lex' [] pos = Left [S.Token S.EOF (S.SourceLocation pos 0)]
          lex' code pos = let results = [ m code pos | m <- matchers ] in
                          case maxSuccess results of
                            Nothing -> Right ([], pos)
                            Just (S.Token S.Skip (S.SourceLocation _ length)) -> lex' (drop length code) (pos + length)
                            Just token -> case lex' (drop (S.getTokenLength token) code) (pos + S.getTokenLength token) of
                              Left rest -> Left (token:rest)
                              Right (rest, error) -> Right (token:rest, error)

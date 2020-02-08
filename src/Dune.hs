module Dune
  ( compile
  , parseCycles
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Set                      as S
import           Data.Maybe                     ( catMaybes )

import           System.Process
import           System.Exit                    ( ExitCode(..) )

compile :: FilePath -> IO (Maybe String)
compile dir = do
  (exitCode, _stdout, duneError) <- readProcessWithExitCode
    "dune"
    ["build", "--root", dir]
    ""
  return $ case exitCode of
    ExitSuccess   -> Nothing
    ExitFailure _ -> Just duneError

parseFileName :: Text -> Maybe Text
parseFileName line = do
  firstToken <- tokenBegin line
  tokenEnd firstToken
 where
  tokenBegin t = case T.splitOn "__" t of
    [_, token] -> Just token
    _          -> Nothing
  tokenEnd t = case T.splitOn ".impl" t of
    [token, _] -> Just token
    _          -> Nothing

parseCycles :: String -> [Text]
parseCycles duneOutput = do
  let duneLines = T.lines $ T.pack duneOutput
  S.toList $ S.fromList . catMaybes $ parseFileName <$> duneLines

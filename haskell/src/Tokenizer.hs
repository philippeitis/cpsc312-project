module Tokenizer
 (
    tokenize
 ) where

import System.Process (createProcess, CreateProcess(std_out), StdStream(CreatePipe), proc, ProcessHandle)
import GHC.IO.Handle ( Handle, hGetContents )

data Token = Token String String deriving (Show)

readTokens :: String -> Maybe [Token]
readTokens = readTokenPairs . lines

readTokenPairs :: [String] -> Maybe [Token]
readTokenPairs [] = Just []
readTokenPairs [x] = Nothing
readTokenPairs (text:tag:rest) = readTokenPairs rest
    >>= (\items -> Just (Token tag text : items))

readOutput :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO (Maybe [Token])
readOutput (_, Just stdout, _, _) = hGetContents stdout
    >>= \x -> return (readTokens x)
readOutput _ = return Nothing

-- >>> tokenize "Hello"
-- Just []
tokenize :: String -> IO (Maybe [Token])
tokenize sentence = createProcess (proc "python" ["tokenizer.py", sentence]){ std_out = CreatePipe }
    >>= readOutput

main :: IO ()
main = tokenize "Hello world" >>= print

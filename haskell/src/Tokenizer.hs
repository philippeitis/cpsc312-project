module Tokenizer
 (
    tokenize,
    setupTokenizer
 ) where

import Control.Monad (void)
import GHC.IO.Handle (Handle, hGetContents)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Info
import System.Process (CreateProcess (std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc,
                       waitForProcess)
import Tokens (Token, newToken)

readTokens :: String -> Maybe [Token]
readTokens = readTokenPairs . lines

-- |Reads token pairs from a list with pairs of Text / Tag.
readTokenPairs :: [String] -> Maybe [Token]
readTokenPairs [] = Just []
readTokenPairs [x] = Nothing
readTokenPairs (text:tag:rest) = readTokenPairs rest
    >>= \items -> newToken tag text >>= \token -> Just (token : items)

-- |Reads the stdout handle from createProcess.
readOutput :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO (Maybe [Token])
readOutput (_, Just stdout, _, _) = hGetContents stdout
    >>= \x -> return (readTokens x)
readOutput _ = return Nothing

-- |Calls out to Python to tokenize using spaCy
tokenize :: String -> IO (Maybe [Token])
tokenize sentence = createProcess (proc python ["./src/tokenizer.py", sentence]){ std_out = CreatePipe }
    >>= readOutput

-- |Sets default Python path based on OS
python = case os of
    "linux" -> "./venv/bin/python"
    "darwin" -> "./venv/bin/python"
    "mingw32" -> "./venv/scripts/python.exe"

-- |Utility function to wait for the process created by createProcess to terminate
waitForProcessWrapper :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
waitForProcessWrapper (_, _, _, handle) = void (waitForProcess handle)

-- |Installs spacy into the local venv
installSpacy :: IO ()
installSpacy = createProcess (proc python ["-m", "pip", "install", "-U", "pip", "setuptools", "wheel"])
    >>= waitForProcessWrapper
    >> createProcess (proc python ["-m", "pip", "install", "-U", "spacy"])
    >>= waitForProcessWrapper
    >> createProcess (proc python ["-m", "spacy", "download", "en_core_web_sm"])
    >>= waitForProcessWrapper

-- |Sets up the tokenizer - if no local venv directory exists, a venv will be created and spacy will be installed.
-- Otherwise, nothing happens
setupTokenizer :: IO ()
setupTokenizer = doesDirectoryExist "venv" >>= \isDir -> if isDir then
    putStrLn "Tokenizer already installed"
    else putStrLn "Installing tokenizer"
        >> createProcess (proc "python" ["-m", "venv", "./venv"])
        >>= waitForProcessWrapper
        >> putStrLn "Created venv"
        >> installSpacy
        >> putStrLn "Installed spacy"

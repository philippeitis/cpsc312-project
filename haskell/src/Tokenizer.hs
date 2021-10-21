module Tokenizer
 (
    tokenize,
    setupTokenizer
 ) where

import System.Process (createProcess, CreateProcess(std_out), StdStream(CreatePipe), proc, ProcessHandle, waitForProcess)
import System.Directory (doesDirectoryExist, doesFileExist)
import GHC.IO.Handle ( Handle, hGetContents )
import System.Info
import Distribution.System (OS(Windows, Linux, OSX), buildOS)

data Token = Token String String deriving (Show)

python = "./venv/bin/python"

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

tokenize :: String -> IO (Maybe [Token])
tokenize sentence = createProcess (proc python ["./src/tokenizer.py", sentence]){ std_out = CreatePipe }
    >>= readOutput

installSpacy :: IO ()
installSpacy = createProcess (proc python ["-m", "pip", "install", "-U", "pip", "setuptools", "wheel"])
    >>= waitForProcessWrapper
    >> createProcess (proc python ["-m", "pip", "install", "-U", "spacy"])
    >>= waitForProcessWrapper
    >> createProcess (proc python ["-m", "spacy", "download", "en_core_web_sm"])
    >>= waitForProcessWrapper

waitForProcessWrapper :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
waitForProcessWrapper (_, _, _, handle) = waitForProcess handle >> return ()

setupTokenizer :: IO ()
setupTokenizer = doesDirectoryExist "venv" >>= \isDir -> if isDir then
    putStrLn "Tokenizer already installed"
    else putStrLn "Installing tokenizer"
        >> createProcess (proc "python" ["-m", "venv", "./venv"])
        >>= waitForProcessWrapper
        >> putStrLn "Created venv"
        >> installSpacy
        >> putStrLn "Installed spacy"

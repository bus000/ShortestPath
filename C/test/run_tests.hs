#!/usr/bin/env runhaskell

import System.IO
import System.Directory
import System.Process
import Text.Show

data TestEnv = TestEnv FilePath FilePath deriving (Eq, Show)

data Test = Test TestEnv Bool deriving (Eq, Show)

testBin :: String
testBin = "./test_bin/"

outPost :: String
outPost = "_correct.txt"

getTestEnv :: [FilePath] -> [TestEnv]
getTestEnv [] = []
getTestEnv (file:files) =
    (TestEnv (testBin ++ file) (file ++ outPost)) : (getTestEnv files)

runTest :: [TestEnv] -> [IO Bool]
runTest [] = []
runTest (test:tests) = (runTest' test) : (runTest tests)
  where
    runProg :: FilePath -> IO String
    runProg exec = do
        (_, Just handleOut, _, _) <- createProcess (proc exec [])
            { std_out = CreatePipe }
        output <- hGetContents handleOut
        return output

    {-runTest' :: TestEnv -> Either String IO Bool-}
    runTest' (TestEnv exec correct) = do
        let output = runProg exec
        outString <- output
        {-fileExists <- doesFileExist correct-}
        expected <- readFile correct
        (return $ outString == expected)

resultString :: [Test] -> String
resultString tests =
    let filenames = map (\(Test (TestEnv cFile outfile) result) -> cFile) tests
        maxStrLen = maximum (map length filenames)
    in resultString' tests (maxStrLen + 5)
  where
    resultString' [] _ = ""
    resultString' ((Test (TestEnv cFile outFile) result):tests) len =
        let filename = cFile ++ (repeat ' ')
        in take len filename ++ (show result) ++ "\n" ++
            (resultString tests)

main ::IO ()
main = do
    putStrLn "Testing"
    binContents <- getDirectoryContents testBin

    let cFiles = filter (\x -> x /= "." && x /= "..") binContents
        testEnv = getTestEnv cFiles

    testResult <- sequence $ runTest testEnv

    let tests = map (\(env, res) -> Test env res) (zip testEnv testResult)

    putStr $ resultString tests

#!/usr/bin/env runhaskell

import System.IO
import System.Directory
import System.Process
import Data.List

data TestEnv = TestEnv FilePath FilePath deriving (Eq, Show)

data Test = TestSuccess FilePath String String
          | TestError FilePath String
    deriving (Eq, Show)

testBin :: String
testBin = "./test_bin/"

outPost :: String
outPost = "_correct.txt"

getTestEnv :: [FilePath] -> [TestEnv]
getTestEnv [] = []
getTestEnv (file:files) =
    (TestEnv (testBin ++ file) (file ++ outPost)) : (getTestEnv files)

getTests :: [TestEnv] -> IO [Test]
getTests [] = return []
getTests (env:envs) = do
    test <- getTest env
    tests <- getTests envs
    return (test:tests)

  where
    getTest :: TestEnv -> IO (Test)
    getTest (TestEnv exec output) = do
        execExist <- doesFileExist exec
        outputExist <- doesFileExist output

        case (execExist, outputExist) of
            (False, False) ->
                return $ TestError exec (errFindFiles [exec, output])
            (False, True) ->
                return $ TestError exec (errFindFiles [exec])
            (True, False) ->
                return $ TestError exec (errFindFiles [output])
            (True, True) ->
                runTest $ TestEnv exec output

    runTest :: TestEnv -> IO (Test)
    runTest (TestEnv exec output) = do
        process <- createProcess (proc exec []) { std_out = CreatePipe }
        case process of
            (_, Just handleOut, _, _) -> do
                execout <- hGetContents handleOut
                expected <- readFile output
                return $ (TestSuccess exec execout expected)

            (_, Nothing, _, _) ->
                return $ TestError exec errOpenHandle

    errFindFiles :: [FilePath] -> String
    errFindFiles files =
        if length files == 1
            then "Error: could not find file " ++ (head files)
            else "Error: could not find files " ++ (intercalate " " files)

    errOpenHandle :: String
    errOpenHandle = "Error: could not open handle"

showTests :: [Test] -> String
showTests tests =
    let filenames = map (\x -> case x of
            (TestSuccess name _ _) -> name
            (TestError name _) -> name) tests
        maxStrLen = maximum (map length filenames)
    in showTests' tests (maxStrLen + 5)

  where
    showTests' :: [Test] -> Int -> String
    showTests' [] _ = ""
    showTests' ((TestSuccess program programOut expected):tests) len =
        let program' = take len (program ++ (repeat ' '))
        in if programOut == expected
            then program' ++ "OK\n" ++ (showTests' tests len)
            else program' ++ "ERROR\n" ++ (showTests' tests len)
    showTests' ((TestError program errorMsg):tests) len =
        let program' = take len (program ++ (repeat ' '))
        in program' ++ errorMsg ++ "\n" ++ (showTests' tests len)

main = do
    putStrLn "TESTING"
    binContents <- getDirectoryContents testBin

    let execRigths = map (\x -> do
        permissions <- getPermissions (testBin ++ x)
        return $ executable permissions) binContents

    executable <- sequence execRigths

    let cFiles' = filter (\x -> x /= "." && x /= "..") binContents
        cFiles = map fst $ filter (\(cFile, exec) -> exec) $ zip cFiles'
            executable
        testEnv = getTestEnv cFiles

    testResult <- getTests testEnv

    putStr $ showTests testResult

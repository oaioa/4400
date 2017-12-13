module BashCommand
  (cloneGitRepo, totalCommits, resetMaster, resetToPrevCommit, computeComplexity) where

import           Control.Monad
import           System.Process
import           System.Console.ANSI

redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
resetCode = setSGRCode [Reset]

cloneGitRepo :: String -> Int -> IO String
cloneGitRepo repo worker = do
  readProcess "rm" ["-rf", "clones/user"++(show worker)] ""
  result <- readProcess "git" ["clone", repo, "clones/user"++(show worker)] ""
  return result

totalCommits :: Int -> IO Integer
totalCommits  worker = do
  result <- readProcess "git" ["-C","clones/user"++(show worker),"rev-list", "--count", "master"] ""
  let total = read result
  return total

resetMaster :: Int -> IO String
resetMaster  worker = do
  result <- readProcess "git" ["-C","clones/user"++(show worker),"checkout", "master"] ""
  readProcess "git" ["-C","clones/user"++(show worker),"pull"] ""
  return result

resetToPrevCommit :: Integer -> Int -> IO Int
resetToPrevCommit num  worker = do
  if (num==1)
        then do 
                result <- readProcess "git" ["-C","clones/user"++(show worker),"reset", "--hard", "HEAD"] ""
                return 1
        else do 
                (exitcode, stdout, stderr)<- readProcessWithExitCode "git" ["-C","clones/user"++(show worker),"reset", "--hard", ("HEAD~" ++ (show (num-1)))] ""
                --putStrLn $ "\nResult of reset :"++exitcode
                if (stdout /= []) 
                    then return 1
                    else do
                        putStrLn $ "\nERROR in going back to previous commit :"++stdout
                        return 0

computeComplexity :: Int -> IO Integer
computeComplexity  worker = do
  r <- readProcess "./argonScript.sh" ["clones/user"++(show worker)] ""
  putStrLn $ "\n"++redCode++"RESULT for worker "++(show worker)++"  "++r++resetCode
  let complexity = read r
  return complexity

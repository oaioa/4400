module BashCommand
  (cloneGitRepo, totalCommits, resetMaster, resetToPrevCommit, computeComplexity) where

import           Control.Monad
import           System.Process


cloneGitRepo :: String -> Int -> IO String
cloneGitRepo repo worker = do
  readProcess "rm" ["-rf", "remote"++(show worker)] ""
  result <- readProcess "git" ["clone", repo, "remote"++(show worker)] ""
  return result

totalCommits :: Int -> IO Integer
totalCommits  worker = do
  result <- readProcess "git" ["-C","remote"++(show worker),"rev-list", "--count", "master"] ""
  let total = read result
  return total

resetMaster :: Int -> IO String
resetMaster  worker = do
  result <- readProcess "git" ["-C","remote"++(show worker),"checkout", "master"] ""
  readProcess "git" ["-C","remote"++(show worker),"pull"] ""
  return result

resetToPrevCommit :: Integer -> Int -> IO String
resetToPrevCommit num  worker = do
  if (num==1)
        then do 
                result <- readProcess "git" ["-C","remote"++(show worker),"reset", "--hard", "HEAD"] ""
                return result
        else do 
                result <- readProcess "git" ["-C","remote"++(show worker),"reset", "--hard", ("HEAD~" ++ (show (num-1)))] ""
                return result

computeComplexity :: Int -> IO Integer
computeComplexity  worker = do
  r <- readProcess "./argonScript.sh" ["remote"++(show worker)] ""
  putStrLn $ "RESULT : "++r
  let complexity = read r
  return complexity

module BashCommand
  (cloneGitRepo, totalCommits, resetMaster, resetToPrevCommit, computeComplex) where

import           Control.Monad
import           System.Process

cloneGitRepo :: String -> IO String
cloneGitRepo repo = do
  readProcess "rm" ["-rf", "repo"] ""
  result <- readProcess "git" ["clone", repo, "remote"] ""
  return result

totalCommits :: IO Integer
totalCommits = do
  result <- readProcess "git" ["-C","remote","rev-list", "--count", "master"] ""
  let total = read result
  return total

resetMaster :: IO String
resetMaster = do
  result <- readProcess "git" ["-C","remote","checkout", "master"] ""
  readProcess "git" ["-C","remote","pull"] ""
  return result

resetToPrevCommit :: Integer -> IO String
resetToPrevCommit num = do
  if (num==1)
        then result <- readProcess "git" ["reset", "--hard", "HEAD"] ""
        else result <- readProcess "git" ["reset", "--hard", ("HEAD~" ++ (show (num-1)))] ""
    return result

computeComplex :: IO Integer
computeComplex = do
  result <- readProcess "./argonScript.sh" [] ""
  let total = read result
  return total

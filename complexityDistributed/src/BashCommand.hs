module BashCommand
  (cloneGitRepo, totalCommits, resetMaster, resetToPrevCommit, computeComplexity) where

import           Control.Monad
import           System.Process

cloneGitRepo :: String -> IO String
cloneGitRepo repo = do
  readProcess "rm" ["-rf", "remote"] ""
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
  putStrLn $ "Oh "++(show num)
  if (num==1)
        then do 
                result <- readProcess "git" ["-C","remote","reset", "--hard", "HEAD"] ""
                return result
        else do 
                result <- readProcess "git" ["-C","remote","reset", "--hard", ("HEAD~" ++ (show (num-1)))] ""
                return result

computeComplexity :: IO Integer
computeComplexity = do
  r <- readProcess "./c.sh" [] ""
  putStrLn $ "RESULT : "++r
  --let complexity = read r
  return 0

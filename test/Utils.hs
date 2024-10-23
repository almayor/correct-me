module Utils (withModifiedEnv, withSilencedOutput, findMissing) where

import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.IO
import Control.Exception (bracket_, finally)
import GHC.IO.Handle
import qualified Data.Set as Set

-- A helper function to temporarily modify environment variables
withModifiedEnv :: [(String, String)] -> IO () -> IO ()
withModifiedEnv newVars action = do
    -- Save the original environment
    originalEnv <- getEnvironment
    -- Set the new environment variables
    mapM_ (uncurry setEnv) newVars
    -- Ensure the original environment is restored afterward
    bracket_ (return ()) (restoreEnv originalEnv) action

  where
    -- Restore the original environment by unsetting any new variables and resetting the old ones
    restoreEnv env = do
      -- Unset all new variables
      let newKeys = map fst newVars
      mapM_ unsetEnv newKeys
      -- Restore the original environment
      mapM_ (uncurry setEnv) env


-- Helper function to redirect stdout and stderr to /dev/null
withSilencedOutput :: IO a -> IO a
withSilencedOutput action = do
    -- Open /dev/null for writing
    devNull <- openFile "/dev/null" WriteMode

    -- Save the original stdout and stderr
    originalStdout <- hDuplicate stdout
    originalStderr <- hDuplicate stderr

    -- Redirect stdout and stderr to /dev/null
    hDuplicateTo devNull stdout
    hDuplicateTo devNull stderr

    -- Ensure we restore the original stdout and stderr afterwards
    action `finally` do
        -- Restore the original stdout and stderr
        hDuplicateTo originalStdout stdout
        hDuplicateTo originalStderr stderr

        -- Close the original handles
        hClose devNull
        hClose originalStdout
        hClose originalStderr


-- General function to find the first missing value from a list, generalized for Ord and Bounded types
findMissing :: (Ord a, Bounded a, Enum a) => [a] -> a
findMissing xs = findFirstMissingSet existingSet [maxBound, pred maxBound .. minBound]
  where
    existingSet = Set.fromList xs  -- Convert list to Set for fast lookups

-- Helper function to find the first missing value using Set
findFirstMissingSet :: (Ord a) => Set.Set a -> [a] -> a
findFirstMissingSet existingSet (x:xs)
    | x `Set.notMember` existingSet = x  -- If x is not in the set, return it
    | otherwise = findFirstMissingSet existingSet xs

-- If the list covers the whole bounded range, this pattern match avoids non-exhaustive errors
findFirstMissingSet _ [] = error "No missing element found within the bounded range"

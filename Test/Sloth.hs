-- | The main module that defines the main function strictCheck for
--   testing whether a function is minimally strict.
module Test.Sloth
  (
    -- * Running Tests
    strictCheck, verboseCheck, interactCheck, check,

    -- * Configuration
    Config(..), defaultConfig, verboseConfig, successesConfig, uncoloredConfig,
    interactiveConfig,

    -- * Generation of Test Cases
    Data, Typeable,

    -- * Testing Polymorphic Functions
    A,

    -- -- * Generation of Dotty Graphs
    -- toAttr, writeCheck, writeInputRef
  ) where


import Data.Data ( Data, Typeable )

import Test.Sloth.CoMonad ( extract )
import Test.Sloth.TestCase ( TestCase, isValid, showTestCase )
import Test.Sloth.Refine ( bfs, coSeq, A )
import Test.Sloth.CharSet ( Testable(..), checkCharSet, pruneSet )
import Test.Sloth.Search ( Search )
import Test.Sloth.Config
  ( Config(..), defaultConfig, verboseConfig, successesConfig,
    uncoloredConfig, interactiveConfig )

-- The following module provides functions for debugging purposes.
-- import Test.Sloth.Internal.Dotty ( toAttr, writeCheck, writeInputRef )


-- | Test a function for partial values up to a specific size and do
-- not present successful test cases.
strictCheck :: Testable fun => fun -> Int -> IO ()
strictCheck = check defaultConfig

-- | Test a function for partial values up to a specific size and even
-- present successful test cases.
verboseCheck :: Testable fun => fun -> Int -> IO ()
verboseCheck = check verboseConfig

-- | Interactively test a function for partial values up to a specific
-- size.
interactCheck :: Testable fun => fun -> Int -> IO ()
interactCheck = check interactiveConfig

-- | Test a function for partial values up to a specific size where
-- the provided configuration determines which test cases are presented.
check :: Testable fun => Config -> fun -> Int -> IO ()
check config f n
  | interactive config = interactCheck' results
  | otherwise          = putStr (unlines results)
 where
  results = listCheck config f n

  interactCheck' []     = return ()
  interactCheck' [r]    = putStrLn r
  interactCheck' (r:rs) = do
    putStrLn r
    putStr "More? [y(es)/n(o)/a(ll)]"
    c <- getChar
    putStr "\n"
    case c of
         'n' -> return ()
         'a' -> putStr (unlines rs)
         _   -> interactCheck' rs

-- | Tests a function for partial values up to a specific size and
-- yields a list of results.
listCheck :: Testable fun => Config -> fun -> Int -> [String]
listCheck config f size = showResults 1 config (bfs r)
 where
  r = pruneSet size config 
               (coSeq (checkCharSet (charSet f (simpleApprox config) size)))

-- | Show test cases by generating a list of Strings.
showResults :: Int -> Config -> [Search TestCase] -> [String]
showResults n _      []     = ["Finished " ++ show (n-1) ++ " tests."]
showResults n config (t:ts)
  | isValid config (extract t) =
    (show n ++ ": " ++ showTestCase config t) : showResults (n+1) config ts
  | otherwise = showResults (n+1) config ts

module Test.Sloth.Config
  (
  Config(..),

  defaultConfig, verboseConfig, successesConfig, uncoloredConfig,
  interactiveConfig,
  ) where


data Config = Config { minInfSize   :: Int
                     , interactive  :: Bool
                     , colored      :: Bool
                     , successes    :: Bool
                     , noBottomPos  :: Bool
                     , detailed     :: Bool
                     , simpleApprox :: Bool }


-- | Default configuration
defaultConfig :: Config
defaultConfig = Config 2 False True False False False False

-- | Show test cases that are no counter-examples
verboseConfig :: Config
verboseConfig = defaultConfig { successes = True, noBottomPos = True }

-- | Show test cases that are no counter-examples but no test case
-- with total results
successesConfig :: Config
successesConfig = defaultConfig { successes = True }

-- | Do not use colors for output
uncoloredConfig :: Config
uncoloredConfig = defaultConfig { colored = False }

-- | Present counter-examples in interactive mode and give a detailed
-- explanation
interactiveConfig :: Config
interactiveConfig = defaultConfig { interactive = True, detailed = True }

module Test.Sloth.Color
  (
   Color(..), showColor
  ) where


data Color = Red
           | Green
           | Magenta
           | Blue

instance Show Color where
  show Red     = "31"
  show Green   = "32"
  show Magenta = "35"
  show Blue    = "34"


-- | Color the result of a show function
showColor :: Color -> ShowS -> ShowS
showColor color s =
  showString ("\027[" ++ show color ++ "m") . s . showString "\027[0m"

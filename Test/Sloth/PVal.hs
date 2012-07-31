{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables #-}

module Test.Sloth.PVal
  (
    PVal(..),

    -- * Show
    showsPrecPValWithSub,

    -- * Conversion
    toPVal, fromPVal,

    -- * Semantic Functions
    sel, infimum,

    -- * Bottom Positions
    botPos, botPosGE, isBottom,

    -- * Polymorphic Functions
    mapPoly, proj,

    simplifyPos
  ) where


import Prelude hiding ( catch )
import Control.Exception ( catches, evaluate, Handler(..), ErrorCall(..),
                           NonTermination, PatternMatchFail, ArithException )
import Control.Monad.State.Lazy ( State, runState, get, put, liftM )
import Data.Data
import Data.Monoid ( Monoid, mconcat, Endo(..) )
import Data.Maybe ( fromJust )
import Data.List ( intersperse, isPrefixOf )
import System.IO.Unsafe ( unsafePerformIO )

import Test.Sloth.Pos
import Test.Sloth.Poly ( polyA, polyPos, isDataTypeA )
import Test.Sloth.Function ( isDataTypeFun )
import Test.Sloth.Generics ( fromConstrWithChildren )


data PVal -- | A term where the first argument encodes the constructor
          = Cons Constr [PVal]
          -- | Bottom with position information
          | Bottom Pos
          -- | Bottom with no postion information
          | BottomNoPos
          -- | A runtime error
          | Error
          -- | A polymorpic component
          | Poly Pos

instance Show PVal where
  showsPrec = showsPrecPValWithSub []

showsBottom :: ShowS
showsBottom = showChar '_'

showsPoly :: Pos -> ShowS
showsPoly = showString . posToVar . first
 where
  posToVar n
    | n<=26     = [toEnum (97+n)]
    | otherwise = 'x':show n

showsBlank :: ShowS
showsBlank = showChar ' '

showsComma :: ShowS
showsComma = showChar ','

showsCons :: ShowS
showsCons = showChar ':'

-- | Shows a PVal with a specific substitution of some positions
showsPrecPValWithSub :: [(Pos,Int -> ShowS)] -> Int -> PVal -> ShowS
showsPrecPValWithSub l pr = showsPrecPVal pr root
 where
  showsPrecPVal _    _ Error       = showsBottom
  showsPrecPVal _    _ BottomNoPos = showsBottom
  showsPrecPVal prec p (Bottom _)  =
    maybe showsBottom ($ prec) (lookup p l)
  showsPrecPVal _    _ (Poly p)    = showsPoly p
  showsPrecPVal _    _ (Cons c []) = showString (showConstr c)
  showsPrecPVal prec p (Cons c ts)
    | isDataTypeFun dt   = intercalateS showsBlank (children prec)
    | isDataTypeTuple dt =
       showParen True (intercalateS showsComma (children minP))
    | isDataTypeList dt  =
       showParen (prec>listP)
          (showsPrecPVal (succ listP) (p|>0) (head ts) . showsCons
            . showsPrecPVal listP (p|>1) (head (tail ts)))
    | isInfix c =
       showParen (prec>appP)
                 (intercalateS (showString (infixSymb symb)) (children appP))
    | otherwise =
       showParen (prec>appP)
                 (intercalateS showsBlank
                               (showString symb:children (succ appP)))
   where
    children cP = mapWithPos (showsPrecPVal cP) p ts
    symb = showConstr c
    dt = constrType c
    minP = 0
    appP = 10
    listP = 5

isInfix :: Constr -> Bool
isInfix c = constrFixity c == Infix

infixSymb :: String -> String
infixSymb ('(':xs) = init xs
infixSymb symb = symb

isDataTypeTuple :: DataType -> Bool
isDataTypeTuple dt = "Prelude.(," `isPrefixOf` dataTypeName dt

isDataTypeList :: DataType -> Bool
isDataTypeList dt = dataTypeName dt == "Prelude.[]"

-- | Semantic join of two PVals
(/\) :: PVal -> PVal -> PVal
Bottom _ /\ _        = BottomNoPos
_        /\ Bottom _ = BottomNoPos
Poly p   /\ Poly p'
  | p==p'     = Poly p
  | otherwise = BottomNoPos 
Cons c1 ts1 /\ Cons c2 ts2
  | c1==c2    = Cons c1 (zipWith (/\) ts1 ts2)
  | otherwise = BottomNoPos
_ /\ _ = BottomNoPos

-- | The infimum of a list of PVals. In contrast to the standard
-- infimum this function yields _|_ for an empty list.
infimum :: [PVal] -> PVal
infimum [] = BottomNoPos
infimum xs = foldr1 (/\) xs

-- | Checks whether a partial value is bottom.
isBottom :: PVal -> Bool
isBottom (Bottom _)  = True
isBottom BottomNoPos = True
isBottom _           = False

-- | Convert a term representation into polymorphic value
fromPVal :: Data a => PVal -> a
fromPVal Error        = error "fromPVal: user error cannot be converted"
fromPVal (Bottom p)   = bottom p
fromPVal BottomNoPos  = bottomNoPos
fromPVal (Poly p)     = polyA p
fromPVal (Cons c pvs) = fromConstrWithChildren fromPVal pvs c

-- | Convert a polymorphic value into a term representation
toPVal :: forall a. Data a => a -> PVal
toPVal x = unsafePerformIO $
  catches (evaluate x >> return (cons x))
          [Handler errorCall, Handler nonTermination, 
           Handler patternMatchFail, Handler arithException, 
           Handler positionException, Handler noPositionException]
 where
  cons v
    | isDataTypeA dt = Poly (polyPos v)
    | otherwise      = Cons (toConstr v) (gmapQ toPVal v)
  dt = dataTypeOf (undefined :: a)

  positionException :: PositionException -> IO PVal
  positionException (PositionException p) = return (Bottom p)

  noPositionException :: NoPositionException -> IO PVal
  noPositionException NoPositionException = return BottomNoPos

  errorCall :: ErrorCall -> IO PVal
  errorCall _ = return Error

  nonTermination :: NonTermination -> IO PVal
  nonTermination _ = return Error

  patternMatchFail :: PatternMatchFail -> IO PVal
  patternMatchFail _ = return Error

  arithException :: ArithException -> IO PVal
  arithException _ = return Error

-- | Project to a specific position of a PVal. The function assumes
-- that the position is a valid position of the partial value.
sel :: PVal -> Pos -> PVal
sel pv rp
  | isRoot rp = pv
  | otherwise = sel (pvs!!first rp) (rest rp)
 where
  Cons _ pvs = pv

-- | Collect all bottom positions in a PVal which are greater equal to
-- a specific position. The function assumes that this position is
-- valid with respect to the PVal.
botPosGE :: PVal -> Pos -> [Pos]
botPosGE pv rp = botPosWithRoot rp (sel pv rp)

-- -- the following implementation is very slow !!!
-- botPosGE :: PVal -> Pos -> [Pos]
-- botPosGE pv rp = filter (>=rp) (botPos pv)

-- | Collect bottom positions in a PVal
botPos :: PVal -> [Pos]
botPos = botPosWithRoot root

botPosWithRoot :: Pos -> PVal -> [Pos]
botPosWithRoot _ Error       = []
botPosWithRoot _ BottomNoPos = []
botPosWithRoot p (Bottom _)  = [p]
botPosWithRoot _ (Poly _)    = []
botPosWithRoot p (Cons _ ts) = concat (mapWithPos botPosWithRoot p ts)

-- | Replace all occurences of the Poly constructor by a partial value
-- by employing a mapping from positions to partial values. This
-- function implements a generic fmap on basis of partial values.
mapPoly :: (Pos -> PVal) -> PVal -> PVal
mapPoly f (Cons c pvs) = Cons c (map (mapPoly f) pvs)
mapPoly f (Poly p)     = f p
mapPoly _ pv           = pv

-- | Project to a specific position of a partial value. If the
-- position does not exist the result is bottom. This function
-- implements a generic indexing to a polymorphic position.
proj :: PVal -> Pos -> PVal
proj pv rp
  | isRoot rp = pv
  | otherwise = 
      case pv of
           Cons _ pvs -> proj (pvs!!first rp) (rest rp)
           _          -> BottomNoPos

-- | Rename positions to letters a to z
simplifyPos :: PVal -> (PVal,Pos -> PVal)
simplifyPos pv = (pv',\p -> fromJust (lookup p mapping))
 where
  (pv',(_,mapping)) = runState (simplifyPosState pv) (0,[]) 

simplifyPosState :: PVal -> State (Int,[(Pos,PVal)]) PVal  
simplifyPosState (Cons c pvs) = liftM (Cons c) (mapM simplifyPosState pvs)
simplifyPosState (Poly p) = do
  (n,mapping) <- get
  let p' = singleton n
  put (n+1,(p,Poly p'):mapping)
  return (Poly p')
simplifyPosState pv = return pv


-- * Utility Functions

-- | Generalization of intercalate to arbitrary monoids
mintercalate :: Monoid m => m -> [m] -> m
mintercalate sep = mconcat . intersperse sep

-- | Specialization of mintercalate to functions
intercalateS :: (a -> a) -> [a -> a] -> a -> a
intercalateS sep = appEndo . mintercalate (Endo sep) . map Endo

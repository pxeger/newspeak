{-# LANGUAGE RankNTypes #-}

module Newspeak.Compiler where

import           Control.Applicative        (liftA2)
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.List                  (intercalate, sortOn, transpose)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, listToMaybe)
import           Data.Ratio                 (denominator, numerator)
import           Text.Read                  (readMaybe)

infixr 3 <&&>
(<&&>) = liftA2 (&&)

infixr 2 `orElse`
(Just x) `orElse` _ = Just x
Nothing  `orElse` x = x

data Type = ConcreteType (String, [Type]) | TypeVariable Int

type TypeCheckerState = Map.Map Int Type

typeCheck :: [Type] -> [Type] -> Maybe TypeCheckerState
typeCheck as bs = 
  let (result, state) = runState (_typeCheckN as bs) Map.empty
  in if result then Just state else Nothing
  where
  _typeCheck :: Type -> Type -> State TypeCheckerState Bool
  _typeCheck (ConcreteType (aName, aArgs)) (ConcreteType (bName, bArgs))
    | aName == bName
      = _typeCheckN aArgs bArgs
    | otherwise
      = return False
  _typeCheck (TypeVariable v) t
    = do state <- get
         case Map.lookup v state of
           Just t2 -> _typeCheck t t2
           Nothing -> do
             put $ Map.insert v t state
             return True
  _typeCheck t1 t2 = _typeCheck t2 t1

  _typeCheckN (a:as) (b:bs) = _typeCheck a b <&&> _typeCheckN as bs
  _typeCheckN []     []     = return True
  _typeCheckN _      _      = return False

substituteTypeVariables state (TypeVariable v)
  = state Map.! v
substituteTypeVariables state (ConcreteType (name, args))
  = ConcreteType (name, map (substituteTypeVariables state) args)

tNum = ConcreteType ("Number", [])
tList el = ConcreteType ("List", [el])

data Op = Literal Integer | Func (StateT [Value] IO ())

data CompilerState = CompilerState{stackTypes :: [Type], out :: [Op]}
type Compiler = CompilerState -> Maybe (Integer -> (Integer, CompilerState))

compile :: Integer -> Integer -> State CompilerState (Integer, Integer)
compile 0     program = return (0, program)
compile limit 0       = return (limit, 0)
compile limit program = do
  state <- get
  let validOps = catMaybes [op state | op <- ops]
  let (program', opIndex) = divMod (program - 1) (toInteger $ length validOps)
  let (program'', state') = (validOps !! fromInteger opIndex) program'
  put state'
  compile (limit - 1) program''

typeCheckedOp :: [Type] -> [Type] -> Op -> Compiler
typeCheckedOp inputTypes outputTypes op compilerState = do
    let arity = length inputTypes
    typeCheckerState <- typeCheck inputTypes (take arity $ stackTypes compilerState)
    let concreteOutputTypes = map (substituteTypeVariables typeCheckerState) outputTypes
    let state' = compilerState{stackTypes = concreteOutputTypes ++ (drop arity $ stackTypes compilerState)}
    return $ \program -> (program, state')

simpleOp :: [Type] -> Type -> ([Value] -> Value) -> Compiler
simpleOp inputTypes outputType f
 = typeCheckedOp inputTypes [outputType] $ Func $ modify op
 where op stack =
         let (args, rest) = splitAt (length inputTypes) stack
          in f args : rest

literalOp :: Compiler
literalOp compilerState = Just literal
  where literal program =
          let (x, program') = runState _literal program
           in (program', compilerState{out = Literal x : out compilerState})
        -- TODO use a better integer encoding
        _literal = do
          n <- gets (`mod` 256)
          modify (`div` 256)
          return n
        {- a modified form of Elias Omega Coding
        _literal :: State Integer Integer
        _literal = do
          bit1 <- takeBits 1
          case bit1 of
            0 -> _
        takeBits n = state $ \x -> (x .&. 1 `shiftL` fromInteger n - 1, x `shiftR` fromInteger n)
        -}


ops :: [Compiler]
ops = map snd $ sortOn fst _ops where
  _ops = [ -- duplicate
          (0, typeCheckedOp [TypeVariable 1] [TypeVariable 1, TypeVariable 1] $ Func dup)
          -- pop
        , (1, typeCheckedOp [TypeVariable 1] [] $ Func pop)
          -- arithmetic
        , (2, simpleOp [tNum, tNum] tNum (\[x, y] -> x + y))
        , (3, simpleOp [tNum, tNum] tNum (\[x, y] -> x - y))
        , (4, simpleOp [tNum, tNum] tNum (\[x, y] -> x * y))
        , (5, simpleOp [tNum, tNum] tNum (\[x, y] -> x / y))
        , (6, simpleOp [tNum, tNum] tNum (\[x, y] -> x ** y))
          -- literals
        , (7, undefined)
        ]
    where pop = modify tail
          dup = modify $ \(x:rest) -> x:x:rest

data Value
  = Int Integer
  | Float Double 
  | Char Char
  | List [Value]

isString  (List s)        = _isString s
  where
    _isString (Char _ : rest) = _isString rest
    _isString []              = True
    _isString _               = False
isString  _               = False

-- conversion utilities
makeString = List . map Char

stringify :: Show a => a -> Value
stringify = makeString . show

hsString :: Value -> String
hsString (List s) = _hsString s
  where
    _hsString (Char c : rest) = c : _hsString rest
    _hsString []              = []
    _hsString _               = error "not a string"
hsString _ = error "not a string"

makeBool False = Int 0
makeBool True  = Int 1 

instance Show Value where
  show (Int x)        = show x
  show (Float x)      = show x
  show (Char x)       = show x
  show x | isString x = show $ hsString x
  show (List xs)      = '[' : intercalate ", " (map show xs) ++ "]"

instance Read Value where
  readsPrec precedence s = 
    justs $ map (foldl orElse Nothing) $ transpose attempts
      where
        u constructor = maybes [(constructor val, rest) | (val, rest) <- readsPrec precedence s]
        attempts =
          [ u Int
          , u Float
          , u Char
          , u List
          , u makeString
          , u makeBool
          ]

        maybes :: [a] -> [Maybe a]
        maybes xs = map Just xs ++ repeat Nothing

        justs (Just x : xs) = x : justs xs
        justs _             = []


-- try to parse x into a Value, but default to just taking `x` as a literal string
readValue x = fromMaybe (makeString x) (readMaybe x)

compareUtil :: forall b. (forall a. Ord a => a -> a -> b) -> Value -> Value -> b
compareUtil f (Int x)   (Int y)   = f x y
compareUtil f (Float x) (Float y) = f x y
compareUtil f (Int x)   (Float y) = f (fromInteger x) y
compareUtil f (Float x) (Int y)   = f x (fromInteger y)
compareUtil f (List x)  (List y)  = f x y
compareUtil f (List x)  y         = f x [y]
compareUtil f x         (List y)  = f [x] y

opUtil :: (forall a. Num a => a -> a -> a) -> Value -> Value -> Value
opUtil f (Int x)   (Int y)   = Int (f x y)
opUtil f (Float x) (Float y) = Float (f x y)
opUtil f (Float x) (Int y)   = Float (f x (fromInteger y))
opUtil f (Int x)   (Float y) = Float (f (fromInteger x) y)

instance Num Value where
  (+) = opUtil (+)
  (-) = opUtil (-)
  (*) = opUtil (*)

  abs (Int x)   = Int (abs x)
  abs (Float x) = Float (abs x)

  signum (Int x)   = Int (signum x)
  signum (Float x) = Float (signum x)

  fromInteger x = Int x

instance Eq Value where
  Int x   == Int y   = x == y
  Float x == Float y = x == y
  Int x   == Float y = fromInteger x == y
  Float x == Int y   = x == fromInteger y

  List x  == List y  = x == y

  _       == _       = False

instance Ord Value where
  compare = compareUtil compare
  (>) = compareUtil (>)
  (<) = compareUtil (<)
  (>=) = compareUtil (>=)
  (<=) = compareUtil (<=)

instance Enum Value where
  toEnum x = Int (toInteger x)
  fromEnum (Int x) = fromInteger x
  fromEnum (Float x) = fromInteger (truncate x)

instance Fractional Value where
  Int x   / Int y   = Float (fromInteger x / fromInteger y)
  Float x / Float y = Float (x / y)
  Int x   / Float y = Float (fromInteger x / y)
  Float x / Int y   = Float (x / fromInteger y)

  fromRational x
    | denominator x == 1
    = Int (numerator x)
    | otherwise
    = Float (fromRational x)

instance Real Value where
  toRational (Int x) = toRational x

instance Integral Value where
  Int x `quotRem` Int y
    = let (q, r) = x `quotRem` y
       in (Int q, Int r)
  Float x `quotRem` Float y
    = let d = x / y
          n = truncate d
       in (Int n, Float (d - fromInteger n))
  Int x `quotRem` Float y = Float (fromInteger x) `quotRem` Float y
  Float x `quotRem` Int y = Float x `quotRem` Float (fromInteger y)

  toInteger (Int x)   = x
  toInteger (Float x) = truncate x

floatUtil f (Float x) = Float (f x)
floatUtil f (Int x)   = Float (f (fromInteger x))
floatUtil f (List xs) = List $ map (floatUtil f) xs

instance Floating Value where
  pi = Float pi

  Int x   ** Int y   = Int (x ^ y)
  Float x ** Int y   = Float (x ** fromInteger y)
  Int x   ** Float y = Float (fromInteger x ** y)
  Float x ** Float y = Float (x ** y)

  logBase (Int x)   (Int y)   = Float (logBase (fromInteger x) (fromInteger y))
  logBase (Float x) (Int y)   = Float (logBase x (fromInteger y))
  logBase (Int x)   (Float y) = Float (logBase (fromInteger x) y)
  logBase (Float x) (Float y) = Float (logBase x y)

  exp = floatUtil exp
  log = floatUtil log
  sqrt = floatUtil sqrt
  sin = floatUtil sin
  cos = floatUtil cos
  tan = floatUtil tan
  asin = floatUtil asin
  acos = floatUtil acos
  atan = floatUtil atan
  sinh = floatUtil sinh
  cosh = floatUtil cosh
  tanh = floatUtil tanh
  asinh = floatUtil asinh
  acosh = floatUtil acosh
  atanh = floatUtil atanh

main = undefined

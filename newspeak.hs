import           Control.Applicative        (liftA2)
import           Control.Monad.State.Strict
import           Data.List                  (sortOn)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
import           Data.Ratio                 (denominator, numerator)

infixr 3 <&&>
(<&&>) = liftA2 (&&) 

data Type = ConcreteType (String, [Type]) | TypeVariable Int

type TypeCheckerState = Map.Map Int Type

typeCheck as bs = runState (_typeCheckN as bs) Map.empty
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
tList = ConcreteType ("List", [TypeVariable 1])

type Op = StateT [Value] IO ()

data CompilerState = CompilerState { stackTypes :: [Type], out :: [Op] }

compile :: Int -> Int -> State CompilerState Int
compile 0     program = return program
compile _     0       = return 0
compile limit program = do
  stack <- gets stackTypes
  let validOps = catMaybes [op stack | op <- ops]
  let (program, opIndex) = divMod (program - 1) (length validOps)
  validOps !! opIndex
  compile (limit - 1) program

typeCheckedOp :: [Type] -> [Type] -> Op -> [Type] -> Maybe (State CompilerState ())
typeCheckedOp inputTypes outputTypes op stack = do
    let arity = length inputTypes
    case typeCheck inputTypes (take arity stack) of
      (True, state) -> Just (op arity state)
      (False, _)    -> Nothing
    where op :: Int -> TypeCheckerState -> State CompilerState ()
          op arity state = do
            let concreteOutputTypes = map (substituteTypeVariables state) outputTypes
            modify $ \s -> s { stackTypes = concreteOutputTypes ++ (drop arity $ stackTypes s) }

simpleOp :: [Type] -> Type -> ([Value] -> Value) -> [Type] -> Maybe (State CompilerState ())
simpleOp inputTypes outputType f
 = typeCheckedOp inputTypes [outputType] $ modify op
 where op stack =
         let (args, rest) = splitAt (length inputTypes) stack
          in f args : rest

ops :: [[Type] -> Maybe (State CompilerState ())]
ops = map snd $ sortOn fst ops' where
  ops' = [ -- duplicate
          (0, typeCheckedOp [TypeVariable 1] [TypeVariable 1, TypeVariable 1] dup)
          -- pop
        , (1, typeCheckedOp [TypeVariable 1] [] pop)
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

data Value = Int Integer | Float Double | List [Value]

instance Num Value where
  Int x   + Int y   = Int (x + y)
  Float x + Float y = Float (x + y)
  Int x   + Float y = Float (fromInteger x + y)
  Float x + Int y   = Float (x + fromInteger y)

  Int x   - Int y   = Int (x - y)
  Float x - Float y = Float (x - y)
  Int x   - Float y = Float (fromInteger x - y)
  Float x - Int y   = Float (x - fromInteger y)

  Int x   * Int y   = Int (x * y)
  Float x * Float y = Float (x * y)
  Int x   * Float y = Float (fromInteger x * y)
  Float x * Int y   = Float (x * fromInteger y)

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
  Int x   `compare` Int y   = x `compare` y
  Float x `compare` Float y = x `compare` y
  Int x   `compare` Float y = fromInteger x `compare` y
  Float x `compare` Int y   = x `compare` fromInteger y
  List x  `compare` List y  = x `compare` y

  Int x   > Int y   = x > y
  Float x > Float y = x > y
  Int x   > Float y = fromInteger x > y
  Float x > Int y   = x > fromInteger y
  List x  > List y  = x > y

  Int x   < Int y   = x < y
  Float x < Float y = x < y
  Int x   < Float y = fromInteger x < y
  Float x < Int y   = x < fromInteger y
  List x  < List y  = x < y

  Int x   >= Int y   = x >= y
  Float x >= Float y = x >= y
  Int x   >= Float y = fromInteger x >= y
  Float x >= Int y   = x >= fromInteger y
  List x  >= List y  = x >= y

  Int x   <= Int y   = x <= y
  Float x <= Float y = x <= y
  Int x   <= Float y = fromInteger x <= y
  Float x <= Int y   = x <= fromInteger y
  List x  <= List y  = x <= y

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

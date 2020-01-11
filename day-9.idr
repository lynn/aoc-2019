-- https://adventofcode.com/2019/day/9 in Idris.

import Data.Vect
import Debug.Trace

%hide Prelude.File.Mode

infixr 1 =<<
(=<<) : Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f

ℤ : Type
ℤ = Integer

Result : Type -> Type
Result = Either String

Program : Nat -> Type
Program n = Vect n ℤ

Input : Type
Input = List ℤ

Output : Type
Output = List ℤ

data Mode
    = Position
    | Immediate
    | Relative  -- shiny and new!

data Opcode
    = Arithmetic (ℤ -> ℤ -> ℤ)
    | JumpIf (ℤ -> Bool)
    | Scan
    | Print
    | AdjustRelativeBase
    | Halt

decodeMode : ℤ -> Result Mode
decodeMode 0 = Right Position
decodeMode 1 = Right Immediate
decodeMode 2 = Right Relative
decodeMode n = Left ("unknown mode " ++ show n)

decodeOpcode : ℤ -> Result Opcode
decodeOpcode 1 = Right (Arithmetic (+))
decodeOpcode 2 = Right (Arithmetic (*))
decodeOpcode 3 = Right Scan
decodeOpcode 4 = Right Print
decodeOpcode 5 = Right (JumpIf (/= 0))
decodeOpcode 6 = Right (JumpIf (== 0))
decodeOpcode 7 = Right (Arithmetic (\x, y => if x < y  then 1 else 0))
decodeOpcode 8 = Right (Arithmetic (\x, y => if x == y then 1 else 0))
decodeOpcode 9 = Right AdjustRelativeBase
decodeOpcode 99 = Right Halt
decodeOpcode n = Left ("unknown opcode " ++ show n)

-- Run a program of length p from interpreter state (i, rel) (the program counter and relative offset).
runFrom : {p : Nat} -> ℤ -> ℤ -> Program p -> Input -> Result Output
runFrom {p} i rel program input =
    do
        op <- word i
        modeA <- decodeMode (mod (div op 100) 10)
        modeB <- decodeMode (mod (div op 1000) 10)
        modeC <- decodeMode (mod (div op 10000) 10)
        opcode <- decodeOpcode (mod op 100)
        case opcode of
            Arithmetic f => do
                a <- fetch modeA =<< word (i + 1)
                b <- fetch modeB =<< word (i + 2)
                c <- intToAddress =<< fetchIndex modeC =<< word (i + 3)
                runFrom (i + 4) rel (replaceAt c (f a b) program) input
            JumpIf f => do
                a <- fetch modeA =<< word (i + 1)
                b <- fetch modeB =<< word (i + 2)
                runFrom (if f a then b else i + 3) rel program input
            Scan => do
                a <- intToAddress =<< fetchIndex modeA =<< word (i + 1)
                case input of
                    (x :: xs) => runFrom (i + 2) rel (replaceAt a x program) xs
                    [] => Left "end of input"
            Print => do
                a <- fetch modeA =<< word (i + 1)
                (a ::) <$> runFrom (i + 2) rel program input
            AdjustRelativeBase => do
                a <- fetch modeA =<< word (i + 1)
                runFrom (i + 2) (rel + a) program input
            Halt =>
                Right []
    where
        intToAddress : ℤ -> Result (Fin p)
        intToAddress i = maybeToEither "out of program bounds" (integerToFin i p)

        word : ℤ -> Result ℤ
        word = map (flip index program) . intToAddress

        fetch : Mode -> ℤ -> Result ℤ
        fetch Position k = word k
        fetch Immediate k = Right k
        fetch Relative k = word (k + rel)

        fetchIndex : Mode -> ℤ -> Result ℤ
        fetchIndex Position k = Right k
        fetchIndex Immediate k = Left "can't store into immediate"
        fetchIndex Relative k = Right (k + rel)

-- Run a program of length p.
run : {p : Nat} -> Program p -> Input -> Result Output
run program input = runFrom 0 0 program input

-- Turn a List into a Vect by padding it.
fromListPadding : (len : Nat) -> (pad : a) -> List a -> Vect len a
fromListPadding Z     _   _         = []
fromListPadding (S n) pad []        = pad :: fromListPadding n pad []
fromListPadding (S n) pad (x :: xs) = x   :: fromListPadding n pad xs

main : IO ()
main = do
    prog <- getLine
    let boost = fromListPadding 5000 0 $ the (List ℤ) $ map cast $ split (==',') prog
    printLn (run boost [1])
    printLn (run boost [2])

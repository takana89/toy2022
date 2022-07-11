module Toy where

import Data.Char
import Numeric

type SourceCode  = String
type Interactive = [Input] -> [Output]
type Input  = String
type Output = String

drive :: Interactive -> (String -> String)
drive f = unlines . f . lines

toy :: SourceCode -> Interactive
toy prog = map output . eval . initState (load prog)

type Label  = String
type Memory = [(Label, Content)]

data Content
    = Code Code
    | Data Int
    deriving Eq

instance Read Content where
    readsPrec _ str = case words str of
        [cnt] -> case readSigned readDec cnt of
            [(num, "")] -> [(Data num, "")]
            _           -> [(Code (read cnt, None), "")]
        [op, arg]
              -> [(Code (read op, read arg), "")]

load :: SourceCode -> Memory
load = map trans . lines

trans :: String -> (Label, Content)
trans s = case break isSpace s of
    ([], []) -> error "empty line"
    (lab, content) -> (lab, read content)

initState :: Memory -> ([Input] -> ToyState)
initState mem inputs = undefined

output :: ToyState -> Output
output state = case state of
    (_, _, _, _, output) -> output

type ToyState = (Final, Memory, Acc, [Input], Output)

type Final = Bool
type Acc   = Int

eval :: ToyState -> [ToyState]
eval state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = eval (step state)
            
isFinal :: ToyState -> Bool
isFinal (flg, _, _, _, _) = flg

step :: ToyState -> ToyState
step state = execute (decode (fetch state)) state

type Code = (Operator, Operand)

data Operator
    = STOP
    | GET
    | PRONT
    | LOAD
    | STORE
    | ADD
    | SUB
    | GOTO
    | IFZERO
    | IFPOS
    deriving (Eq, Show, Read)

data Operand
    = None
    | Num Int
    | Lab Label
    deriving (Eq, Show)

instance Read Operand where
    readsPrec _ str  = case words str of
        [oprd] -> case readSigned readDec oprd of
            [(num, "")] -> [(Num num, "")]
            _           -> [(Lab oprd, "")]
        _      -> []

fetch :: ToyState -> Code
fetch state = undefined

type Instruction = ToyState -> ToyState

decode :: Code -> Instruction
decode code = undefined

execute :: Instruction -> ToyState -> ToyState
execute cmd state = cmd state

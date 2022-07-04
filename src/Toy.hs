module Toy where

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

load :: SourceCode -> Memory
load = undefined

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
    deriving (Show, Read)

data Operand
    = None
    | Num Int
    | Lab Label
    deriving (Show, Read) 
    
fetch :: ToyState -> Code
fetch state = undefined

type Instruction = ToyState -> ToyState

decode :: Code -> Instruction
decode code = undefined

execute :: Instruction -> ToyState -> ToyState
execute cmd state = cmd state

module Toy where

type SourceCode  = String
type Interactive = [Input] -> [Output]
type Input  = String
type Output = String

drive :: Interactive -> (String -> String)
drive f = unlines . f . lines

toy :: SourceCode -> Interactive
toy prog = map output . eval . initState (load prog)

type Memory = ()

load :: SourceCode -> Memory
load = undefined

initState :: Memory -> ([Input] -> ToyState)
initState mem inputs = undefined

output :: ToyState -> Output
output state = undefined

type ToyState = ()

eval :: ToyState -> [ToyState]
eval state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = eval (step state)
            
isFinal :: ToyState -> Bool
isFinal state = undefined

step :: ToyState -> ToyState
step state = undefined
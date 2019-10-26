pal = (1,[2],[((1,"a",""),(1,"a")),
               ((1,"b",""),(1,"b")),
               ((1,"a",""),(2,"")),
               ((1,"b",""),(2,"")),
               ((1,"",""),(2,"")),
               ((2,"a","a"),(2,"")),
               ((2,"b","b"),(2,""))])


type PDA = (Integer,[Integer],[Transition])
type Transition = ((Integer,String,String),(Integer,String))
type Configuration = (Integer,String,String)

data Result = Accept | Reject
  deriving Show


run :: PDA -> String -> Result


run (startState, acceptingStates, trans) string
   | (startState == head acceptingStates) && (string == "") = Accept
   | recurTransitions acceptingStates (applyTransitions (startState, string, "") trans) trans == [(1, "yes", "yes")] = Accept
   | otherwise = Reject

recurTransitions :: [Integer] -> [Configuration] -> [Transition] -> [Configuration]

recurTransitions acceptingStates [] trans = [(1,"no","no")]
recurTransitions acceptingStates ((state, input, stack):ys) trans
  | input == "" && stack == "" && (elem state acceptingStates) = [(1, "yes", "yes")]
  | otherwise = recurTransitions acceptingStates (ys ++ applyTransitions (state, input, stack) trans) trans

applyTransitions :: Configuration -> [Transition] -> [Configuration]

applyTransitions (_) [] = []
applyTransitions (state, input, stack) (((transState, transInput, transStack), (nextState, nextStackHead)):xs) = [applyStack (state, input, stack) x | x <- getTransitions (state, input, stack) (((transState, transInput, transStack), (nextState, nextStackHead)):xs)]

getTransitions :: Configuration -> [Transition] -> [Transition]

getTransitions (_) [] = []
getTransitions (state, input, stack) (((transState, transInput, transStack), (nextState, nextStackHead)):xs)
    | (state == transState) && (take 1 input == transInput) && (take 1 stack == take 1 transStack) =
      ((transState, transInput, transStack), (nextState, nextStackHead)) : getTransitions (state, input, stack) xs
    | (state == transState) && (take 1 input == transInput) && transStack == "" =
      ((transState, transInput, transStack), (nextState, nextStackHead)) : getTransitions (state, input, stack) xs
    | (state == transState) && (transInput == "") && (transStack == "") = ((transState, transInput, transStack), (nextState, nextStackHead)) : getTransitions (state, input, stack) xs
    | (state == transState) && (transInput == "") && ((take 1 stack) == transStack) = ((transState, transInput, transStack), (nextState, nextStackHead)) : getTransitions (state, input, stack) xs
    | otherwise = getTransitions (state, input, stack) xs

applyStack :: Configuration -> Transition -> Configuration

applyStack (state, input, stack) ((transState, transInput, transStack), (nextState, nextStackHead))
    | (transInput /= "") && (transStack /= "") && (nextStackHead /= "") = (nextState, drop 1 input, nextStackHead ++ drop 1 stack)
    | (transInput /= "") && (transStack /= "") = (nextState, drop 1 input, drop 1 stack)
    | (transInput /= "") && (nextStackHead /= "") = (nextState, drop 1 input, nextStackHead ++ stack)
    | (transStack /= "") && (nextStackHead /= "") = (nextState, input, nextStackHead ++ drop 1 stack)
    | (transInput /= "") = (nextState, drop 1 input, stack)
    | (transStack /= "") = (nextState, input, drop 1 stack)
    | (nextStackHead /= "") = (nextState, input, nextStackHead ++ stack)
    | otherwise = (nextState, input, stack)

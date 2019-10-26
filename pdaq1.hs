

type Transition = ((Int, String, String), (Int, String))
type PDA = (Int, [Int], [Transitiion])
type Configuration = (Int, String, String)

data Result = Accept | Reject deriving Show -- String = Input string

run :: PDA -> String -> Result
run (start, end, transition) "" = Accept
run (start, end, transition) str  = runs (start, end, transition) [(start, str, "")]

runs :: PDA -> [Configuration] -> Result
runs (start, end, transition) [] = Reject
runs (start, end, transition) (head:tail)
| endState head end == True = Accept
| otherwise = runs(start, end, transition) (tail ++ (getNewConfigs head transition []))



-- Step takes a Configurtion, and a Transition
-- Returns  a List of Configurations
step :: Configuration -> Transitiion -> [Configuration]





--  This takes a Configuration, List of Transitions, List of Configuartions
-- Returns a List of Configurations
getNewConfigs :: Configuration -> [Transitiion] -> [Configuration] -> [Configuration]
-- When no more transitions exist
getNewConfigs c [] cs = cs
-- When a configuartion has no remaining input
getNewConfigs (a, "", c) tr cs = cs
-- Build list of configurations recursively while using the step function
getNewConfigs (a, b, c) (t:tr) cs = getNewConfigs (a,b,c) tr (cs ++ (step(a,b,c)t))




-- Rule 1
-- The current state of the stack is empty
-- There is no next character in the input
-- There is no new character to be pushed to the top of the stack
step (a,b,"") ((d,"",""),(g,""))
  -- if the current state variables are equal to eachother
  -- return the configuration [(new state, the remaining input, empty string(current state of the stack))]
  -- otherwise return empty List
  | a == d = [(g,b,"")]
  | otherwise = []


-- Rule 2
-- The current state of the stack is empty
-- There is no next character in the input
-- There is anew character to be pushed to the top of the stack
step (a, b, "") ((d, "", ""),(g, [h]))
  -- if the current state variables are equal to eachother
  -- return the configuration [(new state, the remaining input, empty current state of the stack))]
  | a == d = [(g, b, [h])]
  | otherwise = []


-- Rule 3
-- The current state of the stack is empty
-- There is a next character in the input
-- There is no new character to be pushed to the top of the stack
step (a, (b:bs), "") ((d, [e], ""), (g, ""))
  -- if the current state variables are equal to eachother
  -- and the head of the remaining input is equal to the next character of input
  -- return configuration
  -- return the configuration [(new state, the remaining input, empty current state of the stack))]
  | a == d && b == e = [(g, bs, "")]
  | otherwise = []


-- Rule 4
-- The current state of the stack is empty
-- There is a next character in the input
-- There is a new character to be pushed to the top of the stack
step (a,(b:bs),"") ((d,[e],""),(g,[h]))
  -- if the current state variables are equal to eachother
  -- and the head of the remaining input is equal to the next character of input
  -- return the configuration [(new state, the remaining input, current state of the stack))]
  | a == d && b == e = [(g, bs, [h])]
  | otherwise = []


--Rule 5
-- The current state of the stack is not empty
-- There is no next character in the input
-- There is no new character to be pushed to the top of the stack
step (a,b,c) ((d,"",""),(g,""))
  -- if the current state variables are equal to eachother
  -- return the configuration [(new state, the remaining input, current state of the stack))]
  | a == d = [(g, b, c)]
  | otherwise = []


--Rule 6
-- The current state of the stack is not empty
-- There is no next character in the input
-- There is a new character to be pushed to the top of the stack
step (a,b,c) ((d,"",""),(g,[h]))
  | a == d = [(g, b, (h:c))]
  | otherwise = []


--Rule 7
-- The current state of the stack is not empty
-- There is a character at the top of the stack
-- There is no new character to be pushed to the top of the stack
step (a,b,(c:cs)) ((d,"",[f]),(g,""))
  | a == d && c == f = [(g, b, cs)]
  | otherwise = []


--Rule 8
-- The current state of the stack is not empty
-- There is a character at the top of the stack
-- There is a new character to be pushed to the top of the stack
step (a,b,(c:cs)) ((d,"",[f]),(g,[h]))
  | a == d && c == f = [(g, b, (h:cs))]
  | otherwise = []


--Rule 9
-- The current state of the stack is not empty
-- There is a next character in the input
-- There is no new character to be pushed to the top of the stack
step (a,(b:bs),c) ((d,[e],""),(g,""))
  | a == d && b == e = [(g, bs, c)]
  | otherwise = []


--Rule 10
-- The current state of the stack is not empty
-- There is a next character in the input
-- There is a new character to be pushed to the top of the stack
step (a,(b:bs),c) ((d,[e],""),(g,[h]))
  | a == d && b == e = [(g, bs, h:c)]
  | otherwise = []


--Rule 11
-- The current state of the stack is not empty
-- There is a next character in the input and on the top of the stack
-- There is no new character to be pushed to the top of the stack1
step (a,(b:bs),(c:cs)) ((d,[e],[f]),(g,""))
    | a == d && b == e && c == f = [(g, bs, cs)]
    | otherwise = []


--Rule12
-- The current state of the stack is not empty
-- There is a next character in the input and on the top of the stack
-- There is a new character to be pushed to the top of the stack1
step (a,(b:bs),(c:cs)) ((d,[e], [f]), (g, [h]))
    | a == d && b == e && c == f = [(g, bs, (h:cs))]
    | otherwise = []


-- getNewConfigs which result in returning an empty List
step (a,b,"") ((d,[e],[f]),(g,"")) = []
step (a,b,"") ((d,"",[f]),(g,"")) = []
step (a,b,"") ((d,[e],[f]),(g,[h])) = []
step (a,b,"") ((d,"",[f]),(g,[h])) = []


-- Used to find if the final state of the pda has been reached
endState :: Configuration -> [Int] -> Bool
endState (a,b,c) [] = False
endState (a,b,c) (x:xs)
  | a == x && b == "" && c == "" = True
  | otherwise = endState (a,b,c) xs

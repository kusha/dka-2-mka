-- Project: DKA-2-MKA (FLP 15/16 FUN)
-- Author: Mark Birger (xbirge00)
-- Date: 7.4.2016
-- Minimalization algorithm

-- External modules
import System.IO
import System.Environment
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.Exit
import Debug.Trace

-- Internal modules
import AutomataTypes

-- | Main function, monad
main :: IO ()
main = do
    -- read arguments
    args <- getArgs 
    -- parse arguments
    let (minimize, inputSource) = procArgs args
    -- read input
    input <- fmap lines $ if (inputSource==[]) 
        then getContents 
        else readFile inputSource
    -- parse a valid automata
    let automata = parseAutomata input
    if not $ minimize then do
        putStrLn $ show automata
        return ()
    else do
        let sinkedAutomata = appendSink automata -- complete automata
        let table = firstIteration sinkedAutomata -- create inital equal classes table
        let splittedTalbe = fullSplit sinkedAutomata table -- split classes of equalty
        let generatedAutomata = generateAutomata sinkedAutomata splittedTalbe -- generate automata from table
        let result = removeSink generatedAutomata -- remove sink states from automata
        putStrLn $ show result
        return ()


-- PARSING STATE MACHINE PART

-- | Returns tuple (need to minimize, filename if exist)
-- from input agruments, raises error for more than 2 params.
procArgs :: [String] -> (Bool,String)
procArgs [] = (True, [])
procArgs [x]
    | x=="-i" = (False, [])
    | x=="-t" = (True, [])
    | otherwise = (True, x)
procArgs [x,y]
    | x=="-i" = (False, y)
    | x=="-s" = (True, y)
    | otherwise = (True, y)
procArgs _ = error "Program expects max 2 arguments"


-- | Returns parsed automata from array of lines.
-- raises erorrs in case of invalid input data.
parseAutomata :: [String] -> Automata
parseAutomata (states:start:finites:transitions) =
    if null transitions
        then (A getStates [] [] start getFinites) -- error "No transitions in automata"
        else completeAlphabet (A getStates [] getTransitions start getFinites) where -- generate alphabet by automata
            -- parse states set
            getStates = splitOn "," states
            -- parse transitions
            getTransitions = map getTransition transitions
            getTransition transition = getTransition' $ splitOn "," transition
            getTransition' [fromState,[atSymbol],toState] = T fromState atSymbol toState -- TODO check symbol
            getTransition' _ = error "Invalid transition definition" 
            -- parser finites
            getFinites = splitOn "," finites
            -- | Creates automata alphabet from transitions
            completeAlphabet :: Automata -> Automata
            completeAlphabet (A states _ transitions start finites) = (A states alphabet transitions start finites) where
                alphabet = removeDuplicates $ fmap getCharacter transitions
parseAutomata _ = error "Bad automata syntax"

-- | Get symbol from transition, used in alphabet generation
getCharacter :: Transition -> Symbol
getCharacter (T fromState atSymbol toState) = atSymbol

-- | Removes duplicates from list, datatype should be ordable
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort


-- MINIMIZATION PART

-- | Returns next state by state and symbol from transitions set.
-- Is safe, raises error in case of invalid transition set.
getTarget :: [Transition] -> State -> Symbol -> State
getTarget transitions from at =  case unsafeGetTarget transitions from at of
    Just state  -> state
    Nothing -> error "Can't find transition for state at the symbol" -- Can be in case of not complete FSM at minimalization stage 

-- | Unsafe getTarget, returns Maybe State.
-- Useful in SINK addition stage for non-existing transition detection
unsafeGetTarget :: [Transition] -> State -> Symbol -> Maybe State
unsafeGetTarget ((T fromState atSymbol toState):xs) from at = if (fromState == from) && (atSymbol == at)
    then Just toState 
    else unsafeGetTarget xs from at
unsafeGetTarget [] _ _ = Nothing

-- | Generate first an Equal Class.
-- At the first iteration S2C filled with ZEROS!!! Need to manual setClasses call.
initializeClass :: Automata -> State -> State2Classes
initializeClass (A _ alphabet _ _ _) state = (S2C state [0 | _ <- alphabet])

-- | Generate first iteration of Classes Table instance.
-- Set of [finites, nonfinites]
firstIteration :: Automata -> ClassesTable
firstIteration automata@(A _ alphabet _ _ finites) = [
        [initializeClass automata state | state <- finites], 
        [initializeClass automata state | state <- (nonFinite automata)]
    ]

-- | Returns set of non-finite State by Automata
nonFinite :: Automata -> [State]
nonFinite (A states _ _ _ finites) = [x | x <- states, not $ elem x finites]

-- setting state2class by himself
-- ClassesTable goes through this function
-- nested for (ClassesTable, Class, State2Classes, State)

-- | Set internal S2C links to classes indexes in ClassTable 
-- Need an automata to define transitions.
-- THIS IS IMPORTANT FUNCTIONS, HALF STEP OF minimizeStep FUNCTION
setClasses :: ClassesTable -> Automata -> ClassesTable
setClasses classes (A _ alphabet transitions _ _) = [[
    (S2C forState [getClass classes $ getTarget transitions forState symbol | symbol <- alphabet ]) 
        | (S2C forState _) <- c] 
        | c <- classes] 

-- | Return index of Equality class in Classes Table by State 
getClass :: ClassesTable -> State -> Int
getClass table state =  case elemIndex True $ [getClass' c state | c <- table] of
    Just idx  -> idx
    Nothing -> error "Invalid state machine, can't getClass" -- won't happen if valid FSM
getClass' :: Class -> State -> Bool
getClass' ((S2C forState _):xs) state = if forState == state
    then True
    else getClass' xs state
getClass' [] state = False
-- ^ Return True if State in Equality class

-- | Super simple Classes Table splitting function
-- It's posiible because of Ord and Eq definition in AutomataTypes.hs
splitClasses :: ClassesTable -> ClassesTable
splitClasses (x:xs) = groupSorted x ++ splitClasses xs where
    groupSorted = group . sort -- yes, sorting of equal classes work too :-)
splitClasses [] = []

-- | Minimization step of Classes Table
-- It is combination of setClasses and splitClasses function.
-- Requires automata for setClasses function.
minimizeStep :: Automata -> ClassesTable -> ClassesTable
minimizeStep automata classes = splitClasses $ setClasses classes automata

-- | Recursive do ... until implementation.
-- Splites Classes table while it is possible.
fullSplit :: Automata -> ClassesTable -> ClassesTable
fullSplit automata x = fullSplit' automata x $ minimizeStep automata x
fullSplit' :: Automata -> ClassesTable -> ClassesTable -> ClassesTable
fullSplit' automata x y = if x == y
    then x
    else fullSplit' automata y nextStep where
        nextStep = minimizeStep automata y


-- GENERATE AUTOMATA PART

-- | Generates automata by Automata and splitted Class Table.
generateAutomata :: Automata -> ClassesTable -> Automata
generateAutomata (A _ alphabet _ startOld finitesOld) classes = (A states alphabet transitions start finites) where
    states = [show x | x <- [0..n]] where
        n = pred $ length classes
    transitions = [
        (T (show from) (alphabet!!at) (show to)) --  note, that iterating over alphabet indexes
        | from <- [0..n], to <- [0..n], at <- [0..(pred $ length alphabet)], valid from to at] where
            n = pred $ length classes -- number of states (equal classes)
            valid from to at = (classes2Int classes) !! from !! at == to 
            -- ^ sink was pushed to end, filter by himself and setClasses 
            -- ^ UPDTED: sink now filters after generation (it allows to filter predefined custom SINK state)
    start = show $ getClass classes startOld
    finites = fmap show $ removeDuplicates [getClass classes x | x <- finitesOld] -- code reusage :-)

-- | Translates Classes Table to List of S2C toClasses (just links)
classes2Int :: ClassesTable -> [[Int]]
classes2Int classes = [generateTransitions $ head x | x <- classes] -- head is because they are equal by S2C toClasses

-- | Equal classes S2C to toClasses list of Ints
generateTransitions :: State2Classes -> [Int]
generateTransitions (S2C _ toClasses) = toClasses



-- SINK MANIPULATIONS PART

-- | Returns True if automata already complete (not adding SINK state)
isFull :: Automata -> Bool
isFull (A states alphabet transitions _ _) = not $ elem False 
    [ case unsafeGetTarget transitions from (alphabet!!at) of -- unsafeGetTarget reusage
        Just state  -> True
        Nothing -> False
    | from <- states, at <- [0..(pred $ length alphabet)] ]

-- | Returns new complete Automata with SINK state and added transitions
appendSink :: Automata -> Automata
appendSink automata@(A statesOld alphabet transitionsOld start finites) = if isFull automata then automata
    else (A states alphabet transitions start finites) where
    states = statesOld++("SINK":[]) -- adding SINK to the end! UPDATE: no matter anymore
    transitions = [ case unsafeGetTarget transitionsOld from (alphabet!!at) of
            Just state  -> (T from (alphabet!!at) state) -- use existing transition
            Nothing -> (T from (alphabet!!at) "SINK") -- or generate new transition
        | from <- statesOld, at <- [0..(pred $ length alphabet)] ] ++ -- for each combination from State at Symbol
        [ (T "SINK" (alphabet!!at) "SINK") | at <- [0..(pred $ length alphabet)] ] -- SINK to SINK transitions

-- | Removes SINK states form automata (non-finite, fully self-transitioned)
removeSink :: Automata -> Automata
removeSink automata@(A states alphabet transitions _ finites) = (
        let sinks = [ s | s <- states, not $ elem s finites, selfTransition transitions s alphabet] in
        removeStates automata sinks
    )

-- | Removes a set of States from automata (including transitions with this state)
removeStates :: Automata -> [State] -> Automata
removeStates automata [] =  automata
removeStates automata@(A statesOld alphabet transitionsOld start finites) (x:xs) = 
    removeStates (A states alphabet transitions start finites) xs where
        states = [s | s <- statesOld, s /= x]
        transitions = [t | t@(T from at to) <- transitionsOld, from /= x, to /= x]

-- | Return True if State has a transition to himself at each Symbol from list, recursively
selfTransition :: [Transition] -> State -> [Symbol] -> Bool
selfTransition transtitions state (x:xs) = if elem (T state x state) transtitions
    then selfTransition transtitions state xs
    else False
selfTransition _ _ [] = True

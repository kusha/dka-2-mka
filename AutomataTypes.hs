-- Project: DKA-2-MKA (FLP 15/16 FUN)
-- Author: Mark Birger (xbirge00)
-- Date: 7.4.2016
-- Additional datatypes

module AutomataTypes where

-- Extenal imports
import Data.List (intercalate)
import Data.Char (chr)

-- | Symlink for a state
type State = String 
-- | Symlink for a symbol from alphabet
type Symbol = Char

-- | Print State without quotes
displayState :: State -> String
displayState = id

-- | Print Symbol without quotes
displaySymbol :: Symbol -> String
displaySymbol = id . (:[])

-- | Datatype representing a transition
data Transition = T
    { fromState :: State
    , atSymbol :: Symbol
    , toState :: State
    }

-- | Transition printing (1,a,1 alike)
instance Show Transition where
    show (T fs as ts) = displayState fs ++ "," ++ displaySymbol as ++ "," ++ displayState ts 

-- | Transition equalty definition
-- Used for elem in selfTransition check.
instance Eq Transition where
    (T f1 a1 t1) == (T f2 a2 t2) = f1 == f2 && a1 == a2 && t1 == t2

-- | Automata representation (strictly formal)
data Automata = A
    { states :: [State]
    , alphabet :: [Symbol]
    , transitions :: [Transition]
    , start :: State
    , finites :: [State]
    }

-- | Automata printing
-- Warning: not prints alphabet as task requires.
instance Show Automata where
    -- "AUTOMATA whith alphabet " ++ show e ++ ":\n" ++ header for alphabet output
    show (A q e d q0 f) = displayListInline displayState q ++ "\n" 
        ++ displayState q0 ++ "\n" 
        ++ displayListInline displayState f  ++ 
        if not $ null d 
            then "\n" ++ displayListNewline show d 
            else ""

-- | Display list inline with comma without quotes
displayListInline func = intercalate "," . map func
-- | Display list with newlines without quotes
displayListNewline func = intercalate "\n" . map func

-- | This datatype represents State linked to Equalty Classes Indexes
data State2Classes = S2C
    { forState :: State
    , toClasses :: [Int]
    }  

-- Eq defined for grouping of S2C list
instance Eq State2Classes where
    (S2C _ toClasses1) == (S2C _ toClasses2) = toClasses1 == toClasses2

-- | Ord defined for sorting before grouping
-- This allows to group all values, creates nice sorting of ClassesTable
instance Ord State2Classes where
    compare (S2C _ toClasses1) (S2C _ toClasses2) = compare str1 str2 where
        str1 = toString toClasses1
        str2 = toString toClasses2

-- | Yes, sorting works by convertion to String and ordinal indexing :-O
toString :: [Int] -> String
toString i = [chr x | x <- i]

-- | Debug printing of S2C instances (not used in realesed version)
instance Show State2Classes where
    show (S2C forState toClasses) = "( " ++ show forState ++ " | " ++ show toClasses ++ " )"

-- | Symlink for Equalty Class (list of S2C)
type Class = [State2Classes]
-- | Symlink for Classes Table (list of Classes)
type ClassesTable = [Class]

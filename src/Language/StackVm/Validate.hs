module Language.StackVm.Validate (validate, LabeledLine, FunctionBlock) where
import Language.StackVm.Syntax
import Control.Monad.StatusMessage
import Prelude hiding (Ord(Lt, Gt, Eq))
import Data.List (sortBy, nubBy, (\\), concatMap, groupBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (!?))
import Control.Arrow ((***), first)

type LabeledLine = ((String, Int), Command) -- ((File, Line), Command)
type FunctionBlock = (String, Int, [LabeledLine]) -- (name, nLocal, commands)

-- |Fully analyze the source code. Calls all error checking functions check* as well as checking that a Sys.init function or leading code exists
validate :: [(String, [Command])] -> StatusMessage [FunctionBlock]
validate fs = let labeled = map labelLines fs
                  globalErrors = checkGlobals $ concat labeled
                  functions = concatMap getFunctions labeled
                  functionErrors = (\f -> checkFunction <$> (checkStack f =<< checkLabels f)) <$> functions
                  lineErrors = concatMap checkLines functions
                  noInit = if any (\(s, _, _) -> s == "Sys.init") functions then "" else "ERROR no Sys.init function"
                  allErrors = foldl (\(e, w) (e1, w1) -> (e ++ e1, w ++ w1)) ("", "") (globalErrors : (lineErrors, "") : map grabErrors functionErrors)
                  errorString = prependLines "ERROR " (fst allErrors) ++ noInit ++ prependLines "WARNING " (snd allErrors)
              in if null (fst allErrors) then Status (errorString, Just functions) else Status (errorString, Nothing)
  where grabErrors (Status (ew, Nothing)) = (ew, "")
        grabErrors (Status (ew, Just (e, w))) = (e, ew ++ w)
        prependLines s = unlines . map (s++) . lines

-- |Attach source position to each line
labelLines :: (String, [Command]) -> [LabeledLine]
labelLines (f, xs) = zipWith (\i x -> ((f, i), x)) [1..] xs

-- |Splits a file into a list of functions, potentially including statements before any functions as a @Sys.init@ function
getFunctions :: [LabeledLine] -> [FunctionBlock]
getFunctions = pullInit . foldr getFunctions' ([], [])
  where getFunctions' (_, Comment) ys = ys
        getFunctions' y@(_, Function s n) (xs, fs) = ([], (s, n, y:xs) : fs)
        getFunctions' y (xs, fs) = (y:xs, fs)
        pullInit ([], xs) = xs
        pullInit (x@(((f, n), _):_), xs) = ("Sys.init", 0, ((f, n-1), Function "Sys.init" 0) : x ++ [((f, n + length x), Return)]) : xs

-- |Checks issues with consistency among functions, which are
-- * Number of function arguments match
-- * Only calls functions which exist
-- * Only one function with the name name
-- * Functions are used
checkGlobals :: [LabeledLine] -> (String, String)
checkGlobals files = let names = groupBy (\a b -> fName (snd a) == fName (snd b)) . filter (isDef . snd) $ files
                         calls = groupBy (\a b -> fName (snd a) == fName (snd b)) . filter (isCall . snd) $ files
                         dupNames = concatMap (\fs -> if length fs /= 1
                                                      then concatMap (\(p, c) -> errorMsg "one of several definitions of function" id (p, fName c)) fs
                                                      else "") names
                         argMismatch = concatMap (\fs -> if length (groupBy (\a b -> nArgs (snd a) == nArgs (snd b)) fs) /= 1
                                                         then concatMap (\(p, c) -> errorMsg "one of several numbers of arguments to function" show (p, nArgs c)) fs
                                                         else "") calls
                         names' = map (fName . snd . head) names -- TODO use set
                         calls' = map (fName . snd) (concat calls)
                         unused = concatMap (\(p, c) -> errorMsg "unused function" id (p, fName c)) $ filter (\(_, c) -> fName c `notElem` ("Sys.init" : calls')) $ map head names
                         undef = concatMap (\(p, c) -> errorMsg "use of undefined function" id (p, fName c)) $ filter (\(_, c) -> fName c `notElem` names') $ concat calls
                     in (dupNames ++ argMismatch ++ undef, unused)
  where isDef (Function _ _) = True
        isDef _ = False
        isCall (Call _ _) = True
        isCall _ = False
        fName (Function f _) = f
        fName (Call f _) = f
        fName _ = undefined
        nArgs (Call _ n) = n
        nArgs _ = undefined

-- |Checks potential problems that occus viewing one line at a time within a function
-- * Does not push to constant segment
-- * Does not use excessively large costant
-- * Does not overrun LCL
-- * Does not overrun Temp
-- * Does not access a negative index except constant
-- * Does not access Pointer other than 0 or 1
-- * Sys.init does not use argument
-- * Positive or 0 nLocal
checkLines :: FunctionBlock -> String
checkLines (f, nLocal, xs) = concatMap checkLine xs
  where checkLine (p, Pop  Constant _) = errorMsg "pop to constant segment" id (p, "")
        checkLine (p, Push Constant i) = if i >= 0x8000 || i < -0x4000 then errorMsg "constant exceeds bounds (min=-0x4000 max=0x7fff" id (p, "") else ""
        checkLine (p, Pop  Local i) = if i >= nLocal then errorMsg "local segment overrun" (\n -> "(min=0, max=" ++ show (n-1) ++ ")") (p, nLocal) else checkNat p i
        checkLine (p, Push Local i) = if i >= nLocal then errorMsg "local segment overrun" (\n -> "(min=0, max=" ++ show (n-1) ++ ")") (p, nLocal) else checkNat p i
        checkLine (p, Pop  Pointer i) = if i /= 1 && i /= 0 then errorMsg "use of pointer other than 0 or 1" id (p, "") else ""
        checkLine (p, Push Pointer i) = if i /= 1 && i /= 0 then errorMsg "use of pointer other than 0 or 1" id (p, "") else ""
        checkLine (p, Pop  Temp i) = if i > 7 then errorMsg "only 8 temp registers" id (p, "") else checkNat p i
        checkLine (p, Push Temp i) = if i > 7 then errorMsg "only 8 temp registers" id (p, "") else checkNat p i
        checkLine (p, Pop  Argument i) = if f == "Sys.init" then errorMsg "Sys.init has no arguments" id (p, "") else checkNat p i
        checkLine (p, Push Argument i) = if f == "Sys.init" then errorMsg "Sys.init has no arguments" id (p, "") else checkNat p i
        checkLine (p, Pop  _ i) = checkNat p i
        checkLine (p, Push _ i) = checkNat p i
        checkLine (p, Function _ n) = checkNat p n
        checkLine _ = ""
        checkNat p i = if i < 0 then errorMsg "segment can't have negative index" id (p, "") else ""

-- |Check control flow in a function. Ensures the following requirements:
-- * All functions return
-- * No lines outside of a function
-- * No unreachable code
-- * Function consumes all of stack, but not before the start
-- * Line has same stack height no matter how it is reached
checkFunction :: (String, Int, [(Maybe (Maybe Int), LabeledLine)]) -> (String, String)
checkFunction (_, _, _:xs) = checkFunction' True xs ("", "")
  where checkFunction' reachable ((_, (_, Comment)) : xs) e = checkFunction' reachable xs e
        checkFunction' _ ((Nothing, (p, _)) : xs) (e, w) = checkFunction' False xs (e, w ++ errorMsg "unreachable line" id (p, ""))
        checkFunction' _ ((Just Nothing, (p, Label _)) : xs) (e, w) = checkFunction' True xs
                                                                      (e ++ errorMsg "lines that pass control to here have different stack heights" id (p, ""), w)
        checkFunction' _ ((_, (_, Goto _)) : xs) e = checkFunction' False xs e
        checkFunction' _ ((Just (Just 1), (_, Return)) : xs) e = checkFunction' False xs e
        checkFunction' _ ((Just (Just h), (p, Return)) : xs) (e, w) = checkFunction' False xs
                                                                      (e ++ errorMsg "return statement requires stack height 1" (\a -> "is " ++ show a) (p, h), w)
        checkFunction' _ ((Just (Just h), (p, x)) : xs) (e, w) = checkFunction' True xs (e ++ checkOpHeight p x h, w)
        checkFunction' reachable [] (e, w) = if reachable then (e ++ errorMsg "function must end with return or goto" id ((\((_, ((f, i), _)):_) -> (f, i-1)) xs, ""), w) else (e, w)
        checkOpHeight p x h = if minH x > h
                              then errorMsg "stack height too low for operation" (const $ "(needed=" ++ show (minH x) ++ ", actual=" ++ show h ++ ")") (p, "")
                              else ""
        minH Add =            2
        minH Sub =            2
        minH Neg =            1
        minH Eq =             2
        minH Lt =             2
        minH Gt =             2
        minH And =            2
        minH Or =             2
        minH Not =            1
        minH (Push _ _) =     0
        minH (Pop _ _) =      1
        minH (Label _) =      0
        minH (Goto _) =       undefined
        minH (IfGoto _) =     1
        minH (Function _ _) = undefined
        minH (Call _ n) =     n
        minH Return =         undefined
        minH Comment =        undefined
checkFunction _ = undefined

-- |Attaches stack height to each line. Notes whether the line is unreachable (Nothing) or has conflicting height (Just Nothing).
--  Assumes all labels have definitions.
checkStack :: FunctionBlock -> Map.Map String String -> StatusMessage (String, Int, [(Maybe (Maybe Int), LabeledLine)])
checkStack (f, n, [(p, _)]) _ = fail $ errorMsg "function can't be empty" id (p, "")
checkStack (f, n, _ : xs@((begin, _) : _)) labels = let connections = buildGraph Map.empty xs
                                                        dh = Map.fromList $ map (getKey *** deltaH) xs
                                                        levels = stackLevels (Map.singleton (getKey begin) (return 0)) connections dh (getKey begin)
                                                    in return (f, n, map (\x -> (levels !? getKey (fst x), x)) xs)
  where -- Builds a graph of what lines can go where. Call initially with empty map
        buildGraph :: Map.Map String [String] -> [LabeledLine] -> Map.Map String [String]
        buildGraph m ((p,        Goto s)   : xs)                = Map.insert (getKey p) [labels ! s]                 $ buildGraph m xs
        buildGraph m ((p@(f, i), IfGoto s) : xs@(((_,i'),_):_)) = Map.insert (getKey p) [labels ! s, getKey (f, i')] $ buildGraph m xs
        buildGraph m ((p,        Return)   : xs)                = Map.insert (getKey p) []                           $ buildGraph m xs
        buildGraph m ((p@(f, i), _)        : xs@(((_,i'),_):_)) = Map.insert (getKey p) [getKey (f, i')]             $ buildGraph m xs
        buildGraph m ((p, _) : []) = Map.insert (getKey p) [] m
        buildGraph m [] = m
        -- How each command affects stack height
        deltaH :: Command -> Int
        deltaH Add =           -1
        deltaH Sub =           -1
        deltaH Neg =            0
        deltaH Eq =            -1
        deltaH Lt =            -1
        deltaH Gt =            -1
        deltaH And =           -1
        deltaH Or =            -1
        deltaH Not =            0
        deltaH (Push _ _) =     1
        deltaH (Pop _ _) =     -1
        deltaH (Label _) =      0
        deltaH (Goto _) =       0
        deltaH (IfGoto _) =    -1
        deltaH (Function _ _) = undefined
        deltaH (Call _ n) =     1 - n
        deltaH Return =        -1
        deltaH Comment =        0
        -- Build map of height at each line
        stackLevels :: Map.Map String (Maybe Int) -> Map.Map String [String] -> Map.Map String Int -> String -> Map.Map String (Maybe Int)
        stackLevels m e dh v = foldr (\v' m' -> let h = (+(dh ! v)) <$> m ! v -- Height continuing from current line
                                                    h' = m' !? v'             -- Height already known for that line, if available
                                                in if isNothing h'
                                                     then stackLevels (Map.insert v' h m') e dh v'
                                                   else if h' == Just h
                                                     then m'
                                                   else Map.insert v' Nothing m') m (e ! v)
checkStack (f, n, []) _ = undefined

-- |Creates a map of label to source position. Checks errors
-- * Goto calls stay within function
-- * Labels are used and are not redefined
checkLabels :: FunctionBlock -> StatusMessage (Map.Map String String)
checkLabels (s, i, f) = let labels = collectLabels f
                            (redef, defs, jmps) = checkLabels' labels
                            unused = diffBy snd defs jmps
                            undef = diffBy snd jmps defs
                        in Status (concatMap (errorMsg "redefinition of label" id) redef ++
                                     concatMap (errorMsg "unknown label" id) undef ++
                                     concatMap (errorMsg "unused label" id) unused,
                                   if length redef + length unused == 0 then Just (Map.fromList . map (\(p, s) -> (s, getKey p)) $ fst labels) else Nothing)
  where collectLabels' (p, Label s) (l, j) = ((p, s) : l, j)
        collectLabels' (p, Goto s) (l, j) = (l, (p, s) : j)
        collectLabels' (p, IfGoto s) (l, j) = (l, (p, s) : j)
        collectLabels' _ x = x
        collectLabels :: [LabeledLine] -> ([((String, Int), String)], [((String, Int), String)])
        collectLabels = mapBoth (sortBy $ comparing snd) . foldr collectLabels' ([], [])
        checkLabels' :: ([((String, Int), String)], [((String, Int), String)]) -> ([((String, Int), String)], [((String, Int), String)], [((String, Int), String)])
        checkLabels' (defs, jmps) = let (defs', errs) = removeDuplicatesBy snd defs; (jmps', _) = removeDuplicatesBy snd jmps in (errs, defs', jmps')

-- |Given a sorted list, returns (unique elements, duplicated elements)
removeDuplicatesBy :: Eq b => (a -> b) -> [a] -> ([a], [a])
removeDuplicatesBy f xss@(x:xs) = (nubBy (\a b -> f a == f b) xss, catMaybes $ zipWith (\a b -> if f a == f b then Just a else Nothing) xss xs)
removeDuplicatesBy f [] = ([], [])

-- |Elements of first sorted list not in second sorted list
diffBy :: Eq b => (a -> b) -> [a] -> [a] -> [a]
diffBy _ [] _ = []
diffBy _ xs [] = xs
diffBy f (x:xs) (y:ys) = if f x == f y then diffBy f xs ys else diffBy f (x:xs) ys

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

-- |Template for consistent error messages
errorMsg :: String             -- ^ Description of error
         -> (a -> String)      -- ^ Custom show function
         -> ((String, Int), a) -- ^ (Source position, info)
         -> String             -- ^ Formatted error
errorMsg m s (p, e) = getKey p ++ ", " ++ m ++ ": " ++ s e ++ "\n"

getKey :: (String, Int) -> String
getKey (f, n) = f ++ ':' : show n

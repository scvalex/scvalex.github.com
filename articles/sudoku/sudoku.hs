import Data.Array.Diff
import Data.List
import Data.Maybe
import System.Environment ( getArgs )

type Position = (Int, Int)

data Cell = Filled Int
          | Possibilities [Int]
          | Invalid
            deriving (Eq, Show)

isFilled :: Cell -> Bool
isFilled (Filled _) = True
isFilled _ = False

getFilled :: Cell -> Maybe Int
getFilled (Filled x) = Just x
getFilled _ = Nothing

type Board = DiffArray Position Cell

type ZoneDef = [Position]

stdZones :: [ZoneDef]
stdZones = concat [ [[(l, c) | c <- [0..8]] | l <- [0..8]]
                  , [[(l, c) | l <- [0..8]] | c <- [0..8]]
                  , [[(l, c) | l <- [i*3..i*3+2], c <- [j*3..j*3+2]] | i <- [0..2], j <- [0..2]]
                  ]

allPossibilities :: [Int]
allPossibilities = [1..9]

zonesFor :: [ZoneDef] -> Position -> [ZoneDef]
zonesFor zds p = filter (elem p) zds

normalizeCell :: Cell -> Cell
normalizeCell (Possibilities xs)
    | null xs = Invalid
    | length xs == 1 = Filled (head xs)
normalizeCell x = x

mkCell :: [Int] -> [ZoneDef] -> Board -> Position -> Cell
mkCell aps zdfs brd p = let zs = zonesFor zdfs  p
                            cs = map (brd!) $ concat zs
                            alreadyUsed = catMaybes . map getFilled $ cs
                            ps = aps \\ alreadyUsed
                        in normalizeCell (Possibilities ps)

inferSolveBoard :: [Int] -> [ZoneDef] -> Board -> Board
inferSolveBoard aps zdfs brd = let cs = assocs brd
                                   notFilled = filter (not . isFilled . snd) cs
                                   filled = filter (isFilled . snd) cs
                                   newCs = map (\(p, _) -> (p, mkCell aps zdfs brd p)) notFilled
                                   cs' = newCs ++ filled
                               in array (bounds brd) cs'

inferInferUntilNoChange :: [Int] -> [ZoneDef] -> Board -> Board
inferInferUntilNoChange aps zdfs brd = let brd' = inferSolveBoard aps zdfs brd
                                       in if elems brd == elems brd' then brd else inferInferUntilNoChange aps zdfs brd'

data BoardStatus = BoardIncomplete | BoardComplete | BoardInvalid

checkBoard :: Board -> BoardStatus
checkBoard brd = let invalid = filter (==Invalid) . elems $ brd
                     notFilled = filter (not . isFilled) . elems $ brd
                 in if not . null $ invalid
                    then BoardInvalid
                    else if null notFilled then BoardComplete else BoardIncomplete

findSolution :: [Int] -> [ZoneDef] -> Board -> Maybe Board
findSolution aps zdfs brd = let brd' = inferInferUntilNoChange aps zdfs brd
                            in case checkBoard brd' of
                                 BoardInvalid -> Nothing
                                 BoardComplete -> Just brd'
                                 BoardIncomplete -> let p = fst . head . filter (not . isFilled . snd) . assocs $ brd'
                                                        (Possibilities ps) = brd' ! p
                                                    in listToMaybe $ catMaybes [findSolution aps zdfs (brd' // [(p, Filled x)]) | x <- ps]

getLines :: FilePath -> IO [String]
getLines f = do
  ls <- readFile f
  return $ lines ls

mkBoard :: [Int] -> Board
mkBoard is = listArray ((0, 0), (8, 8)) $ map (\i -> if i `elem` [1..9] then Filled i else Invalid) is

getBoard :: FilePath -> IO Board
getBoard f = do
  ls <- getLines f
  let is = concatMap (map read) . map words $ ls
  return $ mkBoard is

showStdBoard :: Board -> String
showStdBoard brd = let x = map (\(Filled x) -> x) . elems $ brd
                   in unlines . map unwords . map (map show) . reverse 
                      $ fst (iterate (\(t, xs) -> let (h, r) = splitAt 9 xs in (h:t, r)) ([], x) !! 9) 

main :: IO ()
main = do
  (f:_) <- getArgs
  brd <- getBoard f
  putStrLn $ showStdBoard $ fromJust $ findSolution allPossibilities stdZones brd

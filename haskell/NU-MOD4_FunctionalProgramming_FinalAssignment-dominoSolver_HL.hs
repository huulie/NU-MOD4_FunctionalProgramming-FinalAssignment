--                Domino problem solver                --
-- Final assignment of the Functional Programming part -- 
--      Module 4: the critical software developer      -- 
--        Nedap University [5] - September 2020        --
--              Written by Huub Lievestro              --
--     with help from Teun van Hemert (supervisor)     --

import Data.List

-- Definition of types, to descrive the problem -- 
type Position = (Int, Int)    -- (x,y) position on the grid, with origin (0,0) in left upper corner
type Bone = ((Int, Int), Int) -- Bone with corresponding number of pips on the bone, and its number
type PipGrid = [Int]          -- the input, with number of pips per position, indices corresponding with positions (see helper functions)
type BoneGrid = [Int]         -- an output (one possible solution), with bone number per position (or -1 for empty), indices corresponding with positions (see helper functions)


-- Settings of the solver --
-- Board dimensions:
width :: Int -- width of the board (x-direction)
width = 8 

height :: Int -- height of the board (y-direction)
height = 7 


-- Setting the constraints for options to be valid -- 
valid :: (Position,Position) -> BoneGrid -> PipGrid -> Bone -> [Bone] -> Bool -- evaluates all constraints below
valid (p1,p2) bg pg b bs = validOnBoard p1 && validOnBoard p2 && validOnFree p1 bg && validOnFree p2 bg && validPipMatch (position2pip p1 pg) (position2pip p2 pg) b && validNotUsed b bs

validOnBoard :: Position -> Bool -- checks if x- and y-coordinate of a position are within bounds of the board
validOnBoard (x,y) | xValid x && yValid y = True
                   | otherwise            = False
    where xValid x = x >= 0 && x < width
          yValid y = y >= 0 && y < height

validOnFree :: Position -> BoneGrid -> Bool -- checks if Position is empty (== -1) in BoneGrid
validOnFree p bg | bg !!(position2index p) == -1 = True
                 | otherwise = False

validPipMatch :: Int -> Int -> Bone -> Bool -- checks if pip at first position matches first pip of bone, same for second pip
validPipMatch n1 n2 ((nl,nr),_) | n1 == nl && n2 == nr = True
                                | otherwise = False 

validNotUsed :: Bone -> [Bone] -> Bool -- checks if Bone is in list of available Bones
validNotUsed b bs | elem b bs = True
                  | otherwise = False
 -- Note: if trying only bones from the remaining bones list, this constraint is implicitly enforced


-- Solving the problem -- 
solve :: PipGrid  -> [BoneGrid]
solve pg = gotoNextPosition (-1,-1) emptyBoneGrid pg bones

gotoNextPosition :: Position -> BoneGrid -> PipGrid -> [Bone] -> [BoneGrid]  -- Finds next empty position and tries each of the remaining bones, returns list of valid resulting BoneGrid 
gotoNextPosition p bg pg bs = concat [allOrientations (nextEmptyPosition bg p) bg pg b bs | b <- bs]

allOrientations :: Position -> BoneGrid -> PipGrid -> Bone -> [Bone] -> [BoneGrid] -- Try the place a bone in each of the orientations, returns list of valid resulting BoneGrids 
allOrientations p bg pg b bs | boneSymmetrical b =  concat [checkAndPlace p o bg pg b bs | o <- [Horizontal, Vertical]]
                             | otherwise = concat [checkAndPlace p o bg pg b bs | o <- [Horizontal, Vertical, InvHorizontal, InvVertical]]
                             where boneSymmetrical ((nl,nr),_) = nl == nr -- don't show both solutions for a symmetrical bone

checkAndPlace :: Position -> Orientation -> BoneGrid -> PipGrid -> Bone -> [Bone] -> [BoneGrid]
checkAndPlace p o bg pg b bs | length bs == 1 = if checkOrientation p o bg pg b bs then [putBoneOnGrid bg p o b] else [] -- if placing last bone, stop with recursing
                             | checkOrientation p o bg pg b bs = gotoNextPosition p (putBoneOnGrid bg p o b) pg (removeElementFromList b bs) -- try next position (recursive)
                             | otherwise = []


-- Running the solver program -- 
-- Ask for input:
dominoSolver :: IO ()
dominoSolver = do putStrLn "Type an input (linear notation, row-major order): "
                  input <- getLine -- need to parse the input
                  putStrLn "Solving the problem..."
                  --  solve input

-- or use one of the example PipGrids:
examplePipGrid1 :: PipGrid -- has 4 solutions
examplePipGrid1 = [6, 6, 2, 6, 5, 2, 4, 1, 
                   1, 3, 2, 0, 1, 0, 3, 4,
                   1, 3, 2, 4, 6, 6, 5, 4,
                   1, 0, 4, 3, 2, 1, 1, 2,
                   5, 1, 3, 6, 0, 4, 5, 5,
                   5, 5, 4, 0, 2, 6, 0, 3,
                   6, 0, 5, 3, 4, 2, 0, 3]

examplePipGrid2 :: PipGrid -- has 2 solutions
examplePipGrid2 = [4, 2, 5, 2, 6, 3, 5, 4, 
                   5, 0, 4, 3, 1, 4, 1, 1,
                   1, 2, 3, 0, 2, 2, 2, 2,
                   1, 0, 4, 3, 2, 1, 1, 2,
                   4, 0, 6, 0, 3, 6, 6, 5,
                   4, 0, 1, 6, 4, 0, 3, 0,
                   6, 5, 3, 6, 2, 1, 5, 3]


-- Helper functions --
-- for the solver:
removeElementFromList :: Eq a => a -> [a] -> [a]
removeElementFromList x xs = filter (not . (==x)) xs

emptyBoneGrid :: BoneGrid -- generates empty BoneGrid, filled with -1
emptyBoneGrid = take (height*width) (repeat (-1))

bones :: [Bone] -- generates list of bones, generating a Double Six dominoes set
bones = generateBones 0 1

generateBones :: Int -> Int -> [Bone]
generateBones 7 _ = []
generateBones s n = generateSerie s n ++ generateBones (s+1) (n + (7-s))
    where generateSerie s n = [((s,y), (n + y-s)) | y <- [s..6]]

data Orientation = Horizontal | Vertical | InvHorizontal | InvVertical
orientation :: Orientation -> Position -> (Position, Position)
orientation Horizontal    (x,y) = ((x,y),(x+1,y))
orientation Vertical      (x,y) = ((x,y),(x,y+1))
orientation InvHorizontal (x,y) = ((x+1,y),(x,y))
orientation InvVertical   (x,y) = ((x,y+1),(x,y))

position2pip :: Position -> PipGrid ->  Int -- gets number of pips at a Position in the PipGrid
position2pip p pg = pg !! (position2index p)

position2index :: Position -> Int -- warning: Only if nothing was removed from the list!
position2index (x,y) = (y * width + x)

index2position :: Int -> Position -- warning: Only if nothing was removed from the list!
index2position i = (i `mod` width, i `div` width)

nextPosition :: Position -> Position -- returns next position on the grid, row-major order
nextPosition (-1,-1) = (0,0)
nextPosition p = index2position (position2index p +1)

nextEmptyPosition :: BoneGrid -> Position -> Position -- next empty position on bonegrid bg, starting from position p
nextEmptyPosition bg p | p == (width-1,height-1) = (0,0) -- note: when on last position, start at beginning of board
                       | bg !!(position2index (nextPosition p)) == -1 = nextPosition p
                       | otherwise = nextEmptyPosition bg (nextPosition p)

checkOrientation :: Position -> Orientation -> BoneGrid -> PipGrid -> Bone -> [Bone] -> Bool -- Checks if Bone with Orientation is valid on BoneGrid and PipGrid
checkOrientation p o bg pg b bs = valid (orientation o p) bg pg b bs

putBoneOnGrid :: BoneGrid -> Position -> Orientation -> Bone -> BoneGrid -- warning: only use AFTER checking that position + orientation is validOnBoard!
putBoneOnGrid bg p o b = changeDualPositions bg (orientation o p) b

changeDualPositions :: BoneGrid -> (Position, Position) -> Bone -> BoneGrid 
changeDualPositions bg (p1,p2) ((_,_), bn) = replacePosition p2 bn (replacePosition p1 bn bg)
    where replacePosition p bn bg  = take (position2index p) bg ++ [bn] ++ drop (position2index p +1) bg

-- for IO things:
-- -- parseUserInput:: String -> [String]
-- parseUserInput i = splitOn "," i
-- -- parseUserInput = ...

-- line with n items from list xs
-- printListLine :: Int -> [a] -> IO ()
-- printListLine _ [] = []
-- printListLine n xs = splitAt n xs
--     where printLine (l, n) = putStrLn l printListLine n 


-- n < 10 = "0" + n
-- otherwise = n
-- printGrid :: Grid -> IO () -- NEEDS WORK
-- printGrid ns = putStr [n ,n <- ns] 
--     where printRowHorz g y = [g!!i | x <-[0..WIDTH] ] ++ ['/n]

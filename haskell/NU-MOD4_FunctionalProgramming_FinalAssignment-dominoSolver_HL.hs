--                Domino problem solver                --
-- Final assignment of the Functional Programming part -- 
--      Module 4: the critical software developer      -- 
--        Nedap University [5] - September 2020        --
--              Written by Huub Lievestro              --

import Data.List -- Needed for findIndex

-- Definition of types, to descrive the problem -- 
type Position = (Int, Int) -- (x,y) position on the grid, with origin (0,0) in left upper corner

type Bone = (Int, Int) -- Bone with corresponding number of pips on the bone, identified by its number = index in bones (see helper functions)
type PlacedBone = ((Position, Position), Int) -- bone spanning two positions, and its number -- PM: may also be placed in array with index position/number?

type PipGrid = [Int] -- the input, with number of pips per position, corresponding with positions (see helper functions)
type BoneGrid = [Int] -- the output(s), with bone number per position, corresponding with positions (see helper functions)


-- Settings of the solver --
-- Board dimensions:
width :: Int -- width of the board (x-direction)
width = 8 

height :: Int -- height of the board (y-direction)
height = 7 


-- Setting the constraints for options to be valid -- 
valid :: (Position,Position) -> [Position] -> PipGrid -> Bone -> [Bone] -> Bool
valid (p1,p2) ps pg b bs = validOnBoard p1 && validOnBoard p2 && validOnFree p1 ps && validOnFree p2 ps && validPipMatch (position2pip p1 pg) (position2pip p2 pg) b && validNotUsed b bs

validOnBoard :: Position -> Bool -- checks if x- and y-coordinate of a position are within bounds of the board
validOnBoard (x,y) | xValid x && yValid y = True
                   | otherwise            = False
    where xValid x = x >= 0 && x < width
          yValid y = y >= 0 && y < height

validOnFree :: Position -> [Position] -> Bool -- checks if Position is in list of available Positions
validOnFree p ps | elem p ps = True
                 | otherwise = False

validPipMatch :: Int -> Int -> Bone -> Bool -- checks if pip at first position matches first pip of bone, same for second pip
validPipMatch n1 n2 (nl,nr) | n1 == nl && n2 == nr = True
                            | otherwise = False 

validNotUsed :: Bone -> [Bone] -> Bool -- checks if Bone is in list of available Bones
validNotUsed b bs | elem b bs = True
                  | otherwise = False
 -- every time calling function recursively, also provide argument with remaining bones (every step should have information to do its job)
 -- PM: not nessecary if trying all bones from the remaining bones list (then this is implicitly enforced)


-- Solving the problem -- 
solve :: PipGrid  -> [BoneGrid]
solve = undefined
-- CheckOrientaties voor positie (x,y) 
-- Is ((x,y) , (x+1,y)) een valide mogelijkheid? 
--   Zoja: CheckOrientaties voor de volgende positie 
--   Zonee: = oplossing|iets doms 
-- Is ((x,y), (x,y+1)) een valide mogelijkheid? 
--   Zoja: CheckOrientaties voor de volgende positie 
--   Zonee:  = oplossing|iets doms 
-- !! PM: orientation can also be "reversed" horizontal/vertical, because not all bones are symmetric
-- misschien met een variable bord die in recursie steeds kleiner wordt als posities gevuld? Dan ook dat probleem ondervangen
-- oplossing als alle bones geplaatst || laatste pip valide geplaatst

-- meegeven: input, beschikbare posities, beschikbare bones, oplossing tot nu toe
-- info mee naar benenden geven, en dan oplossingen naar boven (geen recursie opleveren)
-- functies met veel input zijn iet erg, zeker niet aan het begin
-- voor ons wss handiger om programma van onder (kleiner) naar boven te schrijven, omdat anders in begin al idee waar heen gaat nodig

-- next position is head ps

nextStepHorizontal :: Position -> [Position] -> PipGrid -> [Bone] -> a -- for each bone taken from all remaining bones 
nextStepHorizontal p ps pg bs = undefined -- [checkHorizontal (orientation Horizontal p) ps pg b bs | b <- bs] -- eventually shiftX and shiftY from generators?!

-- meegeven: input, beschikbare posities, beschikbare bones, oplossing tot nu toe
-- checkHorizontal :: PipGrid -> (Position, Position) -> [Positions] -> [Bones] -> BoneGrid
checkHorizontal :: (Position, Position) -> [Position] -> PipGrid -> Bone -> [Bone] -> b-- -> BoneGrid
checkHorizontal (p1,p2) ps pg b bs | valid (p1,p2) ps pg b bs = undefined -- next recrusion step, with (p1,p2) and b removed from ps resp. bs
                                   | length bs == 0 = undefined -- end recursion, all bones placed = solution -> BoneGrid                                        
                                   | otherwise = undefined -- end recursion, not all bones placed = nonesense

-- Running the solver program -- 
-- Ask for input:
dominoSolver :: IO ()
dominoSolver = do putStrLn "Type an input (linear notation, row-major order): "
                  input <- getLine -- need to parse the input
                  putStrLn "Solving the problem..."
                  --  solve input

-- or use one of the example PipGrids:
examplePipGrid1 :: PipGrid
examplePipGrid1 = [6, 6, 2, 6, 5, 2, 4, 1, 
                   1, 3, 2, 0, 1, 0, 3, 4,
                   1, 3, 2, 4, 6, 6, 5, 4,
                   1, 0, 4, 3, 2, 1, 1, 2,
                   5, 1, 3, 6, 0, 4, 5, 5,
                   5, 5, 4, 0, 2, 6, 0, 3,
                   6, 0, 5, 3, 4, 2, 0, 3]


-- Helper functions --
positions :: [Position] -- generates list of positions, ..  to map pips or bones on
positions = [(x,y) | y <- [0..height], x <- [0..width]]
-- this maps the 2D-postions to a 1D-array, linear index, row-major order, starting at zero

bones :: [Bone] -- generates list of bones, index correspond to their number
bones = generateBones 0

generateBones :: Int -> [Bone]
generateBones 7 = []
generateBones n = generateSerie n ++ generateBones (n+1)
    where generateSerie n = [(n,y) | y <- [n..6]]

data Orientation = Horizontal | Vertical | InvHorizontal | InvVertical
orientation :: Orientation -> Position -> (Position, Position)
orientation Horizontal    (x,y) = ((x,y),(x+1,y))
orientation Vertical      (x,y) = ((x,y),(x,y+1))
orientation InvHorizontal (x,y) = ((x+1,y),(x,y))
orientation InvVertical   (x,y) = ((x,y+1),(x,y))

position2index :: Position -> Int -- Only if nothing was removed from the list!
position2index (x,y) = (y * width + x)

position2pip :: Position -> PipGrid ->  Int
position2pip p pg = pg !! (position2index p)

removePositionFromList :: Position -> [Position] -> [Position]
removePositionFromList p ps = filter (not . (==p)) ps


-- parseUserInput = ...

-- printGrid :: Grid -> IO () -- NEEDS WORK
-- printGrid ns = putStr [n ,n <- ns] 
--     where printRowHorz g y = [g!!i | x <-[0..WIDTH] ] ++ ['/n]
--                Domino problem solver                --
-- Final assignment of the Functional Programming part -- 
--      Module 4: the critical software developer      -- 
--        Nedap University [5] - September 2020        --
--              Written by Huub Lievestro              --


-- Definition of types, to descrive the problem -- 
type Position = (Int, Int) -- (x,y) position on the grid, with origin in left upper corner

type Bone = (Int, Int) -- Bone with corresponding number of pips on the bone, identified by its number = index in bones (see helper functions)
-- type Bone = (Int, (Int, Int)) -- bone identified by its number, with corresponding number of pips on the bone
type PlacedBone = ((Position, Position), Int) -- bone spanning two positions, and its number

type PipGrid = [Int] -- the input, with number of pips per position, corresponding with positions (see helper functions)
type BoneGrid = [Int] -- the output(s), with bone number per position, , corresponding with positions (see helper functions)

-- -- type Grid = [Int] -- maybe combine the two below into this one
-- type PipGrid = [(Position, Int)] -- 
-- type BoneGrid = [(Position, Int)] -- this is the output, with bone number per position

-- Setting of the solver --
-- Board dimensions:
width :: Int -- width of the board (x-direction)
width = 8 

height :: Int -- height of the board (y-direction)
height = 7 

-- Setting the constraints -- 

valid :: Position -> Position -> Bone -> Bool
valid p1 p2 b == validOnBoard p1 && validOnBoard p2 && validOnFree p1 && validOnFree p2 && validPipMatch p1 p2 b && validNotUsed b

validOnBoard :: Position -> Bool
validOnBoard (x,y) | xValid x && yValid y = True
                   | otherwise        = False
    where xValid x = x >= 0 && x < width
          yValid y = y >= 0 && y < height

validOnFree :: Position -> Bool
validOnFree p = undefined
-- could be checked when taken positions are removed from board list, but then index will lose its correspondence

validPipMatch :: Position -> Position -> Bone -> Bool
validPipMatch p p b = undefined 
-- should check if pip at first position matches first pip of bone, same for second

validNotUsed :: Bone -> Bool
validNotUsed b = undefined
-- should check if bone is already used: how?!



solve :: PipGrid -> [BoneGrid]
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


-- userInput = 

-- data Move = Left | Right | Up | Down

-- move :: Move -> Position -> Position
-- move Left (x, y) = (x 1, y)
-- move Right (x, y) = (x + 1, y)
-- move Up (x, y) = (x, y 1)
-- move Down (x, y) = (x, y + 1)

-- printGrid :: Grid -> IO ()
-- printGrid ns = putStr [n ,n <- ns] 
    
-- putStr(
--     where printRowHorz g y = [g!!i | x <-[0..WIDTH] ] ++ ['/n]
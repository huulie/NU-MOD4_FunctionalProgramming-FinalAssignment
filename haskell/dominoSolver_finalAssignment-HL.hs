--                Domino problem solver                --
-- Final assignment of the Functional Programming part -- 
--      Module 4: the critical software developer      -- 
--        Nedap University [5] - September 2020        --
--              Written by Huub Lievestro              --


-- Definition of types, to descrive the problem -- 
type Position = (Int, Int) -- (x,y) position on the grid

type Bone = (Int, (Int, Int)) -- bone identified by its number, with corresponding number of pips on the bone
type PlacedBone((Position, Position), Int) -- bone spanning two positions, and its number

type Grid [Int] -- maybe combine the two below into this one
type PipGrid = [(Position, Int)] -- this is the input, with number of pips per position
type BoneGrid = [(Position, Int)] -- this is the output, with bone number per position

-- Setting of the solver --
-- Board dimensions:
WIDTH :: Int -- width of the board (y-direction)
WIDTH = 7 

HEIGHT :: Int -- height of the board (y-direction)
HEIGHT = 7 

-- xxx-- 




-- Some helper methods:
printGrid :: grid -> IO ()
printGrid g = putStr(grid)
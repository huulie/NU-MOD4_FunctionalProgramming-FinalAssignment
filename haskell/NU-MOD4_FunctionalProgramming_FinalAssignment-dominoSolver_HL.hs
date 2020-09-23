--                Domino problem solver                --
-- Final assignment of the Functional Programming part -- 
--      Module 4: the critical software developer      -- 
--        Nedap University [5] - September 2020        --
--              Written by Huub Lievestro              --


-- Definition of types, to descrive the problem -- 
type Position = (Int, Int) -- (x,y) position on the grid, with origin (0,0) in left upper corner
type Bone = ((Int, Int), Int) -- Bone with corresponding number of pips on the bone, and its number
type PipGrid = [Int] -- the input, with number of pips per position, corresponding with positions (see helper functions)
type BoneGrid = [Int] -- the output(s), with bone number per position, corresponding with positions (see helper functions)


-- Settings of the solver --
-- Board dimensions:
width :: Int -- width of the board (x-direction)
width = 8 

height :: Int -- height of the board (y-direction)
height = 7 


-- Setting the constraints for options to be valid -- 
valid :: (Position,Position) -> BoneGrid -> PipGrid -> Bone -> [Bone] -> Bool -- evaluates all critera below
valid (p1,p2) bg pg b bs = validOnBoard p1 && validOnBoard p2 && validOnFree p1 bg && validOnFree p2 bg && validPipMatch (position2pip p1 pg) (position2pip p2 pg) b && validNotUsed b bs

validOnBoard :: Position -> Bool -- checks if x- and y-coordinate of a position are within bounds of the board
validOnBoard (x,y) | xValid x && yValid y = True
                   | otherwise            = False
    where xValid x = x >= 0 && x < width
          yValid y = y >= 0 && y < height

validOnFree :: Position -> BoneGrid -> Bool -- checks if Position is in list of available Positions
validOnFree p bg | bg !!(position2index p) == -1 = True
                 | otherwise = False

validPipMatch :: Int -> Int -> Bone -> Bool -- checks if pip at first position matches first pip of bone, same for second pip
validPipMatch n1 n2 ((nl,nr),_) | n1 == nl && n2 == nr = True
                                | otherwise = False 

validNotUsed :: Bone -> [Bone] -> Bool -- checks if Bone is in list of available Bones
validNotUsed b bs | elem b bs = True
                  | otherwise = False
 -- PM: every time calling function recursively, also provide argument with remaining bones (every step should have information to do its job)
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

-- driver :: 
-- driver = -- check alle orientaties, en als valide ga naar volgende positie met dat bord
-- meegeven: input, beschikbare posities, beschikbare bones, oplossing tot nu toe


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
removeElementFromList :: Eq a => a -> [a] -> [a]
removeElementFromList x xs = filter (not . (==x)) xs

emptyBoneGrid :: BoneGrid
emptyBoneGrid = take (height*width) (repeat (-1))

bones :: [Bone] -- generates list of bones, index correspond to their number
bones = generateBones 0 1

generateBones :: Int -> Int -> [Bone]
generateBones 7 _ = []
generateBones s n = generateSerie s n ++ generateBones (s+1) (n + (7-s))
    where generateSerie s n = [((s,y), (n + y-s)) | y <- [s..6]] -- bone number calculation TODO

data Orientation = Horizontal | Vertical | InvHorizontal | InvVertical
orientation :: Orientation -> Position -> (Position, Position)
orientation Horizontal    (x,y) = ((x,y),(x+1,y))
orientation Vertical      (x,y) = ((x,y),(x,y+1))
orientation InvHorizontal (x,y) = ((x+1,y),(x,y))
orientation InvVertical   (x,y) = ((x,y+1),(x,y))

position2pip :: Position -> PipGrid ->  Int
position2pip p pg = pg !! (position2index p)

position2index :: Position -> Int -- Only if nothing was removed from the list!
position2index (x,y) = (y * width + x)

index2position :: Int -> Position -- Only if nothing was removed from the list!
index2position i = (i `mod` width, i `div` width)

nextPosition :: Position -> Position
nextPosition p = index2position (position2index p +1)

nextEmptyPosition :: BoneGrid -> Position -> Position -- next empty position on bonegrid bg, starteing from position p
nextEmptyPosition bg p | bg !!(position2index (nextPosition p)) == -1 = nextPosition p
                       | otherwise = nextEmptyPosition bg (nextPosition p)
-- op die position, leg [elke overgebleven steen] voor alle 4 de orientaties, en ga dan evt recursief verder
-- Positon -> Board


-- tryBones bs | null bs = "solution"
--             | checkOrientation "with 4 orientations for each bone in bs"

-- maybe try bones and orientations, and flatten resulting array

--    | valid (p1,p2) ps pg b bs = nextStep
--    | length bs == 0 = undefined -- end recursion, all bones placed = solution -> BoneGrid                                        
--    | otherwise = undefined -- end recursion, not all bones placed = nonesense


checkOrientation :: Position -> Orientation -> BoneGrid -> PipGrid -> Bone -> [Bone] -> Bool
checkOrientation p o bg pg b bs = valid (orientation o p) bg pg b bs
-- if True, then putBoneOnGrid and iterate to next position 


putBoneOnGrid :: BoneGrid -> Position -> Orientation -> Bone -> BoneGrid -- warning: only use AFTER checking that position + orientation is validOnBoard!
putBoneOnGrid bg p o b = changeDualPositions bg (orientation o p) b

changeDualPositions :: BoneGrid -> (Position, Position) -> Bone -> BoneGrid 
changeDualPositions bg (p1,p2) ((_,_), bn) = replacePosition p2 bn (replacePosition p1 bn bg)
    where replacePosition p bn bg  = take (position2index p) bg ++ [bn] ++ (drop (position2index p) bg)

-- tested with putBoneOnGrid emptyBoneGrid (0,0) Horizontal ((1,1),1) = [1,1,....]





--- PREVIOUS ATTEMPTS AND SCRAP PAD---
-- checkOrientation :: Position -> Orientation -> BoneGrid -> PipGrid -> Bone -> [Bone] -> Bool
-- checkOrientation p o bg pg b bs = valid (orientation o p) bg pg b bs -- | b <- bs] -- avoid list comprehension here?!

-- checkStep ps pg bs = [checkOrientation p o ps pg b bs | o <- [Horizontal, Vertical, InvHorizontal, InvVertical], b <- bs, p <- ps]
-- if ps/bs length == 0: stop recursion, found solution
-- if true: remove p and b from list and recurse
-- if false: stop recursion, nonesense

-- parseUserInput = ...

-- printGrid :: Grid -> IO () -- NEEDS WORK
-- printGrid ns = putStr [n ,n <- ns] 
--     where printRowHorz g y = [g!!i | x <-[0..WIDTH] ] ++ ['/n]


--- FEEDBACK ---
-- meegeven: input, beschikbare posities, beschikbare bones, oplossing tot nu toe
-- info mee naar benenden geven, en dan oplossingen naar boven (geen recursie opleveren)
-- functies met veel input zijn niet erg, zeker niet aan het begin
-- voor ons wss handiger om programma van onder (kleiner) naar boven te schrijven, omdat anders in begin al idee waar heen gaat nodig

-- idee om horizontaal en verticaal proberen wel goed, recursieve functie moet horizontaal en verticaal proberen
-- 1 functie recursief die algoritme vooruit stuwt, en dan een functie voor horizontaal en verticaal 
-- probeer simpele functies te maken, ding wat aan elkaar knoopt is ingewikkeld apparaat
-- Ik probeer nu list comprehensions aan elkaar te knopen
-- functie doet zet en produceerd nieuw bord, probleem makkelijker: bord bijhouden dat ingevuld (eerst leeg, met bijv -1), per stap lijst van moves
-- werk met kleine, simpele functies

-- Represent board, and there search next position (next empty spot): list of positions is "not human" and another list (wel voor bones)
-- algrotime in kleine stapjes uitwerken en die implementeren

-- ik schrijf nu meer in for loopjes (vermomd als list comprehension), dat is imperatief
-- moet toe naar zoek lege postitie -> leg daar steen neer in 4 mogelijke orientaties -> en doet dit nog keer, tot stenen op zijn
-- bedenk goed dat je niks kunt veranderen, alleen nieuwe versie door kunt geven

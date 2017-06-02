
--substrings a list
substr start end lst
 | null lst   = []
 | start == end  = []
 | start == 0    = (head lst):(substr start (end - 1)  (tail lst))
 | otherwise     = substr (start-1) (end - 1) (tail lst)


--wrapper for below since we will always pass 0 for n 
getCombinations lst = getCombinationsHelper lst 0

--retrives all combinations of a lst 
-- ex. getCombinations "__XX" 0, returns ["__XX","_XX_","XX__","X__X"]
getCombinationsHelper lst n
 | null lst  = [[]]
 | n == (length lst)    = []
 | otherwise = substr n (n + (length lst) ) (lst ++ lst) : getCombinationsHelper lst (n + 1)



 
--filters the combinations by removing impossible configurations such as "X__X"
--ex. refineCombinations ["__XX","_XX_","XX__","X__X"], returns ["__XX","_XX_","XX__"]
refineCombinations lst = filter (not . isImpossible) lst


--filter condition helper
isImpossible lst
 | (isLetter (lst !! 0)) && (isLetter (lst !! ((length lst) - 1)))  = True
 | otherwise                                                        = False
 
--checks if item is a letter
isLetter item
 | elem item "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   = True
 | otherwise                                = False


--wrapper for below since we will always pass [] for accumulator
carList lst  = carListHelper lst []
 
--determines horizontal cars there are in a 'lane'
--used to find out legal moves
--ex. "__XXAAB" will return [X,A]
carListHelper lst acc  
 | null lst    = [] ++ acc
 | (tail lst ) /= [] && (head lst) == (head (tail lst)) && (isLetter (head lst)) && (not (elem (head lst) acc)) = carListHelper (tail lst) ((head lst):acc)
 | otherwise  = carListHelper (tail lst) acc

 
--determines the unmovable states  in lst before car 
--ex. "__XXAAB" returns ""
beforeCar lst car
 | lst == []                   = []
 | (car /= (head (head lst)) && (isLetter (head (head lst)))) || ((head (head lst)) == '-' && (head(head (tail lst))) /= car) = (head lst)++(beforeCar (tail lst) car)
 | otherwise                   = []

--determines unmovable states in lst after car0
--ex. "__XXAAB" returns "AAB"
afterCar lst car = reverse (beforeCar (reverse lst) car)


--helper for groupCars 
groupCars []     = []
groupCars (x:xs) = (groupCarsHelper xs x):(groupCars (groupCarsHelper2 xs x))   

groupCarsHelper lst car
 | lst == []         = car:[]
 | car == (head lst) = (head lst):(groupCarsHelper (tail lst) car)
 | otherwise         = car:[]
 
groupCarsHelper2 lst car
 | lst == []         = []
 | car == (head lst) = groupCarsHelper2 (tail lst) car
 | otherwise         = lst
 
 
theCar lst car = reverse (drop (length (afterCar (groupCars lst) car)) (reverse (removeBeforeCar lst car)))
 
removeBeforeCar lst car = drop (length (beforeCar (groupCars lst) car)) lst


--wrapper for since we always call car list on the same lane
generateLaneStates lane = generateLaneStatesHelper lane (carList lane)
-- generateStates for a lane
generateLaneStatesHelper lane cars
 | null cars  =[]
 | otherwise  = (carStates lane (carStatesHelper lane (head cars)) (head cars)) ++ generateLaneStatesHelper lane (tail cars)

carStates lst states car
 | null states                      = []
 | otherwise                        =((beforeCar (groupCars lst) car) ++ (head states) ++ (afterCar (groupCars lst) car)):(carStates lst (tail states) car)
 
--NEWLY ADDED
--ADDS LANE TO THE STATE IF IT DOES NOT APPEAR
--
generateLaneStates2 lane 
 | elem lane (generateLaneStates lane)   = generateLaneStates lane 
 | otherwise                           = lane:(generateLaneStates lane ) 
 
carStatesHelper lst car = refineCombinations (getCombinations (theCar lst car))



getLaneStates grid 
 |null grid   = []
 |otherwise   = generateLaneStates2 (head grid) : getLaneStates (tail grid)


generateGridStates grid states laneNum
 | null states    = []
 | otherwise       = (createGrid laneNum 0 grid grid (head states)) ++ (generateGridStates grid (tail states) (laneNum + 1))
 

--creates grid lane number on grid with lane state
createGrid laneNum currLane grid currGrid lane
 | null currGrid        = [] ++ createGrid laneNum  0 grid grid (tail lane)
 | null lane            = []
 | laneNum == currLane  = (head lane) : createGrid laneNum (currLane + 1) grid (tail currGrid) lane
 | otherwise            = (head currGrid) : createGrid laneNum (currLane + 1) grid (tail currGrid) lane

 
 
do_it grid = generateGridStates grid (getLaneStates grid) 0
 
-- groupGrids lst n currList
 -- | null lst = [[]]
 -- | curr == 6  =  [] : [groupGrids (tail lst) 0]
 -- | otherwise   =[(head lst)]++ [groupGrids (tail lst) (curr + 1)]

groupGrids n lst
 | null lst  = []
 | n > 0    = (take n lst) : (groupGrids n (drop n lst))
 
 
 
 
 
 
 
 
 
 
 
 
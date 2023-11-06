import Graphics.Gloss
import System.Random
import System.Random.Shuffle
import System.IO.Unsafe

mapWidth :: Int
mapWidth = 800

mapHeight :: Int
mapHeight = 600

stepsPerSec :: Int
stepsPerSec = 30

type Range t = (t, t)

xPosRange :: Range Float
xPosRange = (0,400)

yPosRange :: Range Float
yPosRange = (0,300)

massRange :: Range Mass
massRange = (0.1, 10)

radiusRange :: Range Radius
radiusRange = (1, 3)

-- in kg/m^3
densityRange :: Range Density
densityRange = (0.1, 500)

shuffleList :: [a] -> [a]
shuffleList xs = shuffle' xs (length xs) (mkStdGen (length xs)) 

genXPos :: Int -> [Float]
genXPos n = take n $ randomRs xPosRange (mkStdGen n)

genYPos :: Int -> [Float]
genYPos n = take n $ randomRs yPosRange (mkStdGen n)

genXYPos :: Int -> [Point]
genXYPos n = zip (shuffleList $ genXPos n) (genYPos n) 

genMass :: Int -> [Float]
genMass n = take n $ randomRs massRange (mkStdGen n)

genRadius :: Int -> [Float]
genRadius n = take n $ randomRs radiusRange (mkStdGen n)

genDensity :: Int -> [Float]
genDensity n = take n $ randomRs densityRange (mkStdGen n)

type Mass = Float
type Radius = Float
type Density = Float
type Direction = (Float, Float)
type Atom = (Point, Direction, Mass, Radius, Density, Color)
type Universe = [Atom]

mapSetting :: Display
mapSetting = InWindow "Window" (mapWidth, mapHeight) (0, 0)

universe :: Universe
universe = randomUniverse 35

-- TODO: colour change 
randomUniverse :: Int -> Universe
randomUniverse atoms = 
  consUniverse atoms (genXYPos atoms) (genMass atoms) (genRadius atoms) (genDensity atoms)

consUniverse :: Int -> [Point] -> [Mass] -> [Radius] -> [Density] -> Universe
consUniverse 0 _ _ _ _ = []
consUniverse n (p:ps) (m:ms) (r:rs) (d:ds) = 
  (p, (0,0), m, r, d, white) : consUniverse (n-1) ps ms rs ds 

drawUniverse :: Universe -> Picture
drawUniverse model
  = Pictures
  [translate xPos yPos $ color white $ circleSolid radius
  | ((xPos, yPos), _, _, radius, _, c) <- model]

updateUniverse :: vp -> Float -> Universe -> Universe
updateUniverse = undefined

updateAtom :: Atom -> Float -> Universe -> Atom
updateAtom = undefined

updateDirection :: Atom -> Universe -> Direction
updateDirection = undefined

updateColor :: Atom -> Atom
updateColor = undefined

mergeAtoms :: Atom -> Atom -> Atom
mergeAtoms = undefined

checkCollsion :: Universe -> Universe
checkCollsion = undefined

traceAtom :: Atom -> Picture
traceAtom = undefined

main :: IO ()
main = display mapSetting black (drawUniverse universe)
-- main = simulate mapSetting black stepsPerSec universe drawUniverse updateUniverse

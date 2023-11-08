import Graphics.Gloss
import System.Random ( mkStdGen, Random(randomRs) )
import System.Random.Shuffle ( shuffle' )
import Data.List (delete)

mapWidth :: Int
mapWidth = 800

mapHeight :: Int
mapHeight = 600

stepsPerSec :: Int
stepsPerSec = 30

type Range t = (t, t)

xPosRange :: Range Float
xPosRange = (-300,300)

yPosRange :: Range Float
yPosRange = (-200,200)

massRange :: Range Mass
massRange = (100, 1000)

-- radiusRange :: Range Radius
-- radiusRange = (1, 3)

densityRange :: Range Density
densityRange = (100, 300)

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

-- genRadius :: Int -> [Float]
-- genRadius n = take n $ randomRs radiusRange (mkStdGen n)

genDensity :: Int -> [Float]
genDensity n = take n $ randomRs densityRange (mkStdGen n)

type Mass = Float
type Radius = Float
type Volume = Float
type Density = Float
type Force = (Float, Float)
type Direction = (Float, Float)
type Atom = (Point, Direction, Mass, Radius, Density, Color)
type Universe = [Atom]

sphereVol :: Radius -> Volume
sphereVol r = ((4/3) * pi * r) ** 3

sphereRad :: Volume -> Radius
sphereRad v = ((3/4) * v / pi) ** (1/3)

mapSetting :: Display
mapSetting = InWindow "Window" (mapWidth, mapHeight) (0, 0)

universe :: Universe
universe = randomUniverse 1000

-- TODO: colour change 
randomUniverse :: Int -> Universe
randomUniverse atoms =
  consUniverse atoms (genXYPos atoms) (genMass atoms) (genDensity atoms)

consUniverse :: Int -> [Point] -> [Mass] -> [Density] -> Universe
consUniverse 0 _ _ _ = []
consUniverse n (p:ps) (m:ms) (d:ds) =
  (p, (0,0), m, r, d, white) : consUniverse (n-1) ps ms ds
    where r = sphereRad (m / d)

drawUniverse :: Universe -> Picture
drawUniverse model
  = Pictures
  [ translate pX pY $ color white $ circleSolid radius
  | ((pX, pY), _, _, radius, _, c) <- model ]

updateUniverse :: p -> Float -> Universe -> Universe
updateUniverse vp dt model = [updateAtom atom dt model | atom <- checkCollsion model]

updateAtom :: Atom -> Float -> Universe -> Atom
updateAtom = undefined

forceOnAtom :: Atom -> Universe -> Force
forceOnAtom = undefined

updateColor :: Atom -> Atom
updateColor = undefined

mergeAtoms :: [Atom] -> Atom
mergeAtoms [a] = a
mergeAtoms (a:b:abs) = mergeAtoms (mergeTwoAtoms a b : abs)

mergeTwoAtoms :: Atom -> Atom -> Atom
mergeTwoAtoms
  ((pX1, pY1), (vX1, vY1), m1, r1, d1, c1)
  ((pX2, pY2), (vX2, vY2), m2, r2, d2, c2)
  = ((nX, nY), (nVx, nVy), nM, nR, nD, nC)
    where
      (nX, nY, nC, nD)
        | r1 >= r2 = (pX1, pY1, c1, d1)
        | otherwise = (pX2, pY2, c2, d2)
      nVx = vX1 * m1 + vX2 * m2 / nM
      nVy = vY1 * m1 + vY2 * m2 / nM
      nM = m1 + m2
      nR = sphereRad (nM / nD)

deleteDupe :: [Atom] -> Universe -> Universe
deleteDupe as u = foldl (flip delete) u as

checkCollsion :: Universe -> Universe
checkCollsion [] = []
checkCollsion (x:xs) = 
  mergeAtoms (collides x (x:xs)) : checkCollsion (deleteDupe (collides x xs) xs)

collides :: Atom -> Universe -> [Atom]
collides ((pX1, pY1), (vX1, vY1), m1, r1, d1, c1) model =
  [ ((pX2, pY2), (vX2, vY2), m2, r2, d2, c2)
  | ((pX2, pY2), (vX2, vY2), m2, r2, d2, c2) <- model,
  (^2) (abs (pX1 - pX2)) + (^2) (abs (pY1 - pY2)) <= (^2) (r1 + r2)]

traceAtom :: Atom -> Picture
traceAtom = undefined

main :: IO ()
main = display mapSetting black (drawUniverse universe)
-- main = simulate mapSetting black stepsPerSec universe drawUniverse updateUniverse

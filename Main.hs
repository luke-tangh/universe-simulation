import Graphics.Gloss
import System.Random ( mkStdGen, Random(randomRs) )
import System.Random.Shuffle ( shuffle' )
import Data.List (delete)


type Range t = (t, t)
type Mass = Float
type Radius = Float
type Volume = Float
type Density = Float
type Force = (Float, Float)
type Direction = (Float, Float)
type Atom = (Point, Direction, Mass, Radius, Density, Color)
type Universe = [Atom]

type Star = Atom
type Planet = Atom
type Comet = Atom


-- width of the display in pixels
mapWidth :: Int
mapWidth = 1280

-- height of the display in pixels
mapHeight :: Int
mapHeight = 720

-- simulation steps per second
stepsPerSec :: Int
stepsPerSec = 30

-- max move per iteration of an Atom
maxMove :: Float
maxMove = 5.0

-- max merge atom move per iteration
maxMergeMove :: Float
maxMergeMove = 2.5

-- size of the universe
-- dramatically influences performance!
sizeUni :: Int
sizeUni = 300


-- random generation ranges
xPosRange :: Range Float
xPosRange = (-600, 600)

yPosRange :: Range Float
yPosRange = (-320, 320)

xVRange :: Range Float
xVRange = (-2, 2)

yVRange :: Range Float
yVRange = (-1.5, 1.5)

massRange :: Range Mass
massRange = (400, 4000)

-- radiusRange :: Range Radius
-- radiusRange = (1, 3)

densityRange :: Range Density
densityRange = (100, 300)


-- random generators
genXPos :: Int -> [Float]
genXPos n = take n $ randomRs xPosRange (mkStdGen n)

genYPos :: Int -> [Float]
genYPos n = take n $ randomRs yPosRange (mkStdGen n)

genXYPos :: Int -> [Point]
genXYPos n = zip (shuffleList $ genXPos n) (genYPos n)

genXV :: Int -> [Float]
genXV n = take n $ randomRs xVRange (mkStdGen n)

genYV :: Int -> [Float]
genYV n = take n $ randomRs yVRange (mkStdGen n)

genXYV :: Int -> [Direction]
genXYV n = zip (shuffleList $ genXV n) (genYV n)

genMass :: Int -> [Mass]
genMass n = take n $ randomRs massRange (mkStdGen n)

-- genRadius :: Int -> [Radius]
-- genRadius n = take n $ randomRs radiusRange (mkStdGen n)

genDensity :: Int -> [Density]
genDensity n = take n $ randomRs densityRange (mkStdGen n)


-- utility functions
shuffleList :: [a] -> [a]
shuffleList xs = shuffle' xs (length xs) (mkStdGen (length xs))

sphereVol :: Radius -> Volume
sphereVol r = ((4/3) * pi * r) ** 3

sphereRad :: Volume -> Radius
sphereRad v = ((3/4) * v / pi) ** (1/3)


-- simulation
mapSetting :: Display
mapSetting = InWindow "Window" (mapWidth, mapHeight) (0, 0)

universe :: Universe
universe = randomUniverse sizeUni

randomUniverse :: Int -> Universe
randomUniverse atoms =
  consUniverse atoms (genXYPos atoms) (genXYV atoms) (genMass atoms) (genDensity atoms)

consUniverse :: Int -> [Point] -> [Direction] -> [Mass] -> [Density] -> Universe
consUniverse 0 _ _ _ _ = []
consUniverse n (p:ps) (v:vs) (m:ms) (d:ds) =
  (p, v, m, r, d, c) : consUniverse (n-1) ps vs ms ds
    where 
      r = sphereRad (m / d)
      c = makeColor 0.5 0.5 (1 - d') 1
      d' = (d - fst densityRange) / (snd densityRange - fst densityRange)

drawUniverse :: Universe -> Picture
drawUniverse model
  = Pictures
  [ translate pX pY $ color c $ circleSolid radius
  | ((pX, pY), _, _, radius, _, c) <- model ]

updateUniverse :: p -> Float -> Universe -> Universe
updateUniverse vp dt model = 
  [ updateAtom atom dt model 
  | atom <- checkCollsion $ deleteEscape 
    (model ++ randomUniverse (sizeUni - length model))]

updateAtom :: Atom -> Float -> Universe -> Atom
updateAtom ((pX, pY), (vX, vY), m, r, d, c) dt model
  = ((nX, nY), (nVx, nVy), m, r, d, c)
    where
      -- nX = max (-400) (min 400 (pX + nVx))
      -- nY = max (-300) (min 300 (pY + nVy))
      nX = pX + nVx
      nY = pY + nVy
      nVx = max (-maxMove) (min maxMove (vX + aX * dt))
      nVy = max (-maxMove) (min maxMove (vY + aY * dt))
      -- nVx = vX + aX * dt
      -- nVy = vY + aY * dt
      (aX, aY) = (fX / m, fY / m)
      fX = sum $ map fst forces
      fY = sum $ map snd forces
      forces = forceOnAtom (pX, pY) m model

forceOnAtom :: Point -> Mass -> Universe -> [Force]
forceOnAtom p1 m1 model 
  = [ forceTwoAtoms p1 m1 p2 m2 | (p2, _, m2, _, _, _) <- model]

forceTwoAtoms :: Point -> Mass -> Point -> Mass -> Force
forceTwoAtoms (pX1, pY1) m1 (pX2, pY2) m2 
  | pX1 == pX2 && pY1 == pY2 = (0, 0)
  | pX1 <= pX2 && pY1 <= pY2 = (f * cos a, f * sin a)
  | pX1 >= pX2 && pY1 <= pY2 = (-f * cos a, f * sin a)
  | pX1 >= pX2 && pY1 >= pY2 = (-f * cos a, -f * sin a)
  | pX1 <= pX2 && pY1 >= pY2 = (f * cos a, -f * sin a)
    where
      f = g * m1 * m2 / distSquare
      g = 0.0667
      a = atan (abs (pY2 - pY1) / abs (pX2 - pX1))
      distSquare = (pX2 - pX1) ** 2 + (pY2 - pY1) ** 2

{-
updateColor :: Atom -> Atom
updateColor (p, (vX, vY), m, r, d, c) 
  = (p, (vX, vY), m, r, d, nC)
    where
      nC = makeColor 0.5 0.5 v 1
      v = (abs vX + abs vY) / (2 * maxMove)
-}

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
      nVx = max (-maxMergeMove) (min maxMergeMove ((vX1 * m1 + vX2 * m2) / nM))
      nVy = max (-maxMergeMove) (min maxMergeMove ((vY1 * m1 + vY2 * m2) / nM))
      nM = m1 + m2
      nR = sphereRad (nM / nD)

deleteEscape :: Universe -> Universe
deleteEscape model = 
  [ ((pX, pY), (vX, vY), m, r, d, c) 
  | ((pX, pY), (vX, vY), m, r, d, c) <- model,
  pX >= (- fromIntegral mapWidth),
  pX <= fromIntegral mapWidth,
  pY >= (- fromIntegral mapHeight),
  pY <= fromIntegral mapHeight ]

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
  (pX1 - pX2) ** 2 + (pY1 - pY2) ** 2 <= (r1 + r2) ** 2]

main :: IO ()
main = simulate mapSetting black stepsPerSec universe drawUniverse updateUniverse

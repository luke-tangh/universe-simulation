import Graphics.Gloss
import System.Random

mapWidth :: Int
mapWidth = undefined

mapHeight :: Int
mapHeight = undefined

stepsPerSec :: Int
stepsPerSec = 30

-- in kg/m^3
densityRange :: (Density, Density)
densityRange = (0.1, 500)

massRange :: (Mass, Mass)
massRange = (0.1, 10)

type Mass = Float
type Radius = Float
type Density = Float
type Direction = (Float, Float)
type Atom = (Point, Direction, Mass, Radius, Density, Color)
type Universe = [Atom]

mapSetting :: Display
mapSetting = InWindow "Window" (mapWidth, mapHeight) (0, 0)

universe :: Universe
universe = undefined

randomUniverse :: Int -> Universe
randomUniverse = undefined

drawUniverse :: Universe -> Picture
drawUniverse = undefined

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
main = simulate mapSetting black stepsPerSec universe drawUniverse updateUniverse

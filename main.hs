import Graphics.Gloss
import System.Random

mapWidth :: Int
mapWidth = undefined

mapHeight :: Int
mapHeight = undefined

type Mass = Float
type Radius = Float
type Position = (Float, Float)
type Direction = (Float, Float)
type Atom = (Position, Direction, Mass, Radius, Color)
type Universe = [Atom]

randomUniverse :: Int -> Universe
randomUniverse = undefined

drawUniverse :: Universe -> Picture
drawUniverse = undefined

updateUniverse :: Universe -> Universe
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
main = undefined

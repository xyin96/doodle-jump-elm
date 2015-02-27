import Signal (..)
import Mouse
import Time (..)
import Text (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Window
import Random

-- Inputs
type Input = Click | MouseMove (Int, Int)

delta = map inSeconds (fps 60)

input : Signal Input
input = sampleOn delta (merge (map MouseMove Mouse.position) (map (always Click) Mouse.clicks))

-- Model
(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

type alias Game = { x : Float, y : Float, vx : Float, vy : Float, height : Float, gameOver : Bool, platforms : List Platform, time : Int }
type alias Platform = { x : Float, y : Float, width : Float, ay : Float, action : Int }

game = foldp update { time = 0, x = 0, y = 0, vx = 0, vy = 5, height = 0, gameOver = False, platforms = [{ x = 0, y = 0, width = 100, ay =  0, action = 1 }, { x = 150, y = 10, width = 100, ay = 100, action = 0 },  { x = -150, y = -100, width = 100, ay = -100, action = 0 }, { x = 0, y = 0, width = 100, ay =  200, action = 0 }, { x = 0, y = 0, width = 100, ay =  250, action = 0 }] } input

-- Update


collision : Game -> Bool
collision game = collisionAux game.platforms game
  
collisionAux : List Platform -> Game -> Bool
collisionAux platforms game = 
  case platforms of
    platform :: rest -> game.vy < 0 && game.x < platform.x + platform.width / 2 && game.x > platform.x - platform.width / 2 && game.y < platform.y + 20 && game.y > platform.y + 5 || (collisionAux rest game)
    [] -> False
  
  
updatePlatforms : Game -> List Platform -> List Platform
updatePlatforms game platforms  =
  case platforms of 
    platform :: rest -> (updatePlatform game platform) :: (updatePlatforms game rest)
    [] -> []
    
updatePlatform : Game -> Platform -> Platform
updatePlatform game platform = 
  let (rand, seed) = (Random.generate (Random.float (-78.125) 78.125) (Random.initialSeed game.time))
      (rand2, seed2) = (Random.generate (Random.float (-1 * halfWidth) halfWidth) (Random.initialSeed (round (toFloat game.time * rand))))
      (rand3, seed3) = (Random.generate (Random.int 0 10) (Random.initialSeed game.time))
      newAY = if | (platform.ay - game.height) < -1 * halfHeight -> game.height + halfHeight
                 | otherwise -> platform.ay in
  { platform | ay <- newAY,
               y <- newAY - game.height,
               x <- if | (platform.ay - game.height) < -1 * halfHeight -> rand2
                       | platform.action <= 6 -> platform.x
                       | platform.action <= 10 -> platform.x + (cos (toFloat game.time / 25)) * 2
                       | otherwise -> platform.x,
               action <- if | (platform.ay - game.height) < -1 * halfHeight -> rand3
                            | otherwise -> platform.action}
      

update : Input -> Game -> Game
update input game = 
  case input of
    MouseMove (mouseX, mouseY) ->  { game |
        x <- game.x - game.vx,
        y <- if | game.gameOver -> game.y
                | game.y < 50 -> game.y + game.vy
                | game.vy < 0 -> game.y + game.vy
                | otherwise -> game.y,
        vx <- (game.x - toFloat mouseX + halfWidth) / 5, 
        vy <- if | (collision game) -> 12.5
                 | otherwise -> game.vy - 0.5,
        height <- if | game.y < 50 -> game.height
                     | game.vy > 0 && game.y >= 50 -> game.height + game.vy
                     | otherwise -> game.height + game.vy,
        gameOver <- if | game.y < -1 * halfHeight -> True
                       | otherwise -> False,
        platforms <- (updatePlatforms game game.platforms),
        time <- game.time + 1
      }
    Click -> if | game.gameOver -> { game | x <- 0, y <- 0, vx <- 0, vy <- 10, height <- 0, gameOver <- False, platforms <- [{ x = 0, y = 0, width = 100, ay =  0, action = 1 }, { x = 150, y = 10, width = 100, ay = 100, action = 0 },  { x = -150, y = -100, width = 100, ay = -100, action = 0 }, { x = 0, y = 0, width = 100, ay =  200, action = 0 }, { x = 0, y = 0, width = 100, ay =  250, action = 0 }] }
                | otherwise -> game


-- Display
display : (Int, Int) -> Game -> Element
display (w, h) g = 
  let x = g.x
      y = g.y
      height = g.height in
  collage gameWidth gameHeight (List.append [
    rect gameWidth gameHeight |> filled (rgb 200 200 200),
    oval 15 15 |> filled (rgb 0 0 0) |> move (x, y),
    if | g.gameOver -> toForm (asText "GameOver") |> move (150,150)
       | otherwise -> toForm (asText height) |> move (150,150)
  ] (renderPlatforms g.platforms))
  |> container gameWidth gameHeight middle

renderPlatforms : List Platform -> List Form
renderPlatforms platforms = 
  case platforms of
    platform :: rest -> renderPlatform platform :: renderPlatforms rest
    [] -> []

renderPlatform : Platform -> Form
renderPlatform platform = rect platform.width 15 |> filled (rgb 15 15 15) |> move (platform.x, platform.y)

main = map2 display Window.dimensions game

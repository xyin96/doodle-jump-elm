import Signal (..)
import Mouse
import Time (..)
import Text (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Window

-- Inputs
type Input = Click | MouseMove (Int, Int)

delta = map inSeconds (fps 60)

input : Signal Input
input = sampleOn delta (merge (map MouseMove Mouse.position) (map (always Click) Mouse.clicks))

-- Model
(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

type alias Game = { x : Float, y : Float, vx : Float, vy : Float, height : Float, gameOver : Bool }
type alias Platform = { x : Float, y : Float, width : Float, ay : Float }

game = foldp update { x = 0, y = 0, vx = 0, vy = 5, height = 0, gameOver = False } input

platforms = [{ x = halfWidth, y = 0, width = 100, ay =  0 }, { x = halfWidth / 2, y = 10, width = 100, ay = 100 },  { x = halfWidth * 3 / 2, y = -100, width = 100, ay = -100 }]

-- Update


collision : Platform -> Game -> Game
collision platform bouncer = {
  x = bouncer.x,
  y = bouncer.y,
  vx = bouncer.vx,
  vy = if | bouncer.vy < 0 && bouncer.x < platform.x + platform.width && bouncer.x > platform.x && bouncer.y < platform.y + 15 && bouncer.y > platform.y -> 10
          | otherwise -> bouncer.vy,
  height = bouncer.height,
  gameOver = bouncer.gameOver}
  
  
updatePlatforms : Game -> List Platform -> List Platform
updatePlatforms game platforms =
  case platforms of 
    platform :: rest -> { platform | y <- platform.ay - game.height } :: updatePlatforms game rest
    [] -> []

update : Input -> Game -> Game
update input game = 
  case input of
    MouseMove (mouseX, mouseY) -> 
      List.foldr collision { 
        x = game.x - game.vx,
        y = if | game.y < 50 -> game.y + game.vy
               | game.vy < 0 -> game.y + game.vy
               | otherwise -> game.y,
        vx = (game.x - toFloat mouseX) / 5, 
        vy = game.vy - 0.5,
        height = if | game.y < 50 -> game.height
                    | game.vy > 0 && game.y >= 50 -> game.height + game.vy
                    | otherwise -> game.height + game.vy,
        gameOver = if | game.y < -1 * halfHeight -> True
                      | otherwise -> False
      } (updatePlatforms game platforms)
    Click -> if | game.gameOver -> { game | x <- halfWidth, y <- 0, vx <- 0, vy <- 10, gameOver <- False }
                | otherwise -> game


-- Display
display : (Int, Int) -> Game -> Element
display (w, h) g = 
  let x = g.x
      y = g.y
      height = g.height in
  collage gameWidth gameHeight (List.append [
    rect gameWidth gameHeight |> filled (rgb 200 200 200),
    oval 15 15 |> filled (rgb 0 0 0) |> move (x - toFloat w / toFloat 2, y),
    if | g.gameOver -> toForm (asText "GameOver") |> move (150,150)
       | otherwise -> toForm (asText height) |> move (150,150)
  ] (List.map renderPlatforms (updatePlatforms g platforms)))
  |> container w h middle

renderPlatforms : Platform -> Form
renderPlatforms platform = rect platform.width 15 |> filled (rgb 15 15 15) |> move (platform.x - toFloat halfWidth - platform.width / 2, platform.y)

main = map2 display Window.dimensions game

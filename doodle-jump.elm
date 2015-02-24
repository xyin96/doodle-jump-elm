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
type alias Input = { mouseX : Float, delta : Time }

delta = map inSeconds (fps 60)

input : Signal Input
input = sampleOn delta <| Input <~ map toFloat Mouse.x
                                 ~ delta

-- Model
(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

type alias Game = { bouncer : Bouncer, gameover : Bool }
type alias Bouncer = { x : Float, y : Float, vx : Float, vy : Float, height : Float }
type alias Platform = { x : Float, y : Float, width : Float }

character = foldp updateCharacter { x = 0, y = 0, vx = 0, vy = 5, height = 0 } input

platforms = [{ x = halfWidth, y = 0, width = 100 }, { x = halfWidth / 2, y = 10, width = 100 },  { x = halfWidth * 3 / 2, y = -100, width = 100 }]

-- Update

updatePlatform : Bouncer -> Platform -> Platform
updatePlatform bouncer platform = { x = platform.x, y = platform.y, width = platform.width }

collision : Platform -> Bouncer -> Bouncer
collision platform bouncer = {
  x = bouncer.x,
  y = bouncer.y,
  vx = bouncer.vx,
  vy = (if | bouncer.vy < 0 && bouncer.x < platform.x + platform.width && bouncer.x > platform.x && bouncer.y < platform.y + 15 && bouncer.y > platform.y -> 10
           | otherwise -> bouncer.vy),
  height = bouncer.height}

updateCharacter : Input -> Bouncer -> Bouncer
updateCharacter { mouseX, delta } bouncer = List.foldr collision { 
  x = bouncer.x - bouncer.vx,
  y = bouncer.y + bouncer.vy,
  vx = (bouncer.x - mouseX) / 5, 
  vy = bouncer.vy - 0.5,
  height = bouncer.height } platforms

--game : Signal Game
--game = foldp updatePlatforms character input

-- Display
display : (Int, Int) -> Bouncer -> Element
display (w, h) {x, y, vx, vy} = 
  collage gameWidth gameHeight (List.append [
    rect gameWidth gameHeight |> filled (rgb 200 200 200),
    oval 15 15 |> filled (rgb 0 0 0) |> move (x - toFloat w / toFloat 2, y),
    toForm (asText x) |> move (-50,-50)
  ] (List.map renderPlatforms platforms))
  |> container w h middle

renderPlatforms : Platform -> Form
renderPlatforms platform = rect platform.width 15 |> filled (rgb 15 15 15) |> move (platform.x - toFloat halfWidth - platform.width / 2, platform.y)

main = map2 display Window.dimensions character
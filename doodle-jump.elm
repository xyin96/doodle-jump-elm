import Signal (..)
import Mouse
import Time (..)
import Text (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Window

-- Inputs
type alias Input = { mouseX : Float, delta : Time }

delta = map inSeconds (fps 35)

input : Signal Input
input = sampleOn delta <| Input <~ map toFloat Mouse.x
                                 ~ delta

-- Model
(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

type alias Game = { bouncer : Bouncer, gameover : Bool }
type alias Bouncer = { x : Float, y : Float, vx : Float, vy : Float }

character : Bouncer
character = { x = 0, y = 0, vx = 0, vy = 5 }

-- Update
update : Input -> Bouncer -> Bouncer
update { mouseX, delta } bouncer = { x = bouncer.x - bouncer.vx, y = bouncer.y + bouncer.vy, vx = (bouncer.x - mouseX) / 5, vy = (if bouncer.vy > -10 then bouncer.vy - 1 else 10) }

game = foldp update character input

-- Display
display : (Int, Int) -> Bouncer -> Element
display (w, h) {x, y, vx, vy} = 
  collage gameWidth gameHeight [
    rect gameWidth gameHeight |> filled (rgb 200 200 200),
    oval 15 15 |> filled (rgb 0 0 0) |> move (x - toFloat w / toFloat 2, y)
  ]
  |> container w h middle


main = map2 display Window.dimensions game
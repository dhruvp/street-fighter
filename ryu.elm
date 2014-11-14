import Char
import Keyboard
import Window
import Debug


-- MODEL

type Attack =
    { level     : Int,
      timeLeft  : Float
    }

type Model =
    { x   : Float
    , y   : Float
    , vx  : Float
    , vy  : Float
    , dir : Direction
    , punch: Attack
    , kick: Attack
    , crouched: Bool
    }

data Direction = Left | Right

type Position =
    {
      x : Int,
      y : Int
    }

type Keys =
    {
      position  : Position,
      highPunch : Bool,
      highKick  : Bool
    }

ryu : Model
ryu =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , punch = {level = 0, timeLeft = 0}
    , kick = {level = 0, timeLeft = 0}
    , crouched = False
    }



-- UPDATE

step : (Float, Keys) -> Model -> Model
step (dt, keys) ryu =
    ryu
        |> punch keys
        |> punchPhysics dt
        |> kick keys
        |> kickPhysics dt
        |> gravity dt
        |> jump keys
        |> Debug.watch "ryu 1"
        |> walk keys
        |> physics dt
        |> Debug.watch "ryu"

jump : Keys -> Model -> Model
jump keys ryu =
    if  | keys.position.y > 0 && ryu.vy == 0 ->
            { ryu | vy <- 6.0 }
        | ryu.vy == 0 && ryu.y == 0 ->
            if keys.position.y < 0 then { ryu | crouched <- True} else { ryu | crouched <- False}
        | otherwise -> ryu

punch: Keys -> Model -> Model
punch keys ryu =
    if  | (keys.highPunch && ryu.punch.timeLeft == 0) ->
          {ryu | punch <- {level = 3, timeLeft = 20} }
        | otherwise -> ryu

punchPhysics: Float -> Model -> Model
punchPhysics dt ryu =
    if  | ryu.punch.timeLeft > 0 -> {ryu | punch <- {level = ryu.punch.level, timeLeft = ryu.punch.timeLeft - (Debug.watch "dt" dt)}}
        | otherwise -> {ryu | punch <- {level = 0, timeLeft = 0}}

kick: Keys -> Model -> Model
kick keys ryu =
  if  | (keys.highKick && ryu.kick.timeLeft == 0) ->
        {ryu | kick <- {level = 3, timeLeft = 30} }
      | otherwise -> ryu

kickPhysics: Float -> Model -> Model
kickPhysics dt ryu =
    if  | ryu.kick.timeLeft > 0 -> {ryu | kick <- {level = ryu.kick.level, timeLeft = ryu.kick.timeLeft - (Debug.watch "dt" dt)}}
        | otherwise -> {ryu | kick <- {level = 0, timeLeft = 0}}

gravity : Float -> Model -> Model
gravity dt ryu =
    { ryu |
        vy <- if ryu.y > 0 then ryu.vy - dt/4 else 0
    }

physics : Float -> Model -> Model
physics dt ryu =
    { ryu |
        x <- ryu.x + dt * ryu.vx,
        y <- max 0 (ryu.y + dt * ryu.vy)
    }

walk : Keys -> Model -> Model
walk keys ryu =
    { ryu |
        vx <- toFloat keys.position.x,
        dir <- if | keys.position.x < 0 -> Left
                  | otherwise  -> ryu.dir
    }

-- DISPLAY

display : (Int, Int) -> Model -> Element
display (w',h') ryu =
  let (w,h) = (toFloat w', toFloat h')

      verb = if | ryu.crouched -> "crouch"
                | ryu.punch.level == 1 -> "lowPunch"
                | ryu.punch.level == 2 -> "mediumPunch"
                | ryu.punch.level == 3 -> "highPunch"
                | ryu.kick.level == 1 -> "lowKick"
                | ryu.kick.level == 2 -> "mediumKick"
                | ryu.kick.level == 3 -> "highKick"
                | ryu.y  >  0 -> "jump"
                | ryu.vx /= 0 -> "walk"
                | otherwise     -> "stand"

      a = Debug.watch "verb" verb

      dir = case ryu.dir of
              Left -> "left"
              Right -> "right"

      src  = "imgs/"++ verb ++ ".gif"
      --src  = "http://i188.photobucket.com/albums/z137/DreamsInMotion/Video%20Game%20Pictures/Metroid/samus-1.gif"

      ryuImage = image 100 100 src

      groundY = 100 - h/2
  in
      collage w' h'
          [ rect w h
              |> filled (rgb 174 238 238)
          , rect w 50
              |> filled (rgb 74 167 43)
              |> move (0, 24 - h/2)
          , ryuImage
              |> toForm
              |> Debug.trace "ryu"
              |> move (ryu.x, ryu.y + groundY)
          ]


-- SIGNALS


main : Signal Element
main = lift2 display Window.dimensions (foldp step ryu input)
input : Signal (Float, Keys)
input =
  let delta = lift (\t -> t/20) (fps 25)
      deltaArrows =
          lift2 (,)
            delta
            (lift3
              (\arrows punch kick -> { position = arrows, highPunch = punch, highKick = kick })
              Keyboard.arrows
              (Keyboard.isDown (Char.toCode 'F'))
              (Keyboard.isDown (Char.toCode 'C')))
  in
      sampleOn delta deltaArrows

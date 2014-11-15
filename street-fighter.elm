import Char
import Keyboard
import Window
import Debug


-- Model

type Model =
    {
      player1 : Character,
      player2 : Character
    }

type Rectangle =
    {
      height : Float,
      width  : Float
    }

type AttackDimensions =
    {
      xMin   : Float,
      xMax   : Float,
      yMin   : Float,
      yMax   : Float
    }

type KeyMap =
    {
      direction   : Direction,
      highPunch   : Char,
      mediumPunch : Char,
      lowPunch    : Char,
      highKick    : Char,
      mediumKick  : Char,
      lowKick     : Char
    }

playerOneKeyMap : KeyMap
playerOneKeyMap =
    {
      direction   = Right,
      highPunch   = 'F',
      mediumPunch = 'G',
      lowPunch    = 'H',
      highKick    = 'C',
      mediumKick  = 'V',
      lowKick     = 'B'
    }

playerTwoKeyMap : KeyMap
playerTwoKeyMap =
    {
      direction   = Left,
      highPunch   = 'P',
      mediumPunch = 'O',
      lowPunch    = 'I',
      highKick    = 'L',
      mediumKick  = 'K',
      lowKick     = 'J'
    }

type Attack =
    {
       name      : String,
       registered: Bool,
       timeLeft  : Float,
       weight    : Int,
       priority  : Int,
       attackBox : Rectangle
    }

type Character =
    { x   : Float
    , y   : Float
    , vx  : Float
    , vy  : Float
    , dir : Direction
    , attack: Attack
    , crouched: Bool
    , health: Int
    }

data Direction = Left | Right

type Position =
    {
      x : Int,
      y : Int
    }

type Keys =
    {
      position    : Position,
      highPunch   : Bool,
      mediumPunch : Bool,
      lowPunch    : Bool,
      highKick    : Bool,
      mediumKick  : Bool,
      lowKick     : Bool
    }

highPunch : Attack
highPunch = {
  name = "highPunch",
  timeLeft = 35,
  registered = False,
  weight = 10,
  priority =  3,
  attackBox = {
    height = 10,
    width = 80
  }}

mediumPunch : Attack
mediumPunch = {
  name = "mediumPunch",
  timeLeft = 20,
  registered = False,
  weight = 7,
  priority = 2,
  attackBox = {
    width = 50,
     height = 10
  }}

lowPunch : Attack
lowPunch = {
  name = "lowPunch",
  timeLeft = 10,
  registered = False,
  weight = 4,
  priority = 1,
  attackBox = {
    width = 70,
     height = 10
  }}

highKick : Attack
highKick = {
  name = "highKick",
  timeLeft = 40,
  registered = False,
  weight = 10,
  priority = 3,
  attackBox = {
    width = 80,
    height = 20
  }}

mediumKick : Attack
mediumKick = {
  name = "mediumKick",
  timeLeft = 20,
  registered = False,
  weight = 7,
  priority = 2,
  attackBox = {
    width = 70,
    height = 5
  }}

lowKick : Attack
lowKick = {
  name = "lowKick",
  timeLeft = 15,
  registered = False,
  weight = 4,
  priority = 1,
  attackBox = {
    width = 60,
    height = 5
  }}

nullAttack : Attack
nullAttack = {
  name = "None",
  timeLeft = 0,
  registered = True,
  weight = 0,
  priority = 0,
  attackBox = {
    width = 0,
    height = 0
  }}

isHitAttack : Attack
isHitAttack = {
  name = "isHit",
  timeLeft = 50,
  registered = True,
  weight = 0,
  priority = -1,
  attackBox = {
    width = 0,
    height = 0
  }}

ryu : Character
ryu =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , attack = nullAttack
    , crouched = False
    , health = 100
    }

worldState : Model
worldState =
    { player1 = ryu
    , player2 = (hFlip ryu)
    }

-- FLIP

hFlip: Character -> Character
hFlip  ryu = if  | ryu.dir == Right -> {ryu | dir <- Left}
                 | otherwise -> {ryu | dir <- Right}


-- UPDATE

step : (Float, Keys, Keys) -> Model -> Model
step (dt, playerOneKeys, playerTwoKeys) worldState =
    let newPlayer1 = stepCharacter worldState.player1 worldState.player2 dt playerOneKeys playerTwoKeys
        newPlayer2 = stepCharacter worldState.player2 worldState.player1 dt playerTwoKeys playerOneKeys
    in
      {worldState | player1 <- newPlayer1, player2 <- newPlayer2}

stepCharacter : Character -> Character -> Float -> Keys -> Keys -> Character
stepCharacter myCharacter opponentCharacter dt myKeys opponentKeys =
    myCharacter
        |> (chooseAttack myKeys) dt
        |> respondToHit opponentKeys myKeys opponentCharacter
        |> gravity dt
        |> jump myKeys
        |> walk myKeys
        |> physics dt
        |> Debug.watch "ryu"

jump : Keys -> Character -> Character
jump keys ryu =
    if  | keys.position.y > 0 && ryu.vy == 0 ->
            { ryu | vy <- 6.0 }
        | ryu.vy == 0 && ryu.y == 0 ->
            if keys.position.y < 0 then { ryu | crouched <- True} else { ryu | crouched <- False}
        | otherwise -> ryu

updateAttackTime : Attack -> Float -> Attack
updateAttackTime attack dt = {attack | timeLeft <- max 0 attack.timeLeft - dt, registered <- True}

attack attackRecord keys dt ryu =
    if  | ryu.attack.timeLeft <= 0 ->
            {ryu | attack <- attackRecord}
        | ryu.attack.timeLeft > 0 ->
            {ryu | attack <- (updateAttackTime ryu.attack dt)}
        | otherwise ->
            ryu

{--}
chooseAttack keys = attack (getAttack keys) keys
--}

getAttack : Keys -> Attack
getAttack keys = if      | keys.highPunch ->  highPunch
                         | keys.mediumPunch ->  mediumPunch
                         | keys.lowPunch ->  lowPunch
                         | keys.highKick ->  highKick
                         | keys.mediumKick ->  mediumKick
                         | keys.lowKick ->  lowKick
                         | otherwise ->  nullAttack


getAttackDimensions : Character -> Rectangle -> AttackDimensions
getAttackDimensions character attackBox =
  if  | character.dir == Right -> {
          xMin = character.x,
          xMax = character.x + attackBox.width,
          yMin = character.y,
          yMax = character.y + attackBox.height
        }
      | otherwise -> {
          xMin = character.x - attackBox.width,
          xMax = character.x,
          yMin = character.y,
          yMax = character.y + attackBox.height
      }

inHitRadius : Character -> AttackDimensions -> Bool
inHitRadius character dimensions =
  if  | (character.x >= dimensions.xMin &&
        character.x <= dimensions.xMax &&
        character.y >= dimensions.yMin &&
        character.y <= dimensions.yMax) ->
          True
      | otherwise ->
          False

respondToHit : Keys -> Keys -> Character -> Character ->  Character
respondToHit opponentKeys playerKeys opponentCharacter myCharacter =
  let opponentAttack = (getAttack opponentKeys)
      myAttack = (getAttack playerKeys)
  in
    if  | not opponentAttack.registered &&
          opponentAttack.priority > myAttack.priority &&
          (inHitRadius myCharacter (getAttackDimensions opponentCharacter opponentAttack.attackBox)) ->
            {myCharacter | health <- myCharacter.health - opponentAttack.weight, attack <- isHitAttack}
        | otherwise ->
            myCharacter


gravity : Float -> Character -> Character
gravity dt ryu =
    { ryu |
        vy <- if ryu.y > 0 then ryu.vy - dt/4 else 0
    }

physics : Float -> Character -> Character
physics dt ryu =
    { ryu |
        x <- ryu.x + dt * ryu.vx,
        y <- max 0 (ryu.y + dt * ryu.vy)
    }

walk : Keys -> Character -> Character
walk keys ryu =
    { ryu |
        vx <- toFloat keys.position.x * 2
    }

-- DISPLAY

getGif ryu =
  let verb = if | ryu.crouched -> "crouch"
                | ryu.attack.timeLeft > 0 -> ryu.attack.name
                | ryu.y  >  0 -> "jump"
                | ryu.vx /= 0 -> "walk"
                | otherwise   -> "stand"
      dir = case ryu.dir of
        Left -> "left"
        Right -> "right"
  in "imgs/"++ verb ++ "/" ++ dir ++ "/" ++ verb ++ ".gif"

display : (Int, Int) -> Model -> Element
display (w',h') worldState =
  let (w,h) = (toFloat w', toFloat h')

      ryuImage1 = image 100 100 (getGif worldState.player1)
      ryuImage2 = image 100 100 (getGif worldState.player2)

      groundY = 100 - h/2
  in
      collage w' h'
          [ rect w h
              |> filled (rgb 174 238 238)
          , rect w 50
              |> filled (rgb 74 167 43)
              |> move (0, 24 - h/2)
          , ryuImage1
              |> toForm
              |> Debug.trace "ryu1"
              |> move (worldState.player1.x, worldState.player1.y + groundY)
          , ryuImage2
              |> toForm
              |> Debug.trace "ryu2"
              |> move (worldState.player2.x, worldState.player2.y + groundY)
          ]


-- SIGNALS


main : Signal Element
main = lift2 display
          Window.dimensions
          (foldp
            step worldState (input playerOneKeyMap playerTwoKeyMap))


mapPressesToKeys : Position -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Keys
mapPressesToKeys arrows highPunch mediumPunch lowPunch highKick mediumKick lowKick =
  {
    position = arrows,
    highPunch = highPunch,
    mediumPunch = mediumPunch,
    lowPunch = lowPunch,
    highKick = highKick,
    mediumKick = mediumKick,
    lowKick = lowKick
  }

getKeyStream : KeyMap -> Signal Keys
getKeyStream keyMap =
  lift7
    mapPressesToKeys
    (if keyMap.direction == Right then Keyboard.wasd else Keyboard.arrows)
    (Keyboard.isDown (Char.toCode keyMap.highPunch))
    (Keyboard.isDown (Char.toCode keyMap.mediumPunch))
    (Keyboard.isDown (Char.toCode keyMap.lowPunch))
    (Keyboard.isDown (Char.toCode keyMap.highKick))
    (Keyboard.isDown (Char.toCode keyMap.mediumKick))
    (Keyboard.isDown (Char.toCode keyMap.lowKick))

input : KeyMap -> KeyMap -> Signal (Float, Keys, Keys)
input pOneKeyMap pTwoKeyMap =
  let delta = lift (\t -> t/20) (fps 100)
      pOneKeys = getKeyStream playerOneKeyMap
      pTwoKeys = getKeyStream playerTwoKeyMap
      deltaKeyPresses = lift3 (,,) delta pOneKeys pTwoKeys
  in
      sampleOn delta deltaKeyPresses

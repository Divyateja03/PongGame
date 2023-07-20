import Graphics.Gloss.Interface.Pure.Game

data PongGame = PongGame
  { ballPosition :: (Float, Float)
  , ballVelocity :: (Float, Float)
  , paddlePosition :: Float
  , paddleVelocity :: Float
  , shouldExit :: Bool
  , message :: String
  , score :: Int
  }

initialState :: PongGame
initialState = PongGame
  { ballPosition = (0, 0)
  , ballVelocity = (100, 150)
  , paddlePosition = 0
  , paddleVelocity = 0,
   shouldExit = False
  ,score = 0
  }

render :: PongGame -> Picture
render game
  |shouldExit game = pictures [translate (-350) (-100) (color white (text (message game))),translate (-250) 100 (color white (text ("Score:" ++ show(score game))))]
  |otherwise =
        pictures
        [ ball, paddle ]
        where
                (bx, by) = ballPosition game
                ball = translate bx by (color ballColor (circleSolid ballRadius))
                ballColor = white
                ballRadius = 10
                paddle = translate (paddlePosition game) (-265) (color paddleColor (rectangleSolid 80 10))
                paddleColor = blue

handleEvent :: Event -> PongGame -> PongGame
handleEvent (EventKey (Char 'a') Down _ _) game = game { paddleVelocity = -200 }
handleEvent (EventKey (Char 'd') Down _ _) game = game { paddleVelocity = 200 }
handleEvent (EventKey (Char 'a') Up _ _) game = game { paddleVelocity = 0 }
handleEvent (EventKey (Char 'd') Up _ _) game = game { paddleVelocity = 0 }
handleEvent _ game = game

handleWallCollision :: (Float, Float) -> (Float, Float) -> (Float, Float)
handleWallCollision (x, y) (vx, vy)
      | y + vy > fromIntegral 900 / 2 = (vx, -vy)
      | x + vx < -fromIntegral 1000 / 2 || x + vx > fromIntegral 1000 / 2 = (-vx, vy)
      | otherwise = (vx, vy)


paddleWallCollision :: Float -> Float -> Float
paddleWallCollision x vx
      | x + vx < -fromIntegral 1100 / 2 || x + vx > fromIntegral 1100 / 2 = 0
      | otherwise = vx


paddleBallCollision :: PongGame -> PongGame
paddleBallCollision game = game {ballVelocity = newBallVel,score = newScore}
        where
          (bx, by) = ballPosition game
          (bvx, bvy) = ballVelocity game
          pp = paddlePosition game
          nx = snd (ballVelocity game)
          newBallVel =  if ((bx >= pp-40 && pp+40 >= bx) && by == -260) then (bvx,-bvy)
                        else (bvx,bvy)
          ny = snd (newBallVel)
          newScore = if(nx /= ny) then score game + 10 else score game

updatePosition :: Float -> PongGame -> PongGame
updatePosition dt game
  | snd (ballPosition game) < -fromIntegral 580 / 2 =  game {shouldExit  = True , message = "YOU LOST"}
  | otherwise =
    paddleBallCollision $ game { ballPosition = newBallPos, ballVelocity = newBallVel, paddlePosition = newPaddlePos, paddleVelocity = newPaddleVel}
   where
    (bx, by)   = ballPosition game
    (bvx, bvy) = ballVelocity game
    pv = paddleVelocity game
    pp = paddlePosition game
    newBallPos   = (bx + bvx * dt, by + bvy * dt)
    newBallVel   = handleWallCollision newBallPos (bvx, bvy)
    newPaddlePos = pp + pv * dt
    newPaddleVel = paddleWallCollision newPaddlePos pv


main :: IO ()
main = play window black 60 initialState render handleEvent updatePosition
  where
    window = InWindow "Pong Game" (800, 600) (100, 100)

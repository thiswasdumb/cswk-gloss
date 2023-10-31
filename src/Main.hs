--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started            (yeah i used this one to start off :D )    --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Graphics.Gloss
import Graphics.Gloss.Data.Picture 
import Graphics.Gloss.Data.Color 
import Graphics.Gloss.Juicy
--me going through trying to use GLUT and realizing nOPE
--import Graphics.UI.GLUT
--import Graphics.Rendering.OpenGL
import Data.IORef
--import Graphics.Rendering.OpenGL.GL.VertexSpec 

import Control.Monad (zipWithM_)

--------------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
--No time to comment all the code below, but i hope you enjoy my 3D render so far, I loved it and it was so much fun!!! <3
type Point3D = (Float, Float, Float)

data Rectangle3D = Rectangle3D
  { bottomFrontLeft :: Point3D
  , bottomFrontRight :: Point3D
  , bottomBackLeft :: Point3D
  , bottomBackRight :: Point3D
  , topFrontLeft :: Point3D
  , topFrontRight :: Point3D
  , topBackLeft :: Point3D
  , topBackRight :: Point3D
  } deriving Show

{- Adjust the x, y or z in this to change the camera view!! (might mess it up a bit though) -}
project :: Point3D -> (Float, Float)
project (x, y, z) = (x / (1 - z/500), (y - 140) / (1 - z/500))


drawLine3D :: Point3D -> Point3D -> Picture
drawLine3D p1 p2 = line [project p1, project p2]
 --Was gonna have it rotate, but became big pain in butt
rotatePoint3D :: Float -> Point3D -> Point3D
rotatePoint3D angle (x, y, z) = (x * cos a + z * sin a, y, -x * sin a + z * cos a)
  where a = angle * pi / 180 

drawRectangle3D :: Rectangle3D -> Float -> Float -> Float -> Picture
drawRectangle3D rect xOffset yOffset zOffset = pictures
  [ color (makeColor 0.2 0.1 0.1 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , bottomFrontRight rect
      , bottomBackRight rect
      , bottomBackLeft rect
      ]
  , color (makeColor 0.2 0.1 0.1 1) $ polygon $ map (project . addOffsets)
      [ topFrontLeft rect
      , topFrontRight rect
      , topBackRight rect
      , topBackLeft rect
      ]
  , color (makeColor 0.2 0.1 0.1 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , topFrontLeft rect
      , topBackLeft rect
      , bottomBackLeft rect
      ]
  , color (makeColor 0.2 0.1 0.1 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontRight rect
      , topFrontRight rect
      , topBackRight rect
      , bottomBackRight rect
      ]
  , color (makeColor 0.2 0.1 0.1 1) $ polygon $ map (project . addOffsets)
      [ bottomBackLeft rect
      , topBackLeft rect
      , topBackRight rect
      , bottomBackRight rect
      ]
  , color (makeColor 0.2 0.1 0.1 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , topFrontLeft rect
      , topFrontRight rect
      , bottomFrontRight rect
      ]
  , drawLine3D (addOffsets $ bottomFrontLeft rect) (addOffsets $ bottomFrontRight rect)
  , drawLine3D (addOffsets $ bottomFrontRight rect) (addOffsets $ bottomBackRight rect)
  , drawLine3D (addOffsets $ bottomBackRight rect) (addOffsets $ bottomBackLeft rect)
  , drawLine3D (addOffsets $ bottomBackLeft rect) (addOffsets $ bottomFrontLeft rect)
  , drawLine3D (addOffsets $ topFrontLeft rect) (addOffsets $ topFrontRight rect)
  , drawLine3D (addOffsets $ topFrontRight rect) (addOffsets $ topBackRight rect)
  , drawLine3D (addOffsets $ topBackRight rect) (addOffsets $ topBackLeft rect)
  , drawLine3D (addOffsets $ topBackLeft rect) (addOffsets $ topFrontLeft rect)
  , drawLine3D (addOffsets $ bottomFrontLeft rect) (addOffsets $ topFrontLeft rect)
  , drawLine3D (addOffsets $ bottomFrontRight rect) (addOffsets $ topFrontRight rect)
  , drawLine3D (addOffsets $ bottomBackRight rect) (addOffsets $ topBackRight rect)
  , drawLine3D (addOffsets $ bottomBackLeft rect) (addOffsets $ topBackLeft rect)
  ]
  where
    addOffsets (x, y, z) = (x + xOffset, y + yOffset, z + zOffset)

drawRectangle3DBlack :: Rectangle3D -> Float -> Float -> Float -> Picture
drawRectangle3DBlack rect xOffset yOffset zOffset = pictures
  [ color (makeColor 0.3 0.1 0 1) $ polygon $ map (project . addOffsets) 
      [ bottomFrontLeft rect
      , bottomFrontRight rect
      , bottomBackRight rect
      , bottomBackLeft rect
      ]
  , color (makeColor 0.3 0.1 0 1) $ polygon $ map (project . addOffsets)
      [ topFrontLeft rect
      , topFrontRight rect
      , topBackRight rect
      , topBackLeft rect
      ]
  , color (makeColor 0.3 0.1 0 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , topFrontLeft rect
      , topBackLeft rect
      , bottomBackLeft rect
      ]
  , color (makeColor 0.3 0.1 0 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontRight rect
      , topFrontRight rect
      , topBackRight rect
      , bottomBackRight rect
      ]
  , color (makeColor 0.3 0.1 0 1) $ polygon $ map (project . addOffsets)
      [ bottomBackLeft rect
      , topBackLeft rect
      , topBackRight rect
      , bottomBackRight rect
      ]
  , color (makeColor 0.3 0.1 0 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , topFrontLeft rect
      , topFrontRight rect
      , bottomFrontRight rect
      ]
  , drawLine3D (addOffsets $ bottomFrontLeft rect) (addOffsets $ bottomFrontRight rect)
  , drawLine3D (addOffsets $ bottomFrontRight rect) (addOffsets $ bottomBackRight rect)
  , drawLine3D (addOffsets $ bottomBackRight rect) (addOffsets $ bottomBackLeft rect)
  , drawLine3D (addOffsets $ bottomBackLeft rect) (addOffsets $ bottomFrontLeft rect)
  , drawLine3D (addOffsets $ topFrontLeft rect) (addOffsets $ topFrontRight rect)
  , drawLine3D (addOffsets $ topFrontRight rect) (addOffsets $ topBackRight rect)
  , drawLine3D (addOffsets $ topBackRight rect) (addOffsets $ topBackLeft rect)
  , drawLine3D (addOffsets $ topBackLeft rect) (addOffsets $ topFrontLeft rect)
  , drawLine3D (addOffsets $ bottomFrontLeft rect) (addOffsets $ topFrontLeft rect)
  , drawLine3D (addOffsets $ bottomFrontRight rect) (addOffsets $ topFrontRight rect)
  , drawLine3D (addOffsets $ bottomBackRight rect) (addOffsets $ topBackRight rect)
  , drawLine3D (addOffsets $ bottomBackLeft rect) (addOffsets $ topBackLeft rect)
  ]
  where
    addOffsets (x, y, z) = (x + xOffset, y + yOffset, z + zOffset)

drawRectangle3DWhite :: Rectangle3D -> Float -> Float -> Float -> Picture
drawRectangle3DWhite rect xOffset yOffset zOffset = pictures
  [ color (makeColor 0.9 0.7 0.4 1) $ polygon $ map (project . addOffsets) 
      [ bottomFrontLeft rect
      , bottomFrontRight rect
      , bottomBackRight rect
      , bottomBackLeft rect
      ]
  , color (makeColor 0.9 0.7 0.4 1) $ polygon $ map (project . addOffsets)
      [ topFrontLeft rect
      , topFrontRight rect
      , topBackRight rect
      , topBackLeft rect
      ]
  , color (makeColor 0.9 0.7 0.4 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , topFrontLeft rect
      , topBackLeft rect
      , bottomBackLeft rect
      ]
  , color (makeColor 0.9 0.7 0.4 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontRight rect
      , topFrontRight rect
      , topBackRight rect
      , bottomBackRight rect
      ]
  , color (makeColor 0.9 0.7 0.4 1) $ polygon $ map (project . addOffsets)
      [ bottomBackLeft rect
      , topBackLeft rect
      , topBackRight rect
      , bottomBackRight rect
      ]
  , color (makeColor 0.9 0.7 0.4 1) $ polygon $ map (project . addOffsets)
      [ bottomFrontLeft rect
      , topFrontLeft rect
      , topFrontRight rect
      , bottomFrontRight rect
      ]
  , drawLine3D (addOffsets $ bottomFrontLeft rect) (addOffsets $ bottomFrontRight rect)
  , drawLine3D (addOffsets $ bottomFrontRight rect) (addOffsets $ bottomBackRight rect)
  , drawLine3D (addOffsets $ bottomBackRight rect) (addOffsets $ bottomBackLeft rect)
  , drawLine3D (addOffsets $ bottomBackLeft rect) (addOffsets $ bottomFrontLeft rect)
  , drawLine3D (addOffsets $ topFrontLeft rect) (addOffsets $ topFrontRight rect)
  , drawLine3D (addOffsets $ topFrontRight rect) (addOffsets $ topBackRight rect)
  , drawLine3D (addOffsets $ topBackRight rect) (addOffsets $ topBackLeft rect)
  , drawLine3D (addOffsets $ topBackLeft rect) (addOffsets $ topFrontLeft rect)
  , drawLine3D (addOffsets $ bottomFrontLeft rect) (addOffsets $ topFrontLeft rect)
  , drawLine3D (addOffsets $ bottomFrontRight rect) (addOffsets $ topFrontRight rect)
  , drawLine3D (addOffsets $ bottomBackRight rect) (addOffsets $ topBackRight rect)
  , drawLine3D (addOffsets $ bottomBackLeft rect) (addOffsets $ topBackLeft rect)
  ]
  where
    addOffsets (x, y, z) = (x + xOffset, y + yOffset, z + zOffset)

initialRect :: Rectangle3D
initialRect = Rectangle3D
  { bottomFrontLeft = (-100, -10, -100)
  , bottomFrontRight = (100, -10, -100)
  , bottomBackLeft = (-100, 10, -100)
  , bottomBackRight = (100, 10, -100)
  , topFrontLeft = (-100, -10, 100)
  , topFrontRight = (100, -10, 100)
  , topBackLeft = (-100, 10, 100)
  , topBackRight = (100, 10, 100)
  }

initialRect2 :: Rectangle3D
initialRect2 = Rectangle3D
  { bottomFrontLeft = (-25, 1, -25)
  , bottomFrontRight = (25, 1, -25)
  , bottomBackLeft = (-25, 1, -25)
  , bottomBackRight = (25, 1, -25)
  , topFrontLeft = (-25, 1, 25)
  , topFrontRight = (25, 1, 25)
  , topBackLeft = (-25, 1, 25)
  , topBackRight = (25, 1, 25)
  }


rotateRectangle3D :: Float -> Rectangle3D -> Rectangle3D
rotateRectangle3D angle rect = rect
  { bottomFrontLeft = rotatePoint3D angle (bottomFrontLeft rect)
  , bottomFrontRight = rotatePoint3D angle (bottomFrontRight rect)
  , bottomBackLeft = rotatePoint3D angle (bottomBackLeft rect)
  , bottomBackRight = rotatePoint3D angle (bottomBackRight rect)
  , topFrontLeft = rotatePoint3D angle (topFrontLeft rect)
  , topFrontRight = rotatePoint3D angle (topFrontRight rect)
  , topBackLeft = rotatePoint3D angle (topBackLeft rect)
  , topBackRight = rotatePoint3D angle (topBackRight rect)
  }

main :: IO ()
main = simulate
  (InWindow "3D Rectangle" (800, 600) (10, 10)) (dim white) 20
  (initialRect, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2, initialRect2)
  (\(rect1, rect2, rect3, rect4, rect5, rect6, rect7, rect8, rect9, rect10, rect11, rect12, rect13, rect14, rect15, rect16, rect17) -> drawRectangles rect1 rect2 rect3 rect4 rect5 rect6 rect7 rect8 rect9 rect10 rect11 rect12 rect13 rect14 rect15 rect16 rect17)
  (\_ _ (rect1, rect2, rect3, rect4, rect5, rect6, rect7, rect8, rect9, rect10, rect11, rect12, rect13, rect14, rect15, rect16, rect17) -> (rect1, rect2, rect3, rect4, rect5, rect6, rect7, rect8, rect9, rect10, rect11, rect12, rect13, rect14, rect15, rect16, rect17))

drawRectangles :: Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Rectangle3D -> Picture
drawRectangles rect1 rect2 rect3 rect4 rect5 rect6 rect7 rect8 rect9 rect10 rect11 rect12 rect13 rect14 rect15 rect16 rect17 = pictures
  [ drawRectangle3D rect1 0 0 0
  , drawRectangle3DBlack rect2 (-75) 10 (-75)
  , drawRectangle3DWhite rect3 (-75) 10 (-25)
  , drawRectangle3DBlack rect4 (-75) 10 (25)
  , drawRectangle3DWhite rect5 (-75) 10 (75)
  , drawRectangle3DBlack rect6 (-25) 10 (-25)
  , drawRectangle3DWhite rect7 (-25) 10 (-75)
  , drawRectangle3DBlack rect8 (-25) 10 (75)
  , drawRectangle3DWhite rect9 (-25) 10 (25)
  , drawRectangle3DBlack rect10 (25) 10 (-75)
  , drawRectangle3DWhite rect11 (25) 10 (-25)
  , drawRectangle3DBlack rect12 (25) 10 (25)
  , drawRectangle3DWhite rect13 (25) 10 (75)
  , drawRectangle3DBlack rect14 (75) 10 (-25)
  , drawRectangle3DWhite rect15 (75) 10 (-75)
  , drawRectangle3DBlack rect16 (75) 10 (75)
  , drawRectangle3DWhite rect17 (75) 10 (25)
  , beanBlue (-20.5) 0 (-160)
  , beanBlue (-20.5) 0 (-100)
  , beanBlue (-20.5) 0 (-37.5)
  , beanBlue (-20.5) 0 (22.5)
  , beanRed (20.5) 0 (-160)
  , beanRed (20.5) 0 (-100)
  , beanRed (20.5) 0 (-37.5)
  , beanRed (20.5) 0 (22.5)
  ]

toPoint3D :: (Float, Float, Float) -> Point3D
toPoint3D (x, y, z) = (x, y - 20, -z)

drawLine3Dv2 :: [Point3D] -> Picture
drawLine3Dv2 points = line (map project points)

boundingBox :: Picture -> Maybe (Float, Float, Float)
boundingBox pic = case pic of
    Blank -> Nothing
    Polygon pts -> Just (minimum xs, minimum ys, maximum xs - minimum xs)
        where xs = map fst pts
              ys = map snd pts
    Line pts -> Just (minimum xs, minimum ys, maximum xs - minimum xs)
        where xs = map fst pts
              ys = map snd pts
    _ -> Nothing

beanVerts :: [Point3D]
beanVerts = [(0, 0, 0), (8, 0, -12), (16, 0, -16), (20, 0, 0), (20, 0, 24), (16, 0, 36), (8, 0, 40), (0, 0, 36), (0, 0, 24),
            (6, 0.5, -6), (12, 0.5, -12), (15, 0.5, 0), (15, 0.5, 20), (12, 0.5, 28), (6, 0.5, 32), (0, 0.5, 28), (0, 0.5, 20),
            (4, 1, -4), (8, 1, -8), (10, 1, 0), (10, 1, 16), (8, 1, 20), (4, 1, 22), (0, 1, 20), (0, 1, 16),
            (4, 1.5, -6), (8, 1.5, -12), (10, 1.5, 0), (10, 1.5, 20), (8, 1.5, 28), (4, 1.5, 32), (0, 1.5, 28), (0, 1.5, 20),
            (2, 1.65, -10), (4, 1.65, -14), (5, 1.65, 0), (5, 1.65, 6), (4, 1.65, 14), (2, 1.65, 18), (0, 1.65, 14), (0, 1.65, 6)]

beanVertsRev :: [Point3D]
beanVertsRev = [(0, 0, 0), (-8, 0, -12), (-16, 0, -16), (-20, 0, 0), (-20, 0, 24), (-16, 0, 36), (-8, 0, 40), (0, 0, 36), (0, 0, 24),
            (-6, 0.5, -6), (-12, 0.5, -12), (-15, 0.5, 0), (-15, 0.5, 20), (-12, 0.5, 28), (-6, 0.5, 32), (0, 0.5, 28), (0, 0.5, 20),
            (-4, 1, -4), (-8, 1, -8), (-10, 1, 0), (-10, 1, 16), (-8, 1, 20), (-4, 1, 22), (0, 1, 20), (0, 1, 16),
            (-4, 1.5, -6), (-8, 1.5, -12), (-10, 1.5, 0), (-10, 1.5, 20), (-8, 1.5, 28), (-4, 1.5, 32), (0, 1.5, 28), (0, 1.5, 20),
            (-2, 1.65, -10), (-4, 1.65, -14), (-5, 1.65, 0), (-5, 1.65, 6), (-4, 1.65, 14), (-2, 1.65, 18), (0, 1.65, 14), (0, 1.65, 6)]


scalePoint :: Float -> Point3D -> Point3D
scalePoint s (x, y, z) = (x*s, y*s, z*s)

beanBlue :: Float -> Float -> Float -> Picture
beanBlue xOffset yOffset zOffset = pictures $
  [color (makeColor 0 0.1 0.7 1) $ polygon $ map (projectv2 xOffset yOffset zOffset) beanVertsRev]
  ++ [color (makeColor 0 0.1 0.5 1) $ polygon $ map (projectv2 xOffset yOffset zOffset) bottomFace, color (makeColor 0 0.1 0.9 1) $ polygon $ map (projectv2 xOffset yOffset zOffset) topFace]
  ++ map (color (makeColor 0 0.1 0.9 1) . polygon . map (projectv2 xOffset yOffset zOffset)) [ [beanVertsRev !! i | i <- [8..15]], [beanVertsRev !! i | i <- [16..23]], [beanVertsRev !! i | i <- [24..31]] ]
  where bottomFace = [beanVertsRev !! i | i <- [0..7]]
        topFace = [beanVertsRev !! i | i <- [32, 31..24]]

beanRed :: Float -> Float -> Float -> Picture
beanRed xOffset yOffset zOffset = pictures $
  [color (makeColor 0.3 0 0 1) $ polygon $ map (projectv2 xOffset yOffset zOffset) beanVerts]
  ++ [color (makeColor 0.5 0 0 1) $ polygon $ map (projectv2 xOffset yOffset zOffset) bottomFace, color (makeColor 0 0.1 0.9 1) $ polygon $ map (projectv2 xOffset yOffset zOffset) topFace]
  ++ map (color (makeColor 0.7 0 0 1) . polygon . map (projectv2 xOffset yOffset zOffset)) [ [beanVerts !! i | i <- [8..15]], [beanVerts !! i | i <- [16..23]], [beanVerts !! i | i <- [24..31]] ]
  where bottomFace = [beanVerts !! i | i <- [0..7]]
        topFace = [beanVerts !! i | i <- [32, 31..24]]

cowVerts :: [Point3D]
cowVerts = [(-40, 0, -70), (40, 0, -70), (70, 0, -40), (70, 0, 40), (40, 0, 70), (-40, 0, 70), (-70, 0, 40), (-70, 0, -40), 
            (-30, 60, -50), (30, 60, -50), (50, 60, 0), (30, 60, 50), (-30, 60, 50), (-50, 60, 0), (-40, 20, -50), (40, 20, -50), 
            (50, 20, 0), (40, 20, 50), (-40, 20, 50), (-50, 20, 0), (-20, 80, -20), (20, 80, -20), (20, 80, 20), (-20, 80, 20), 
            (-20, 0, -70), (20, 0, -70), (20, 0, -90), (-20, 0, -90), (-60, 0, -50), (60, 0, -50), (60, 0, 50), (-60, 0, 50)]
            
cowFaces :: [[Int]]
cowFaces = [
    [0, 1, 2, 3, 4, 5, 6, 7], [0, 7, 8, 9, 10], [1, 2, 3, 4, 10, 11, 12, 13], [5, 6, 7, 14, 15],
    [0, 6, 15, 8], [1, 5, 14, 11], [2, 4, 12, 13], [9, 16, 15, 8],
    [10, 11, 12, 13, 14, 16, 15, 9]
  ]

cow :: Picture
cow = pictures [ color black $ polygon $ map ((projectv2 0 25 0) . (cowVerts !!)) face | face <- cowFaces ]


projectv2 :: Float -> Float -> Float -> Point3D -> (Float, Float)
projectv2 xOffset yOffset zOffset (x, y, z) =
  ((x + xOffset) / (1 - (z + zOffset)/500), ((y + yOffset - 140) / (1 - (z + zOffset)/500)))



--Below are all functions which worked at some point, and I wanted to keep incase I broke my code and wanted an older version


{- side view



rotatePoint3D angle (x, y, z) = (x * cos a - y * sin a, x * sin a + y * cos a, z)
  where a = angle * pi / 180

-}
{-
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "3D Rectangle"
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  clearColor $= Color4 0.2 0.2 0.2 1
  windowSize $= Size 800 600
  depthFunc $= Just Less
  viewport $= (Position 50 50, Size 1280 760)
  angle <- newIORef (0 :: GLfloat)
  displayCallback $= Main.display angle
  idleCallback $= Just (idle angle)
  mainLoop

display :: IORef GLfloat -> IO ()
display angle = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  a <- get angle
  Graphics.UI.GLUT.rotate a $ Vector3 0 1 0
  lineWidth $= 3.0
  renderPrimitive Quads $ do
    -- Front face
    Graphics.Rendering.OpenGL.color $ Color3 (0.2 :: GLfloat) 0.1 0
    vertex3 (-0.5) (-0.1) (0.5)
    vertex3 (-0.5)  (0.1) (0.5)
    vertex3  (0.5)  (0.1) (0.5)
    vertex3  (0.5) (-0.1) (0.5)
    -- Back face
    Graphics.Rendering.OpenGL.color $ Color3 (0.2 :: GLfloat) 0.1 0
    vertex3 (-0.5) (-0.1) (-0.5)
    vertex3 (-0.5)  (0.1) (-0.5)
    vertex3  (0.5)  (0.1) (-0.5)
    vertex3  (0.5) (-0.1) (-0.5)
    -- Top face
    Graphics.Rendering.OpenGL.color $ Color3 (0.6 :: GLfloat) 0.1 0
    vertex3 (-0.5) (0.1) (0.5)
    vertex3 (-0.5) (0.1) (-0.5)
    vertex3 (0.5) (0.1) (-0.5)
    vertex3 (0.5) (0.1) (0.5)
    -- Bottom face
    Graphics.Rendering.OpenGL.color $ Color3 (0.2 :: GLfloat) 0.1 0
    vertex3 (-0.5) (-0.1) (0.5)
    vertex3 (-0.5) (-0.1) (-0.5)
    vertex3 (0.5) (-0.1) (-0.5)
    vertex3 (0.5) (-0.1) (0.5)
      -- Left face
    Graphics.Rendering.OpenGL.color $ Color3 (0.2 :: GLfloat) 0.1 0
    vertex3 (-0.5) (-0.1) (0.5)
    vertex3 (-0.5) (0.1) (0.5)
    vertex3 (-0.5) (0.1) (-0.5)
    vertex3 (-0.5) (-0.1) (-0.5)
    -- Right face
    Graphics.Rendering.OpenGL.color $ Color3 (0.2 :: GLfloat) 0.1 0
    vertex3 (0.5) (-0.1) (0.5)
    vertex3 (0.5) (0.1) (0.5)
    vertex3 (0.5) (0.1) (-0.5)
    vertex3 (0.5) (-0.1) (-0.5)
  lineWidth $= 1.0
  Graphics.Rendering.OpenGL.color $ Color3 (0 :: GLfloat) 0 0
  lineWidth $= 5.0
  renderPrimitive Lines $ do
    -- Connecting lines
    vertex3 (-0.5) (-0.1) (0.5)
    vertex3 (-0.5) (-0.1) (-0.5)
    vertex3 (-0.5)  (0.1) (0.5)
    vertex3 (-0.5)  (0.1) (-0.5)
    vertex3  (0.5)  (0.1) (0.5)
    vertex3  (0.5)  (0.1) (-0.5)
    vertex3  (0.5) (-0.1) (0.5)
    vertex3  (0.5) (-0.1) (-0.5)
  swapBuffers

vertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3 x y z = vertex $ Vertex3 x y z

idle :: IORef GLfloat -> IO ()
idle angle = do
  a <- get angle
  angle $= a + 0.007
  postRedisplay Nothing

-}
{-
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "3D Rectangle"
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  clearColor $= Color4 0.2 0.2 0.2 1
  windowSize $= Size 800 600
  depthFunc $= Just Less
  viewport $= (Position 0 0, Size 800 600)
  angle <- newIORef (0 :: GLfloat)
  displayCallback $= Main.display angle
  idleCallback $= Just (idle angle)
  mainLoop

display :: IORef GLfloat -> IO ()
display angle = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  Graphics.Rendering.OpenGL.color $ Color3 (1 :: GLfloat) 1 1
  a <- get angle
  Graphics.UI.GLUT.rotate a $ Vector3 0 1 0
  renderPrimitive Quads $ do
    vertex $ Vertex3 (-0.5 :: GLfloat) (-0.5 :: GLfloat) (0.5 :: GLfloat)
    vertex $ Vertex3 (-0.5 :: GLfloat) (0.5 :: GLfloat) (0.5 :: GLfloat)
    vertex $ Vertex3 (0.5 :: GLfloat) (0.5 :: GLfloat) (0.5 :: GLfloat)
    vertex $ Vertex3 (0.5 :: GLfloat) (-0.5 :: GLfloat) (0.5 :: GLfloat)
    vertex $ Vertex3 (-0.5 :: GLfloat) (-0.5 :: GLfloat) (-0.5 :: GLfloat)
    vertex $ Vertex3 (-0.5 :: GLfloat) (0.5 :: GLfloat) (-0.5 :: GLfloat)
    vertex $ Vertex3 (0.5 :: GLfloat) (0.5 :: GLfloat) (-0.5 :: GLfloat)
    vertex $ Vertex3 (0.5 :: GLfloat) (-0.5 :: GLfloat) (-0.5 :: GLfloat)
  swapBuffers

idle :: IORef GLfloat -> IO ()
idle angle = do
  a <- get angle
  angle $= a + 0.01
  postRedisplay Nothing
-}
{-
type Point = (Float, Float)
type Point3 = (Float, Float, Float)

data Vertex = Vertex {x :: Float, y :: Float, z :: Float}
  deriving (Show)

main :: IO ()
main = animate FullScreen black renderScene

renderScene :: Float -> Picture
renderScene t = Graphics.Gloss.Data.Picture.rotate t $ drawCube cube 

drawCube :: [Point3] -> Picture
drawCube vs = Graphics.Gloss.Data.Picture.color red $ pictures $ map (\(p1, p2) -> drawLine vs (vs !! p1) (vs !! p2) 0) edges

uncurry3 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e 
uncurry3 f (a,b,c,d) = f a b c d

lineBetween3 :: Point3 -> Point3 -> Float -> Picture
lineBetween3 p1 p2 t = lineBetween [toTuple3 p1, toTuple3 p2] x1 y1 z1 x2 y2 z2
  where
    (x1, y1, z1) = toTuple3 p1
    (x2, y2, z2) = toTuple3 p2

drawLine :: [Point3] -> Point3 -> Point3 -> Float -> Picture
drawLine vs p1 p2 t = lineBetween3 p1 p2 t

toTuple :: Graphics.Gloss.Point -> (Float, Float)
toTuple (x, y) = (x, y)

toTuple3 :: Point3 -> (Float, Float, Float)
toTuple3 (x, y, z) = (x, y, z)

lineBetween :: [Point3] -> Float -> Float -> Float -> Float -> Float -> Float -> Picture
lineBetween vs x1 y1 z1 x2 y2 z2 = Graphics.Gloss.Data.Picture.color white (line [(x1, y1), (x2, y2)])

edges :: [(Int, Int)]
edges = [(0,1),(1,2),(2,3),(3,0),(4,5),(5,6),(6,7),(7,4),(0,4),(1,5),(2,6),(3,7)]

cube :: [Point3]
cube = [(-50, -50, -50), (50, -50, -50), (50, 50, -50), (-50, 50, -50), (-50, -50, 50), (50, -50, 50), (50, 50, 50), (-50, 50, 50)]

-}

--------------------------------------------------------------------------------
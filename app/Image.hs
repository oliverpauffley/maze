-- | parse images into pixel maps that can be used for masking mazes
module Image where

import           App               (Config, MazeBuilder)
import           Codec.Picture     (DynamicImage,
                                    Image (imageData, imageHeight, imageWidth),
                                    Pixel (PixelBaseComponent, pixelAt), Pixel8,
                                    PixelRGB8 (PixelRGB8), convertRGB8,
                                    dynamicPixelMap, generateImage, readPng)
import           Control.Monad.RWS
import           Mask              (killNodes)
import           Maze              (Maze, NodeID (NodeID), newMaze)

data ImageMask = ImageMask
  { size       :: Int,
    blackNodes :: [NodeID]
  }
  deriving (Eq, Show)

parseImage :: FilePath -> IO (Either String ImageMask)
parseImage path = do
  img <- readPng path
  return $ imageToMask <$> img

imageToMask :: DynamicImage -> ImageMask
imageToMask img = ImageMask (imageWidth (convertRGB8 img)) (blackPixels $ (imageToNode . convertRGB8) $ square img)
  where
    square = dynamicPixelMap squareImage
    blackPixels allPixels = [i | (i, p) <- allPixels, blackPixel p]

squareImage :: (Pixel a) => Image a -> Image a
squareImage img = generateImage (pixelAt img) edge edge
  where
    edge = min (imageWidth img) (imageHeight img)

imageToNode :: Image PixelRGB8 -> [(NodeID, PixelRGB8)]
imageToNode img = [(NodeID (x, y), pixelAt img x y) | x <- [0 .. (width - 1)], y <- [0 .. (height - 1)]]
  where
    width = imageWidth img
    height = imageWidth img

blackPixel :: PixelRGB8 -> Bool
blackPixel (PixelRGB8 r g b) = r + g + b < 30

maskToBlankMaze :: (Monoid w) => ImageMask -> MazeBuilder Config w Maze ()
maskToBlankMaze mask = do
  put maze
  killNodes (blackNodes mask)
  where
    maze = newMaze (size mask)

{-# LANGUAGE RecordWildCards #-}

-- | parse images into pixel maps that can be used for masking mazes
module Image where

import           App               (Config (Config), MazeBuilder, mask)
import           Codec.Picture     (DynamicImage,
                                    Image (imageHeight, imageWidth),
                                    Pixel (pixelAt), PixelRGB8 (PixelRGB8),
                                    convertRGB8, dynamicPixelMap, generateImage,
                                    readPng)
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
imageToNode img = [(toNodeID x y, pixelAt img x y) | x <- [0 .. (width - 1)], y <- [0 .. (height - 1)]]
  where
    width = imageWidth img
    height = imageWidth img
    toNodeID x y = NodeID (x, (height - 1) - y)

blackPixel :: PixelRGB8 -> Bool
blackPixel (PixelRGB8 r g b) = r + g + b < 30

maskToBlankMaze :: MazeBuilder Config Maze ()
maskToBlankMaze = do
  Config {..} <- ask
  case mask of
    Nothing -> pure ()
    Just path -> do
      Right imageMask <- liftIO $ parseImage path
      put (maze imageMask)
      killNodes (blackNodes imageMask)
  where
    maze m = newMaze (size m)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Mask nodes in the maze so that they cannot be used in the generation of mazes
module Mask where

-- import Codec.Picture (
--     DynamicImage,
--     Image (imageHeight, imageWidth),
--     Pixel (pixelAt),
--     PixelRGB8 (..),
--     convertRGB8,
--     dynamicPixelMap,
--     generateImage,
--     readPng,
--  )
-- import Control.Monad.RWS
-- import Data.Foldable (traverse_)
-- import Data.Functor.Rep (Representable (..))
-- import qualified Data.Map as Map
-- import MazeShape (
--     Config (..),
--     Maze,
--     MazeBuilder,
--     NodeID (..),
--     Opposite,
--     connections,
--     getNode,
--     removeNodes,
--  )
-- import MazeShape.Square (newSquareGrid)

-- killNode ::
--     (Representable d, Bounded (Rep d), Enum (Rep d), Opposite (Rep d), Eq (Rep d)) =>
--     NodeID ->
--     MazeBuilder (Maze d) ()
-- killNode i = do
--     m <- get
--     let node = getNode m i
--     -- clear this node from all neigbours
--     let neighbours = map snd $ connections node
--     traverse_ (modify' . removeNodes i) neighbours
--     -- clear from map
--     modify' $ Map.delete i

-- killNodes ::
--     (Traversable t, Representable d, Bounded (Rep d), Enum (Rep d), Opposite (Rep d), Eq (Rep d)) =>
--     t NodeID ->
--     MazeBuilder (Maze d) ()
-- killNodes = traverse_ killNode

-- data ImageMask = ImageMask
--     { size :: Int
--     , blackNodes :: [NodeID]
--     }
--     deriving (Eq, Show)

-- parseImage :: FilePath -> IO (Either String ImageMask)
-- parseImage path = do
--     img <- readPng path
--     return $ imageToMask <$> img

-- imageToMask :: DynamicImage -> ImageMask
-- imageToMask img = ImageMask (imageWidth (convertRGB8 img)) (blackPixels $ (imageToNode . convertRGB8) $ square img)
--   where
--     square = dynamicPixelMap squareImage
--     blackPixels allPixels = [i | (i, p) <- allPixels, blackPixel p]

-- -- TODO We are getting the pixels from an image and then using that to mask out spaces.
-- -- I think this could be done by having a homomorphism between the coordinate system in use and the square pixel coords
-- -- for hex we can use the algorithm described here: https://www.redblobgames.com/grids/hexagons/#pixel-to-hex

-- squareImage :: (Pixel a) => Image a -> Image a
-- squareImage img = generateImage (pixelAt img) edge edge
--   where
--     edge = min (imageWidth img) (imageHeight img)

-- imageToNode :: Image PixelRGB8 -> [(NodeID, PixelRGB8)]
-- imageToNode img = [(toNodeID x y, pixelAt img x y) | x <- [0 .. (width - 1)], y <- [0 .. (height - 1)]]
--   where
--     width = imageWidth img
--     height = imageWidth img
--     toNodeID x y = NodeID (x, (height - 1) - y)

-- blackPixel :: PixelRGB8 -> Bool
-- blackPixel (PixelRGB8 r g b) = r + g + b < 30

-- maskToBlankMaze :: MazeBuilder (Maze d) ()
-- maskToBlankMaze = do
--     Config{..} <- ask
--     case mask of
--         Nothing -> pure ()
--         Just path -> do
--             Right imageMask <- liftIO $ parseImage path
--             put (maze imageMask)
--             killNodes (blackNodes imageMask)
--   where
--     maze m = newSquareGrid (size m)

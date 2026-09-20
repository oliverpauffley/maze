{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Data (Proxy)
import DeadEnds (getDeadEnds)
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude hiding (Path, connect)
import Draw (mazeToDiagram)
import GridKind (GridKind (..), SomeGrid (..))
import MazeShape
import Options.Applicative
import Param
import Solve (findLongestRoute)

main :: IO ()
main = do
    customExecParser p opts >>= run
  where
    opts =
        info
            (config <**> helper)
            (fullDesc <> progDesc "Create and solve random mazes!")
    p = prefs showHelpOnEmpty

run :: Config -> IO ()
run cfg@Config{..} = do
    case shapeToGrid shape of
        SomeGrid (_ :: Proxy d) -> do
            let m = makeGrid @d $ mazeSize
            (_, maze) <- runBuilder (algorithmFun algorithm) cfg m
            -- TODO wire up solutions and dead ends
            solution <- Solve.findLongestRoute maze
            let deadEnds = getDeadEnds maze
                diagram = mazeToDiagram maze solution
            renderSVG fileName (mkWidth diagramSize) diagram

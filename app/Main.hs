{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)

import Lens.Micro.Mtl
import Lens.Micro.TH

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (
    Widget,
 )
import qualified Brick.Types as T
import Brick.Util (bg, clamp, fg, on)
import Brick.Widgets.Core (
    overrideAttr,
    str,
    updateAttrMap,
    (<+>),
    (<=>),
 )
import qualified Brick.Widgets.ProgressBar as P

import Brick.Main (renderWidget)

data MyAppState n = MyAppState {_x, _y, _z :: Float}

makeLenses ''MyAppState

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
  where
    -- use mapAttrNames
    whiteBar prog =
        updateAttrMap
            ( A.mapAttrNames
                [ (xDoneAttr, P.progressCompleteAttr)
                , (xToDoAttr, P.progressIncompleteAttr)
                ]
            )
            (bar prog)
    lbl c = Just $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v
    ui =
        (str "C: " <+> whiteBar 0.93)
            <=> str "Hit 'x', 'y', or 's' to advance progress, or 'q' to quit"

oldDrawUI :: MyAppState () -> [Widget ()]
oldDrawUI p = [ui]
  where
    -- use mapAttrNames
    whiteBar prog =
        updateAttrMap
            ( A.mapAttrNames
                [ (xDoneAttr, P.progressCompleteAttr)
                , (xToDoAttr, P.progressIncompleteAttr)
                ]
            )
            (bar prog)
    xBar =
        updateAttrMap
            ( A.mapAttrNames
                [ (xDoneAttr, P.progressCompleteAttr)
                , (xToDoAttr, P.progressIncompleteAttr)
                ]
            )
            $ bar
            $ _x p
    -- or use individual mapAttrName calls
    yBar =
        updateAttrMap
            ( A.mapAttrName yDoneAttr P.progressCompleteAttr
                . A.mapAttrName yToDoAttr P.progressIncompleteAttr
            )
            $ bar
            $ _y p
    -- or use overrideAttr calls
    zBar =
        overrideAttr P.progressCompleteAttr zDoneAttr $
            overrideAttr P.progressIncompleteAttr zToDoAttr $
                bar $
                    _z p

    lbl c = Just $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v
    ui =
        (str "X: " <+> xBar)
            <=> (str "Y: " <+> yBar)
            <=> (str "Z: " <+> zBar)
            <=> (str "C: " <+> whiteBar 0.93)
            <=> str "Hit 'x', 'y', or 's' to advance progress, or 'q' to quit"

appEvent :: T.BrickEvent () e -> T.EventM () (MyAppState ()) ()
appEvent (T.VtyEvent e) =
    let valid = clamp (0.0 :: Float) 1.0
     in case e of
            V.EvKey (V.KChar 'x') [] -> x %= valid . (+ 0.05)
            V.EvKey (V.KChar 'y') [] -> y %= valid . (+ 0.03)
            V.EvKey (V.KChar 'z') [] -> z %= valid . (+ 0.02)
            V.EvKey (V.KChar 'q') [] -> M.halt
            _ -> return ()
appEvent _ = return ()

initialState :: MyAppState ()
initialState = MyAppState 0.25 0.18 0.63

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

yDoneAttr, yToDoAttr :: A.AttrName
yDoneAttr = theBaseAttr <> A.attrName "Y:done"
yToDoAttr = theBaseAttr <> A.attrName "Y:remaining"

zDoneAttr, zToDoAttr :: A.AttrName
zDoneAttr = theBaseAttr <> A.attrName "Z:done"
zToDoAttr = theBaseAttr <> A.attrName "Z:remaining"

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (theBaseAttr, bg V.brightBlack)
        , (xDoneAttr, V.black `on` V.white)
        , (xToDoAttr, V.white `on` V.black)
        , (yDoneAttr, V.magenta `on` V.yellow)
        , (zDoneAttr, V.blue `on` V.green)
        , (zToDoAttr, V.blue `on` V.red)
        , (P.progressIncompleteAttr, fg V.yellow)
        ]

theApp :: M.App (MyAppState ()) e ()
theApp =
    M.App
        { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return ()
        , M.appAttrMap = const theMap
        }

region :: V.DisplayRegion
region = (30, 10)

qwe = do
    vty <- mkVty V.defaultConfig
    -- ctx <- V.displayContext _outp region
    -- ( V.outputPicture ctx ) renderWidget Nothing (drawUI initialState) region
    V.update vty $ renderWidget Nothing (drawUI initialState) region

main :: IO ()
main = void $ M.defaultMain theApp initialState

-- main = putStrLn "Hello, Haskell!"
-- main = qwe

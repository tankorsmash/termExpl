{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import Control.Monad.State (modify)
import qualified Data.Vector as Vec
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
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P

import Brick.Main (renderWidget)

-- data MyAppState n = MyAppState {_selectedIndex :: Int}
data MyAppState n = MyAppState {_nameList :: L.List WidgetName String }

data WidgetName = Name1 | Name2 | Name3 deriving (Ord, Show, Eq)

makeLenses ''MyAppState

drawUI :: MyAppState () -> [Widget WidgetName]
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
    -- ui =
    --     (str "C: " <+> whiteBar 0.93)
    --         <=> str "Hit 'x', 'y', or 's' to advance progress, or 'q' to quit"
    drawList isSelected element =
        let sel = if isSelected then " > " else "   "
         in str $ sel ++ element
    ui = L.renderList drawList True (_nameList p)

appEvent :: T.BrickEvent WidgetName e -> T.EventM WidgetName (MyAppState ()) ()
appEvent (T.VtyEvent e) =
    case e of
        -- V.EvKey (V.KChar 'j') [] -> selectedIndex .= 1
        -- V.EvKey (V.KChar 'k') [] -> selectedIndex .= 0
        V.EvKey (V.KChar 'q') [] -> M.halt
        evt -> T.zoom nameList $ L.handleListEventVi L.handleListEvent evt
appEvent _ = return ()

initialState :: MyAppState ()
initialState = MyAppState $ L.list Name1 (Vec.fromList ["james", "john", "jim", "jesus"]) 1

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

theApp :: M.App (MyAppState ()) e WidgetName
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

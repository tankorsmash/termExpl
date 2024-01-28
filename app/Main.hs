{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO (hFlush, stdout)

import Control.Monad (void)
import Control.Monad.State (liftIO, modify)
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
import qualified Brick.Widgets.Border as WB
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as WC
import Brick.Widgets.Core (
    overrideAttr,
    str,
    updateAttrMap,
    (<+>),
    (<=>),
 )
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.ProgressBar as P

import qualified Brick as V
import Brick.Main (renderWidget)
import Data.Maybe (fromMaybe)

data ProjectInfo = ProjectInfo
    { _projectName :: !String
    , _projectPath :: !FilePath
    , _projectDescription :: !String
    }

type ReturnType = [Char]

noProject :: ProjectInfo
noProject = ProjectInfo "No project selected" "" ""

projects :: [ProjectInfo]
projects =
    [ ProjectInfo "weekend" "/home/joshb/code_wsl/haskell/weekend" "Haskell Discord Bot"
    , ProjectInfo "uiua aoc" "/home/joshb/code_wsl/uiua/aoc_2023/" "Advent of Code 2023 in Uiua"
    , ProjectInfo "zig" "/home/joshb/code_wsl/zig/test_zigg/" "An investigation into Zig"
    , ProjectInfo "termExpl" "/home/joshb/code_wsl/haskell/termExpl/" "A project setup"
    ]

-- data MyAppState n = MyAppState {_selectedIndex :: Int}
newtype MyAppState n = MyAppState {_nameList :: L.List WidgetName ProjectInfo}

data WidgetName = Name1 | Name2 | Name3 deriving (Ord, Show, Eq)

makeLenses ''MyAppState
makeLenses ''ProjectInfo

drawList :: Bool -> ProjectInfo -> Widget WidgetName
drawList isSelected projectInfo =
    if isSelected
        then str " > " <+> V.withAttr selectedListElementAttr (str $ view projectName projectInfo)
        else str $ "   " ++ view projectName projectInfo

drawUI :: MyAppState ReturnType -> [Widget WidgetName]
drawUI p = [V.withBorderStyle BS.unicodeRounded ui]
  where
    progressPct = fromIntegral selectedIndex / fromIntegral totalEls
    ui =
        V.hBox
            [ listSelection
                -- <=> V.hBox [whiteBar progressPct, str "horiz text"]
                <=> V.hBox [whiteBar progressPct]
            ]
    -- use mapAttrNames
    whiteBar prog =
        updateAttrMap
            ( A.mapAttrNames
                [ (xDoneAttr, P.progressCompleteAttr)
                , (xToDoAttr, P.progressIncompleteAttr)
                ]
            )
            (bar prog)

    lbl c = Just $ flip (<>) "%" $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v

    theList = _nameList p
    formattedSelection =
        ( \projectInfo ->
            WB.border $
                (V.withAttr selectedProjectName . WC.hCenter) (str $ view projectName projectInfo)
                    <=> str " "
                    <=> V.withAttr theBaseAttr (str $ view projectPath projectInfo)
                    <=> str " "
                    <=> (V.withAttr selectedProjectDescription $ str $ view projectDescription projectInfo)
        )
            . snd
            <$> L.listSelectedElement theList

    selectedIndex = maybe (-1) fst $ L.listSelectedElement theList
    totalEls = length $ L.listElements theList
    listSelection =
        V.vLimitPercent 50 $
            (V.withAttr selectListAttr . WB.border $ L.renderList drawList True theList)
                <+> fromMaybe (str "None selected") formattedSelection

appEvent :: T.BrickEvent WidgetName e -> T.EventM WidgetName (MyAppState ReturnType) ()
appEvent (T.VtyEvent e) =
    case e of
        -- V.EvKey (V.KChar 'j') [] -> selectedIndex .= 1
        -- V.EvKey (V.KChar 'k') [] -> selectedIndex .= 0
        V.EvKey (V.KChar 'q') [] -> M.halt
        V.EvKey V.KEnter [] -> do
            M.halt
        evt -> T.zoom nameList $ L.handleListEventVi L.handleListEvent evt
appEvent _ = return ()

initialState :: MyAppState ReturnType
initialState = MyAppState $ L.list Name1 (Vec.fromList projects) 1

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

selectListAttr :: A.AttrName
selectListAttr = A.attrName "selectListAttr"
selectedListElementAttr = A.attrName "selectedListElementAttr"
selectedProjectName = A.attrName "selectedProjectName"
selectedProjectDescription = A.attrName "selectedProjectDescription"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (theBaseAttr, bg V.brightBlack)
        , (xDoneAttr, V.black `on` V.white)
        , (xToDoAttr, V.white `on` V.black)
        , (selectListAttr, V.black `on` V.white)
        , (selectedListElementAttr, V.black `on` V.brightCyan)
        , (selectedProjectName, V.withStyle (fg V.green) V.underline)
        , (selectedProjectDescription, V.style V.italic)
        , (P.progressIncompleteAttr, fg V.yellow)
        ]

theApp :: M.App (MyAppState ReturnType) e WidgetName
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

tempDrawUi = do
    vty <- mkVty V.defaultConfig
    V.update vty $ renderWidget Nothing (drawUI initialState) region

main :: IO ()
main = do
    appState <- M.defaultMain theApp initialState
    let myList = view nameList appState
    let projectInfo = maybe noProject snd $ L.listSelectedElement myList
    writeFile "/tmp/haskell_output" $ view projectPath projectInfo

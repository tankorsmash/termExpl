{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO (hFlush, stdout)

import Control.Monad (void)
import Control.Monad.State (modify, liftIO)
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

type ProjectPair = (String, FilePath, String)

noProject :: ProjectPair
noProject = ("No project selected", "", "")

projects :: [ProjectPair]
projects =
    [ ("weekend", "/home/joshb/code_wsl/haskell/weekend", "Haskell Discord Bot")
    , ("uiua aoc", "/home/joshb/code_wsl/uiua/aoc_2023/", "Advent of Code 2023 in Uiua")
    , ("zig", "/home/joshb/code_wsl/zig/test_zigg/", "An investigation into Zig")
    ]

-- data MyAppState n = MyAppState {_selectedIndex :: Int}
newtype MyAppState n = MyAppState {_nameList :: L.List WidgetName ProjectPair}

data WidgetName = Name1 | Name2 | Name3 deriving (Ord, Show, Eq)

makeLenses ''MyAppState

drawList :: Bool -> ProjectPair -> Widget WidgetName
drawList isSelected (projectName, projectPath, projectDescription) =
    if isSelected
        then V.withAttr theBaseAttr (str $ " > " ++ projectName)
        else str $ "   " ++ projectName

drawUI :: MyAppState () -> [Widget WidgetName]
drawUI p = [ui]
  where
    progressPct = fromIntegral selectedIndex / fromIntegral totalEls
    ui =
        V.hBox
            [ listSelection
                <=> V.hBox [whiteBar progressPct, str "horiz text"]
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
        ( \(projectName, projectPath, projectDescription) ->
            V.withAttr selectedProjectName (str projectName)
                <=> V.withAttr theBaseAttr (str projectPath)
                <=> V.withAttr selectedProjectDescription (str projectDescription)
        )
            . snd
            <$> L.listSelectedElement theList
    selectedIndex = maybe (-1) fst $ L.listSelectedElement theList
    totalEls = length $ L.listElements theList
    listSelection =
        V.vLimitPercent 50 $
            V.withAttr selectListAttr (L.renderList drawList True theList)
                <=> fromMaybe (str "None selected") formattedSelection


submitSelection :: ProjectPair -> T.EventM WidgetName (MyAppState ()) ()
submitSelection (projectName , projectPath, projectDescription) = do
    liftIO $ print projectName
    liftIO $ print projectName
    liftIO $ print projectName
    liftIO $ print projectName
    liftIO $ print projectName
    liftIO $ hFlush stdout
    liftIO $ print projectName
    liftIO $ hFlush stdout

appEvent :: T.BrickEvent WidgetName e -> T.EventM WidgetName (MyAppState ()) ()
appEvent (T.VtyEvent e) =
    case e of
        -- V.EvKey (V.KChar 'j') [] -> selectedIndex .= 1
        -- V.EvKey (V.KChar 'k') [] -> selectedIndex .= 0
        V.EvKey (V.KChar 'q') [] -> M.halt
        V.EvKey V.KEnter [] -> do
                myList <- use nameList
                let qwe  = maybe noProject snd $ L.listSelectedElement myList
                _ <- submitSelection qwe
                -- M.halt
                return ()
        -- V.EvKey V.KEnter [] -> submitSelection
        evt -> T.zoom nameList $ L.handleListEventVi L.handleListEvent evt
appEvent _ = return ()

initialState :: MyAppState ()
initialState = MyAppState $ L.list Name1 (Vec.fromList projects) 1

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

selectListAttr :: A.AttrName
selectListAttr = A.attrName "selectListAttr"
selectedProjectName = A.attrName "selectedProjectName"
selectedProjectDescription = A.attrName "selectedProjectDescription"

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
        , (selectListAttr, V.red `on` V.green)
        , (selectedProjectName, V.withStyle (fg V.blue) V.italic)
        , (selectedProjectDescription, V.style V.standout)
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

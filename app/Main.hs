{-# LANGUAGE TemplateHaskell #-}
module Main where
  
import Brick
import qualified Graphics.Vty as V

import Control.Lens
import Control.Monad
-- import Control.Concurrent
import Control.Arrow

import Data.Map as M
import Data.Maybe

data Bubble = Bubble
  { _text :: String
  , _children :: [Bubble]
  , _anchorPos :: (Int, Int)
  } deriving (Eq, Ord, Show)

data MindMap = MindMap
  { _parentBubbles :: [Bubble]
  } deriving (Eq, Ord, Show)
$(makeLenses ''MindMap)

data MindApp = MindApp
  { _saveState :: MindMap
  } deriving (Eq, Ord, Show)
$(makeLenses ''MindApp)

type Name = ()
type Event = ()

main :: IO ()
main = runMindApp newMindMap

runMindApp :: MindMap -> IO ()
runMindApp s = do
  let builder = V.mkVty V.defaultConfig
      appState = MindApp
        { _saveState = s
        }
  initialVty <- builder
  void $ customMain initialVty builder Nothing mindAppInstructions appState

newMindMap :: MindMap
newMindMap = MindMap []

mindAppInstructions :: App MindApp Event Name
mindAppInstructions = App { appDraw = return . drawMindMaps
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = mkAttrMap
          }

drawMindMaps :: MindApp -> Widget Name
drawMindMaps _ = collapseImage $ M.fromList [((1, 1), ('c', mempty))]

collapseImage :: M.Map (Int, Int) (Char, AttrName) -> Widget Name
collapseImage cs = vBox . fmap (hBox . fmap cell) $ rectangle x y
  where (x, y) = getImageBounds cs
        cell p = drawCell . fromMaybe (' ', mempty) . M.lookup p $ cs

rectangle :: Int -> Int -> [[(Int, Int)]]
rectangle x y = (sequence . fmap (,) $ [0 .. x]) <$> [0 .. y]

getImageBounds :: M.Map (Int, Int) a -> (Int, Int)
getImageBounds = M.foldrWithKey (return . onBoth max) (0, 0)

onBoth :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
onBoth f = uncurry (***) . join (***) f

drawCell :: (Char, AttrName) -> Widget Name
drawCell (c, attr) = withAttr attr $ str . return $ c

handleEvent :: BrickEvent Name Event -> EventM Name MindApp ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt
handleEvent _ = return ()

mkAttrMap :: MindApp -> AttrMap
mkAttrMap _ = attrMap (fg V.white) []

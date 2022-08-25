{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections, RankNTypes, OverloadedStrings #-}
module Main where

import Brick
import qualified Graphics.Vty as V

import Control.Lens (makeLenses, view, over, set, at, Lens', _1, _2, to, (^.))
import Control.Lens.Combinators (_Right)
import Control.Monad
-- import Control.Concurrent
import Control.Arrow
import System.Environment
import System.Directory
import qualified System.Console.Terminal.Size as Terminal
import Safe (lastMay, initMay, tailMay)

import Data.List.Extra ((!?))
import GHC.Data.Maybe (firstJust)

import Data.Aeson
import Data.Aeson.Types (Parser)

import Control.Monad.State.Class
import Control.Monad.IO.Class

import Data.Traversable
import qualified Data.Map as M
import Data.Maybe
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Semigroup

type Image = M.Map (Int, Int) (Char, AttrName)

data Preferences = Preferences
  { _colorfulMode :: Bool
  , _lightMode :: Bool
  , _showBorders :: Bool
  } deriving (Eq, Ord, Show)
$(makeLenses ''Preferences)

data Bubble = Bubble
  { _contents :: String
  , _anchorPos :: (Int, Int)
  , _children :: [Bubble]
  } deriving (Eq, Ord, Show)
$(makeLenses ''Bubble)

data BubbleFocus = BubbleFocus
  { _subPos :: (Int, Int)
  , _bubbleZip :: (Bubble, [Bubble])
  , _insertMode :: Bool
  } deriving (Eq, Ord, Show)
$(makeLenses ''BubbleFocus)

data MindMap = MindMap
  { _rootBubbles :: [Bubble]
  } deriving (Eq, Ord, Show)
$(makeLenses ''MindMap)

data MindApp = MindApp
  { _saveState :: MindMap
  , _focus :: Either (Int, Int) BubbleFocus
  , _screenOffset :: (Int, Int)
  , _keyStack :: String
  , _filename :: FilePath
  , _history :: [MindMap]
  , _future :: [MindMap]
  , _terminalSize :: (Int, Int)
  , _preferences :: Preferences
  , _displayCache :: Image
  } deriving (Eq, Ord, Show)
$(makeLenses ''MindApp)

type Name = ()
type Event = ()

main :: IO ()
main = do
  progname <- getProgName
  args <- getArgs
  if getBoolOpt "help" False args
    then putStrLn $ helpMessage progname
    else case getFileTarget args of
      Nothing -> putStrLn $ helpMessage progname
      Just fp -> runMindApp fp $ getPreferences args
      
redraw :: MonadState MindApp m => m ()
redraw = do
  st <- get
  modify $ set displayCache $
    shiftImage (join (***) (0-) $ st^.screenOffset) $
    foldr M.union M.empty $
    drawBubbleTree (st^.preferences.showBorders) <$> st^.saveState.rootBubbles

getPreferences :: [String] -> Preferences
getPreferences args = Preferences
  { _colorfulMode = getBoolOpt "colorful" False args
  , _lightMode = getBoolOpt "light-mode" False args
  , _showBorders = getBoolOpt "borders" True args
  }

getFileTarget :: [String] -> Maybe String
getFileTarget (cs:css) | take 2 cs == "--" = getFileTarget css
getFileTarget (fp:_) = Just fp
getFileTarget _ = Nothing

getBoolOpt :: String -> Bool -> [String] -> Bool
getBoolOpt cs = foldl $ flip parseBoolArg
  where parseBoolArg arg | arg == "--" <> cs = const True
                         | arg == "--no-" <> cs = const False
                         | otherwise = id

getOpts :: String -> [String] -> Maybe String
getOpts cs (arg:args) | "--" <> cs == argn = firstJust (getOpts cs args) (tailMay argv)
  where (argn, argv) = break (== '=') arg
getOpts cs (_:args) = getOpts cs args
getOpts _ _ = Nothing

helpMessage :: String -> String
helpMessage progname = unlines
  [ "Usage: " <> progname <> " FILENAME [OPTIONS]"
  , ""
  , "OPTIONS:"
  , "  --[no-]light-mode: Change to use a light background (default: off)"
  , "  --[no-]colorful: Fill bubbles with their color (default: off)"
  , "  --[no-]borders: Enable/Disable drawing borders (default: on)"
  , ""
  , "KEYBINDINGS:"
  , "  [hjkl]: move cursor"
  , "  enter: select bubble under cursor"
  , "  gn: create a new bubble"
  , "  g[hjkl]: move currently selected bubble"
  , "  G[hjkl]: as per g[hjkl] but also move all child bubbles"
  , "  i/a: enter insert mode while bubble is selected"
  , "  cc: Rewrite the current bubble"
  , "  escape: leave insert mode, or unselect bubble"
  , "  ZZ: save and quit"
  , "  ZQ: quit WITHOUT saving"
  ]

runMindApp :: FilePath -> Preferences -> IO ()
runMindApp fp pref = do
  fpExists <- doesFileExist fp
  s <-
    if fpExists
    then
      fromMaybe (error "file corrupted or not a mindmap file; try opening with a text editor")
        <$> decodeFileStrict fp
    else
      return newMindMap
  Terminal.Window sy sx <- fromMaybe (Terminal.Window 20 20) <$> Terminal.size
  let builder = V.mkVty V.defaultConfig
      appState = MindApp
        { _saveState = s
        , _focus = Left (sx `quot` 2, sy `quot` 2)
        , _screenOffset = (0, 0)
        , _keyStack = ""
        , _filename = fp
        , _history = []
        , _future = []
        , _terminalSize = (sx, sy)
        , _preferences = pref
        , _displayCache = M.empty
        }
  initialVty <- builder
  void $ customMain initialVty builder Nothing mindAppInstructions appState

mindAppInstructions :: App MindApp Event Name
mindAppInstructions = App
          { appDraw = return . drawMindMaps
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = redraw
          , appAttrMap = mkAttrMap
          }

drawMindMaps :: MindApp -> Widget Name
drawMindMaps st = collapseImage . fitImage (' ', mempty) (st^.terminalSize) $
  drawCursor st $
  st^.displayCache

handleEvent :: BrickEvent Name Event -> EventM Name MindApp ()
handleEvent (VtyEvent (V.EvKey k ms)) = handleKey k ms
handleEvent (VtyEvent (V.EvResize x y)) = modify $ set terminalSize (x, y)
handleEvent _ = return ()

mkAttrMap :: MindApp -> AttrMap
mkAttrMap st = attrMap defAttr . foldr (<>) [] $
  [ bubbleGroup
  , cursorGroup
  ] where
    defAttr = if st^.preferences.lightMode
                 then V.black `on` V.white
                 else fg V.white
    bubbleGroup = do
      variant <- ["bubble", "bubble-border"]
      (layer, color) <-
        [ (0 :: Int, V.blue)
        , (1, V.cyan)
        , (2, V.green)
        , (3, V.yellow)
        -- , (4, V.red)
        -- , (5, V.magenta)
        ]
      let style = if st^.preferences.colorfulMode
                    then on V.black
                    else fg
      return $ (attrName variant <> stimesMonoid layer (attrName "sub"), style color)
    cursorGroup = do
      (variant, color) <-
        [ (mempty, if st^.preferences.lightMode then V.black else V.white)
        , (attrName "insert", V.blue)
        , (attrName "selected", V.yellow)
        ]
      let opFgCol = if st^.preferences.lightMode then V.white else V.black
      return $ (attrName "cursor" <> variant, opFgCol `on` color)

newMindMap :: MindMap
-- newMindMap = MindMap
  -- [ Bubble "hello" (50, 25)
      -- [ Bubble "world" (40, 20) []
      -- , Bubble "haskell" (60, 20)
          -- [ Bubble "→ " (70, 17) []
          -- , Bubble "λ" (70, 23) []
          -- , Bubble "Brick" (55, 15)
              -- [ Bubble "Graphics.Vty" (40, 10) []
              -- , Bubble "TUI Graphics" (60, 10) []
              -- ]
          -- ]
      -- , Bubble "from the other side" (50, 30) []
      -- , Bubble "uwu" (36, 23) []
      -- , Bubble "hackathon" (35, 27)
          -- [ Bubble "o/" (24, 29) []
          -- ]
      -- ]
  -- , Bubble "yeet" (0, 0) []
  -- ]
newMindMap = MindMap []

{-------------------------------
 - JSON
 ------------------------------}

saveMindMap :: (MonadState MindApp m, MonadIO m) => m ()
saveMindMap = do
  unselect
  fp <- view filename <$> get
  st <- view saveState <$> get
  liftIO $ encodeFile fp st

fileVersion :: Int
fileVersion = 1

instance ToJSON MindMap where
  toJSON mindmap = object
    [ "version" .= fileVersion
    , "root-bubbles" .= (mindmap^.rootBubbles)
    ]

instance ToJSON Bubble where
  toJSON b = object
    [ "contents" .= (b^.contents)
    , "children" .= (b^.children)
    , "anchor-pos" .= (b^.anchorPos)
    ]

fromValue :: Value -> Maybe Object
fromValue (Object v) = Just v
fromValue _ = Nothing

instance FromJSON MindMap where
  parseJSON = withObject "MindMap" $ \v -> do
    version <- v .: "version" :: Parser Int
    case version of
      1 -> decodeMindMap1 v
      _ -> fail $ "File has been corrupted / version unknown."
                <> " Try looking at it as plain JSON for issues"

decodeMindMap1 :: Object -> Parser MindMap
decodeMindMap1 v = do
  decodedBs <- sequence . map decodeBubble1 =<< v .: "root-bubbles"
  return $ MindMap
    { _rootBubbles = decodedBs
    }

decodeBubble1 :: Value -> Parser Bubble
decodeBubble1 b = do
  case fromValue b of
    Just v -> do
      children' <- sequence . map decodeBubble1 =<< v .: "children"
      contents' <- v .: "contents"
      anchorPos' <- v .: "anchor-pos"
      return $ Bubble
        { _children = children'
        , _anchorPos = anchorPos'
        , _contents = contents'
        }
    Nothing -> fail "Corrupted child"

{-------------------------------
 - Motions
 ------------------------------}

handleKey :: V.Key -> [V.Modifier] -> EventM Name MindApp ()
-- handleKey (V.KChar 'c') [V.MCtrl] = halt
handleKey (V.KUp) [] = handleKChar 'k'
handleKey (V.KDown) [] = handleKChar 'j'
handleKey (V.KLeft) [] = handleKChar 'h'
handleKey (V.KRight) [] = handleKChar 'l'
handleKey (V.KChar 'r') [V.MCtrl] = handleKChar '\DC2'
handleKey (V.KChar c) [] = handleKChar c
handleKey (V.KChar c) [V.MShift] = handleKChar $ toUpper c
handleKey (V.KEnter) [] = do
  ins <- either (const False) (view insertMode) . view focus <$> get
  if ins
    then handleKChar '\n'
    else selectAtCursor
handleKey (V.KBS) [] = do
  focus' <- view focus <$> get
  case focus' of
    Right (sel@(BubbleFocus {_insertMode = True})) ->
      if sel^.subPos._1 > 0
      then do
        let backspaceOnLine =
              foldr (\(n, c) -> if n + 1 == sel^.subPos._1 then id else (c:)) []
              <<< mkIndices
        modify $ over (focus . _Right . bubbleZip . _1 . contents) $ 
          unlines
          <<< foldr (\(n, cs) -> if n == sel^.subPos._2 then (backspaceOnLine cs:) else (cs:)) []
          <<< mkIndices
          <<< lines
        modify $ over (focus . _Right . subPos . _1) (+ (-1))
      else do
        let combineLine bs (cs:css) = (bs <> cs):css
            combineLine bs [] = [bs]
        modify $ over (focus . _Right . bubbleZip . _1 . contents) $
          unlines
          <<< foldr (\(n, cs) -> if n + 1 == sel^.subPos._2 then combineLine cs else (cs:)) []
          <<< mkIndices
          <<< lines
    _ -> return ()
handleKey (V.KEsc) [] = do
  modify $ set keyStack []
  unselect
handleKey _ _ = return ()

handleKChar :: Char -> EventM Name MindApp ()
handleKChar c = do
  focus' <- view focus <$> get
  case focus' of
    Right (sel@(BubbleFocus {_insertMode = True})) -> do
      if sel^.bubbleZip._1.contents.to (length . lines) == 0
        then modify $ set (focus . _Right . bubbleZip . _1 . contents) [c]
        else do
          let p = sel^.subPos
              insertOnLine cs
                | length cs <= p^._1 = cs <> [c]
                | otherwise =  
                    foldr (\(n, c') -> if n == p^._1 then (c:) . (c':) else (c':)) []
                    . mkIndices $ cs
          modify $ over (focus . _Right . bubbleZip . _1 . contents) $
            unlines
            <<< foldr (\(n, cs) -> if n == p^._2 then (insertOnLine cs:) else (cs:)) []
            <<< mkIndices
            <<< lines
      modify $ over (focus . _Right . subPos) $
        if c == '\n'
          then const 0 *** (+1)
          else first (+1)
      redraw
    _ -> do
      modify $ over keyStack (c :) -- stored in REVERSE ORDER
      collapseKeyStack

handleMove :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
handleMove l n = modify $ over focus $ over l (+n) +++ over (subPos . l) (+n)

handleShift :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
handleShift l n = modify $ over (focus . _Right . bubbleZip . _1 . anchorPos . l) $ (+n)

handleShiftDeep :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
handleShiftDeep l n = modify $ over (focus . _Right . bubbleZip . _1) _handleShiftDeep
  where _handleShiftDeep = over (anchorPos . l) (+n)
                         . over children (fmap $ _handleShiftDeep)

moveScreen :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
moveScreen l n = modify $ over (screenOffset . l) (+n)

collapseKeyStack :: EventM Name MindApp ()
collapseKeyStack = do
    keyStack' <- view keyStack <$> get
    let action = case keyStack' of
          'n':'g':_ -> Just . Right $ do
            focus' <- view focus <$> get
            case focus' of
              Left p -> do
                let newBubble = Bubble
                      { _contents = ""
                      , _anchorPos = p
                      , _children = []
                      }
                modify $ set focus . Right $ BubbleFocus
                  { _subPos = (0, 0)
                  , _bubbleZip = (newBubble, [])
                  , _insertMode = True
                  }
              Right sel -> do
                let p = onBoth (+) (sel^.subPos)
                                   (sel^.bubbleZip._1.anchorPos)
                    newBubble = Bubble
                      { _contents = ""
                      , _anchorPos = p
                      , _children = []
                      }
                modify $ set focus . Right $ BubbleFocus
                  { _subPos = (0, 0)
                  , _bubbleZip = (newBubble, uncurry (flip (<>) . return) $ sel^.bubbleZip)
                  , _insertMode = True
                  }
            redraw
          'z':'z':_ -> Just . Left $ do
            p <- cursorTruePos
            sxy <- view terminalSize <$> get
            modify $ set screenOffset $ onBoth (-) p $
              join (***) (`quot` 2) sxy
            redraw
          'c':'c':_ -> Just . Right $ do
            modify $ set (focus . _Right . bubbleZip . _1 . contents) $ ""
            enterInsert
            redraw
          '\DC2':cs -> Just . Right $ motionLoop cs redo >> redraw
          'u':cs -> Just . Left $ motionLoop cs undo >> redraw
          'd':'d':_ -> Just . Right $ deleteFocused >> redraw
          'Q':'Z':_ -> Just . Left $ halt
          'Z':'Z':_ -> Just . Left $ saveMindMap >> halt
          'j':cs -> Just . Left $ handleMotion cs _2 1
          'k':cs -> Just . Left $ handleMotion cs _2 (-1)
          'h':cs -> Just . Left $ handleMotion cs _1 (-1)
          'l':cs -> Just . Left $ handleMotion cs _1 1
          'i':_ -> Just . Right $ enterInsert
          'a':_ -> Just . Right $ handleMotion [] _1 1 >> enterInsert
          _ -> Nothing
    case action of
      Just em -> do
        let m = either id (updateHistory >>) em
        m
        modify $ set keyStack ""
      Nothing -> return ()

redo :: MonadState MindApp m => m ()
redo = do
  fut <- view future <$> get
  case fut of
    (st:sts) -> do
      unselect
      modify $ set future sts
      modify $ set saveState st
    _ -> return ()

undo :: MonadState MindApp m => m ()
undo = do
  hist <- view history <$> get
  case hist of
    (st:sts) -> do
      unselect
      bs <- allBubbles
      curr <- set rootBubbles bs . view saveState <$> get
      modify $ over future (curr:)
      modify $ set saveState st
      modify $ set history sts
    [] -> return ()

allBubbles :: MonadState MindApp m => m [Bubble]
allBubbles = do
  bs <- view (saveState . rootBubbles) <$> get
  b <- either (const Nothing) (Just . unzipBubbles . view bubbleZip) . view focus <$> get
  return $ fromMaybe id ((:) <$> b) $ bs

cursorTruePos :: MonadState MindApp m => m (Int, Int)
cursorTruePos = do
  focus' <- view focus <$> get
  case focus' of
    Left p -> return p
    Right sel -> return $
      onBoth (+) (sel^.subPos) (sel^.bubbleZip._1.anchorPos)

updateHistory :: MonadState MindApp m => m ()
updateHistory = do
  st <- view saveState <$> get
  bs <- allBubbles
  modify $ over history (set rootBubbles bs st :)

deleteFocused :: MonadState MindApp m => m ()
deleteFocused = do
  focus' <- view focus <$> get
  case focus' of
    Left _ -> return ()
    Right sel -> do
      let notDeletedZip = sel^.bubbleZip._2
      case initMay &&& lastMay $ notDeletedZip of
        (Just bs, Just b) -> do
          modify $ set (focus . _Right . bubbleZip) (b, bs)
          unselect
        _ -> do
          let p = onBoth (+) (sel^.subPos) (sel^.bubbleZip._1.anchorPos)
          modify $ set focus $ Left p

enterInsert :: MonadState MindApp m => m ()
enterInsert = modify $ set (focus . _Right . insertMode) True

handleMotion :: MonadState MindApp m => String -> Lens' (Int, Int) Int -> Int -> m ()
handleMotion ('g':cs) l = (>> redraw) . motionLoop cs . handleShift l
handleMotion ('G':cs) l = (>> redraw) . motionLoop cs . handleShiftDeep l
handleMotion ('z':cs) l = (>> redraw) . motionLoop cs . moveScreen l
handleMotion cs l = motionLoop cs . handleMove l

motionLoop :: MonadState MindApp m => String -> m () -> m ()
motionLoop cs = fromMaybe id $
  (sequence_ .) . replicate <$> (readMaybe . reverse $ cs)

unselect :: MonadState MindApp m => m ()
unselect = do
  focus' <- view focus <$> get
  case focus' of
    Left _ -> return ()
    Right sel -> do
      if sel^.insertMode
        then modify $ set (focus . _Right . insertMode) False
        else do
          modify $ set focus . Left $ onBoth (+) (sel^.subPos) (sel^.bubbleZip._1.anchorPos)
          modify $ over (saveState . rootBubbles) $ (:) $ unzipBubbles (sel^.bubbleZip)

selectAtCursor :: MonadState MindApp m => m ()
selectAtCursor = do
  bs <- view (saveState . rootBubbles) <$> get
  (msieved, rest) <- sieveM inTreeUnderCursor bs
  ep <- view focus <$> get
  case (msieved, ep) of
    (Just zipper, Left p) -> do
      modify $ set (saveState . rootBubbles) rest
      modify $ set focus . Right $ BubbleFocus
        { _subPos = onBoth (-) p (zipper^._1.anchorPos)
        , _bubbleZip = zipper
        , _insertMode = False
        }
    _ -> return ()

inTreeUnderCursor :: MonadState MindApp m => Bubble -> m (Maybe (Bubble, [Bubble]))
inTreeUnderCursor b = do
  onPoint <- underCursor b
  if onPoint
    then return $ Just (b, [])
    else do
      (mchild, childs) <- sieveM inTreeUnderCursor $ b^.children
      case mchild of
        Nothing -> return Nothing
        Just (c, cs) -> do
          let b' = set children childs b
          return $ Just (c, b':cs)

underCursor :: MonadState MindApp m => Bubble -> m Bool
underCursor b = do
  target <- view focus <$> get
  case target of
    Left p -> do
      let p0 = b^.anchorPos
          p1 = onBoth (\x -> (+x) . max 1) p0
             <<< stringWidth &&& stringHeight $ b^.contents
      return $ (uncurry (&&) $ onBoth (>=) p p0)
              && (uncurry (&&) $ onBoth (<=) p p1)
    Right _ -> return False

unzipBubbles :: (Bubble, [Bubble]) -> Bubble
unzipBubbles = uncurry $ foldr (flip $ over children . (:))

{-------------------------------
 - Drawing Nodes
 -------------------------------}

fitImage :: a -> (Int, Int) -> M.Map (Int, Int) a -> M.Map (Int, Int) a
fitImage dflt p img = limitImage p img `M.union` M.fromList [(p, dflt)]

limitImage :: (Int, Int) -> M.Map (Int, Int) a -> M.Map (Int, Int) a
limitImage p = M.filterWithKey (return . uncurry (&&) . onBoth (>) p)

drawCursor :: MindApp -> Image -> Image
drawCursor st = case st^.focus of
                  Left p -> over (at $ onBoth (-) p $ st^.screenOffset) $ _drawCursor mempty
                  Right sel ->
                    let p = onBoth (+) (sel^.subPos) (sel^.bubbleZip._1.anchorPos)
                        a = attrName $ if sel^.insertMode
                                          then "insert"
                                          else "selected"
                        drawCursor' = over (at $ onBoth (-) p $ st^.screenOffset) (_drawCursor a)
                    in (drawCursor' .) 
                       <<< drawAt (join (***) (0-) $ st^.screenOffset)
                       <<< drawBubbleTree (st^.preferences.showBorders) 
                       <<< unzipBubbles $ sel^.bubbleZip
  where _drawCursor m Nothing = Just (' ', attrName "cursor" <> m)
        _drawCursor m (Just (c, _)) = Just (c, attrName "cursor" <> m)

drawBubbleTree :: Bool -> Bubble -> Image
drawBubbleTree showB b = foldl M.union (drawBubble showB 0 b) $
  drawBubbleBranch showB 1 (bubbleCenter b) <$> b^.children

drawBubbleBranch :: Bool -> Int -> (Int, Int) -> Bubble -> Image
drawBubbleBranch showB n p b = foldl M.union (drawChildBubble showB n p b) $
  drawBubbleBranch showB (n + 1) (bubbleCenter b) <$> b^.children

drawChildBubble :: Bool -> Int -> (Int, Int) -> Bubble -> Image
drawChildBubble showB n p b =
  M.union (drawBubble showB n b) (drawSteppedLine 2 '~' mempty p $ bubbleCenter b)

drawBubble :: Bool -> Int -> Bubble -> Image
drawBubble showB n b =
  shiftImage (b^.anchorPos)
  <<< (if showB
          then imageBorder (attrName "bubble-border" <> stimesMonoid n (attrName "sub"))
          else id)
  <<< imageString (attrName "bubble" <> stimesMonoid n (attrName "sub"))
  <<< (\cs -> if (foldr ((&&) . (== "")) True . lines $ cs) then " " else cs)
  $ b^.contents

bubbleCenter :: Bubble -> (Int, Int)
bubbleCenter b = onBoth (+) (b^.anchorPos)
                <<< join (***) (flip quot 2)
                <<< stringWidth &&& stringHeight $ b^.contents

{------------------------------
 - Underlying system
 ------------------------------}

stringWidth :: String -> Int
stringWidth = foldr (max . textWidth) 0 . lines

stringHeight :: String -> Int
stringHeight = length . lines

drawLine :: Char -> AttrName -> (Int, Int) -> (Int, Int) -> Image
drawLine = drawSteppedLine 1

drawSteppedLine :: Int -> Char -> AttrName -> (Int, Int) -> (Int, Int) -> Image
drawSteppedLine stepLen c a p p' = drawSparseLine len c a p p'
  where len = flip quot stepLen $ uncurry (+) $ join (***) abs $ onBoth (-) p' p

drawSparseLine :: Int -> Char -> AttrName -> (Int, Int) -> (Int, Int) -> Image
drawSparseLine len c a p p' = M.fromList $
    (, (c, a)) . onBoth (+) p . join (***) (flip roundedDiv len) <$>
    forM [0..len] (join (***) . (*)) dp
  where dp = onBoth (-) p' p

imageBorder :: AttrName -> Image -> Image
imageBorder a cs = fromMaybe cs $ do
  p <- join (***) (+ (-1)) <$> getImageMinBounds cs
  let p' = join (***) (+1) $ getImageBounds cs
  return $ M.union (drawBorder a p p') cs

drawBorder :: AttrName -> (Int, Int) -> (Int, Int) -> Image
drawBorder a p p' = foldr (M.union . M.map (\c -> (c, a))) M.empty $
  [ (uniformMap '─' $ (,) <$> [x0 + 1 .. x1 - 1] <*> [y0, y1])
  , (uniformMap '│' $ (,) <$> [x0, x1] <*> [y0 + 1 .. y1 - 1])
  , corners
  ] where corners = M.fromList
            [ ((x0, y0), '┌'), ((x1, y0), '┐')
            , ((x0, y1), '└'), ((x1, y1), '┘')
            ]
          (x0, y0) = onBoth min p p'
          (x1, y1) = onBoth max p p'


drawAt :: (Int, Int) -> Image -> Image -> Image
drawAt p = M.union . shiftImage p

shiftImage :: (Int, Int) -> Image -> Image
shiftImage = M.mapKeys . onBoth (+)

imageString :: AttrName -> String -> Image
imageString a = M.map (,a) . chartPositionsFilled ' ' . lines

uniformMap :: Ord k => a -> [k] -> M.Map k a
uniformMap a = M.fromList . fmap (,a)

collapseImage :: M.Map (Int, Int) (Char, AttrName) -> Widget Name
collapseImage cs = vBox . fmap (hBox . fmap cell) . rectangle (0, 0) $ getImageBounds cs
  where cell p = drawCell . fromMaybe (' ', mempty) . M.lookup p $ cs

rectangleBoundary :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
rectangleBoundary (x, y) (x', y') = hor <> sides
  where hor = (,) <$> [min x x' .. max x x'] <*> [y, y']
        sides = (,) <$> [x, x'] <*> [min y y' + 1 .. max y y' - 1]

rectangle :: (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
rectangle (x, y) (x', y') = (sequence . fmap (,) $ [x .. x']) <$> [y .. y']

drawCell :: (Char, AttrName) -> Widget Name
drawCell (c, attr) = withAttr attr $ str . return $ c

{---------------------------
 - Map functions
 ---------------------------}

getImageMinBounds :: M.Map (Int, Int) a -> Maybe (Int, Int)
getImageMinBounds = M.foldrWithKey _reduce Nothing
  where _reduce p _ (Just ps) = Just $ onBoth min p ps
        _reduce p _ Nothing = Just p

getImageBounds :: M.Map (Int, Int) a -> (Int, Int)
getImageBounds = M.foldrWithKey (const . onBoth max) (0, 0)

chartPositionsFilled :: a -> [[a]] -> M.Map (Int, Int) a
chartPositionsFilled d xss = M.fromList $ mapPos <$> positions
  where positions = (,) <$> [0..(foldr (max . length) 1 xss) - 1]
                        <*> [0..(length xss) - 1]
        mapPos (x, y) = ((x, y), fromMaybe d $ (xss !? y) >>= (!? x))

chartPositions :: [[a]] -> M.Map (Int, Int) a
chartPositions = M.fromList . join
               . fmap (uncurry $ \y -> fmap $ \(x, c) -> ((x, y), c))
               . fmap (second mkIndices)
               . mkIndices

{---------------------------
 - Generic functions
 ---------------------------}

sieveM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b, [a])
sieveM f (x:xs) = do
  result <- f x
  case result of
    Just b -> return $ (Just b, xs)
    Nothing -> second (x:) <$> sieveM f xs
sieveM _ [] = return (Nothing, [])

sieve :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
sieve f xs = sieveM (const . f) xs ()

onBoth :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
onBoth f = uncurry (***) . join (***) f

mkIndices :: Traversable t => t a -> t (Int, a)
mkIndices = snd . mapAccumL f 0
  where f n a = (n + 1, (n, a))

-- splitOn :: Eq a => a -> [a] -> [[a]]
-- splitOn a = uncurry (:) . foldr f ([], [])
  -- where f c (cs, css) | c == a = ([], cs:css)
                      -- | otherwise = (c:cs, css)

roundedDiv :: Int -> Int -> Int
roundedDiv n d = quot n d + (if (rem n d) * 2 >= d then 1 else 0)

{-# LANGUAGE TemplateHaskell, FlexibleContexts, TupleSections, RankNTypes #-}
module Main where

import Brick
import qualified Graphics.Vty as V

import Control.Lens (makeLenses, view, over, set, at, Lens', _1, _2)
import Control.Monad
-- import Control.Concurrent
import Control.Arrow

import Control.Monad.State.Class
import Control.Monad.IO.Class

import Data.Traversable
import qualified Data.Map as M
import Data.Maybe
import Text.Read (readMaybe)
import GHC.Data.Maybe (firstJust)
import Data.Char (toUpper)
import Data.Semigroup

type Image = M.Map (Int, Int) (Char, AttrName)

data Motion = JK Int | HL Int

data Bubble = Bubble
  { _contents :: String
  , _anchorPos :: (Int, Int)
  , _children :: [Bubble]
  } deriving (Eq, Ord, Show)
$(makeLenses ''Bubble)

data MindMap = MindMap
  { _rootBubbles :: [Bubble]
  } deriving (Eq, Ord, Show)
$(makeLenses ''MindMap)

data MindApp = MindApp
  { _saveState :: MindMap
  , _focus :: Either (Int, Int) ((Int, Int), (Bubble, [Bubble]))
  , _screenOffset :: (Int, Int)
  , _keyStack :: String
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
        , _focus = Left (5, 5)
        , _screenOffset = (0, 0)
        , _keyStack = ""
        }
  initialVty <- builder
  void $ customMain initialVty builder Nothing mindAppInstructions appState

mindAppInstructions :: App MindApp Event Name
mindAppInstructions = App
          { appDraw = return . drawMindMaps
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = mkAttrMap
          }

drawMindMaps :: MindApp -> Widget Name
drawMindMaps st = collapseImage $ drawCursor st . foldr M.union M.empty $
  drawBubbleTree <$> view (saveState . rootBubbles) st

handleEvent :: BrickEvent Name Event -> EventM Name MindApp ()
handleEvent (VtyEvent (V.EvKey k ms)) = handleKey k ms
handleEvent _ = return ()

mkAttrMap :: MindApp -> AttrMap
mkAttrMap _ = attrMap (fg V.white) . foldr (<>) [] $
  [ bubbleGroup
  , cursorGroup
  ] where
    bubbleGroup = do
      variant <- attrName <$> ["bubble", "bubble-border"]
      (layer, style) <- (id *** fg) <$>
        [ (0 :: Int, V.blue)
        , (1, V.cyan)
        , (2, V.green)
        , (3, V.yellow)
        ]
      return $ (variant <> stimesMonoid layer (attrName "sub"), style)
    cursorGroup = do
      (variant, color) <-
        [ (mempty, V.white)
        , (attrName "insert", V.blue)
        , (attrName "selected", V.yellow)
        ]
      return $ (attrName "cursor" <> variant, V.black `on` color)

newMindMap :: MindMap
newMindMap = MindMap
  [ Bubble "hello" (50, 25)
      [ Bubble "world" (40, 20) []
      , Bubble "haskell" (60, 20)
          [ Bubble "→ " (70, 17) []
          , Bubble "λ" (70, 23) []
          , Bubble "Brick" (55, 15)
              [ Bubble "Graphics.Vty" (40, 10) []
              , Bubble "TUI Graphics" (60, 10) []
              ]
          ]
      , Bubble "from the other side" (50, 30) []
      , Bubble "uwu" (36, 23) []
      , Bubble "hackathon" (35, 27)
          [ Bubble "o/" (24, 29) []
          ]
      ]
  , Bubble "yeet" (0, 0) []
  ]

{-------------------------------
 - Motions
 ------------------------------}

handleKey :: V.Key -> [V.Modifier] -> EventM Name MindApp ()
handleKey (V.KChar 'c') [V.MCtrl] = halt
handleKey (V.KChar c) [] = do
  modify $ over keyStack (c :) -- stored in REVERSE ORDER
  collapseKeyStack
handleKey (V.KChar c) [V.MShift] = do
  modify $ over keyStack (toUpper c :) -- stored in REVERSE ORDER
  collapseKeyStack
handleKey (V.KEnter) [] = selectAtCursor
handleKey (V.KEsc) [] = unselect
handleKey _ _ = return ()
-- handleKey (V.KChar 'j') ms = handleMove ms $ JK 1
-- handleKey (V.KChar 'k') ms = handleMove ms $ JK (-1)
-- handleKey (V.KChar 'h') ms = handleMove ms $ HL (-1)
-- handleKey (V.KChar 'l') ms = handleMove ms $ HL 1
-- handleKey (V.KChar c) []
  -- | '0' <= c && c <= '9' = do
      -- let n = read [c]
      -- modify $ over motionLoop $ (+n) . (*10)
-- handleKey _ _ = return ()

handleMove :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
handleMove l n = modify $ over focus $ over l (+n) +++ over (_1 . l) (+n)

handleShift :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
handleShift l n = modify $ over focus . right . over (_2 . _1 . anchorPos . l) $ (+n)

handleShiftDeep :: MonadState MindApp m => Lens' (Int, Int) Int -> Int -> m ()
handleShiftDeep l n = modify . over focus . right . over (_2 . _1) $ _handleShiftDeep l n
  where _handleShiftDeep l n = over (anchorPos . l) (+n)
                             . over children (fmap $ _handleShiftDeep l n)

collapseKeyStack :: MonadState MindApp m => m ()
collapseKeyStack = do
    keyStack' <- view keyStack <$> get
    let action = case keyStack' of
          'j':cs -> Just $ handleMotion cs _2 1
          'k':cs -> Just $ handleMotion cs _2 (-1)
          'h':cs -> Just $ handleMotion cs _1 (-1)
          'l':cs -> Just $ handleMotion cs _1 1
          _ -> Nothing
    case action of
      Just m -> do
        m
        modify $ set keyStack ""
      Nothing -> return ()

handleMotion :: MonadState MindApp m => String -> Lens' (Int, Int) Int -> Int -> m ()
handleMotion ('g':cs) l = motionLoop cs . handleShift l
handleMotion ('G':cs) l = motionLoop cs . handleShiftDeep l
handleMotion cs l = motionLoop cs . handleMove l

motionLoop :: MonadState MindApp m => String -> m () -> m ()
motionLoop cs = fromMaybe id $
  (sequence_ .) . replicate <$> (readMaybe . reverse $ cs)

unselect :: MonadState MindApp m => m ()
unselect = do
  focus' <- view focus <$> get
  case focus' of
    Left _ -> return ()
    Right (p, zs) -> do
      modify $ set focus . Left $ onBoth (+) p $ view (_1 . anchorPos) zs
      modify $ over (saveState . rootBubbles) . (:) $ unzipBubbles zs

selectAtCursor :: MonadState MindApp m => m ()
selectAtCursor = do
  bs <- view (saveState . rootBubbles) <$> get
  (msieved, rem) <- sieveM inTreeUnderCursor bs
  case msieved of
    Just zipper -> do
      modify $ set (saveState . rootBubbles) rem
      modify $ set focus . Right $ ((0, 0), zipper)
    Nothing -> return ()

inTreeUnderCursor :: MonadState MindApp m => Bubble -> m (Maybe (Bubble, [Bubble]))
inTreeUnderCursor b = do
  onPoint <- underCursor b
  if onPoint
    then return $ Just (b, [])
    else do
      (mchild, childs) <- sieveM inTreeUnderCursor $ view children b
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
      let p0 = view anchorPos b
          p1 = onBoth (+) p0 <<< stringWidth &&& stringHeight <<< view contents $ b
      return $ (uncurry (&&) $ onBoth (>=) p p0)
              && (uncurry (&&) $ onBoth (<=) p p1)
    Right _ -> return False

unzipBubbles :: (Bubble, [Bubble]) -> Bubble
unzipBubbles = uncurry $ foldr (flip $ over children . (:))

{-------------------------------
 - Drawing Nodes
 -------------------------------}

drawCursor :: MindApp -> Image -> Image
drawCursor app = case view focus app of
                  Left p -> over (at p) $ _drawCursor mempty
                  Right (p, bs) ->
                    let p' = onBoth (+) p $ view (_1 . anchorPos) bs
                        drawCursor' = over (at p') (_drawCursor $ attrName "selected")
                    in (drawCursor' .) . M.union . drawBubbleTree . unzipBubbles $ bs
  where _drawCursor m Nothing = Just (' ', attrName "cursor" <> m)
        _drawCursor m (Just (c, _)) = Just (c, attrName "cursor" <> m)

drawBubbleTree :: Bubble -> Image
drawBubbleTree b = foldl M.union (drawBubble 0 b) $
  drawBubbleBranch 1 (bubbleCenter b) <$> view children b

drawBubbleBranch :: Int -> (Int, Int) -> Bubble -> Image
drawBubbleBranch n p b = foldl M.union (drawChildBubble n p b) $
  drawBubbleBranch (n + 1) (bubbleCenter b) <$> view children b

drawChildBubble :: Int -> (Int, Int) -> Bubble -> Image
drawChildBubble n p b = M.union (drawBubble n b) (drawSteppedLine 2 '~' mempty p $ bubbleCenter b)

drawBubble :: Int -> Bubble -> Image
drawBubble n b = shiftImage (view anchorPos b)
               . imageBorder (attrName "bubble-border" <> stimesMonoid n (attrName "sub"))
               . imageString (attrName "bubble" <> stimesMonoid n (attrName "sub"))
               . view contents $ b

bubbleCenter :: Bubble -> (Int, Int)
bubbleCenter b = onBoth (+) (view anchorPos b)
                <<< join (***) (flip quot 2)
                <<< stringWidth &&& stringHeight $ view contents b

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
imageString a = chartPositions . (fmap $ fmap (,a)) . lines

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

chartPositions :: [[a]] -> M.Map (Int, Int) a
chartPositions = M.fromList . join
               . fmap (uncurry $ \y -> fmap $ \(x, c) -> ((x, y), c))
               . fmap (id *** mkIndices)
               . mkIndices

{---------------------------
 - Generic functions
 ---------------------------}

sieveM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b, [a])
sieveM f (x:xs) = do
  result <- f x
  case result of
    Just b -> return $ (Just b, xs)
    Nothing -> (id *** (x:)) <$> sieveM f xs
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

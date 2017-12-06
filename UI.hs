{-# LANGUAGE UnicodeSyntax,TemplateHaskell, FlexibleContexts #-}
 
module Main where
import Brick 
import Brick.Util (clamp)
import qualified Graphics.Vty as V
import qualified Brick.Main as M 
import Brick.Types
  (Widget
  , BrickEvent(..))
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.Vector as Vc
import Control.Monad.State (liftIO,void)
import SocketComm

type Color = ((Int,Int,Int),(Int,Int,Int))

type GridState = Vc.Vector (Vc.Vector (Int,Color))

data StateSet = StateSet { grids :: Vc.Vector GridState,
                           currPos :: Int,
                           gridCommunicator :: Communicator } deriving (Eq,Show)

theMap :: AttrMap
theMap = attrMap V.defAttr []

getColorAttr :: Color -> V.Attr
getColorAttr c = getColor (fst c) `on` getColor (snd c)

getColor :: (Int,Int,Int)->V.Color
getColor c = (\(x,y,z)-> V.rgbColor x y z) $ c

genType0Block :: Color -> Widget ()
genType0Block c = updateAttrMap a2a type0Widget
  where
    type0Widget = (withDefAttr (attrName "type0") (B.border $ str "   "))
    a2a = applyAttrMappings [(attrName "type0", getColorAttr c)]
    
genType1Block :: Color -> Widget ()
genType1Block c = updateAttrMap a2a type1Widget
  where
    type1Widget = (withDefAttr (attrName "type1") (B.border $ str ['\x2588','\x2588','\x2588']))
    a2a = applyAttrMappings [(attrName "type1", getColorAttr c)]

matrixMap :: (a -> b) -> Vc.Vector (Vc.Vector a) -> Vc.Vector (Vc.Vector b)
matrixMap f mtrx = Vc.map (\x-> Vc.map f x) mtrx

makeGridWidget :: GridState -> Widget ()
makeGridWidget llw = (\w-> foldl1 (<=>) w) $ fmap (\x-> foldl1 (<+>) x) $ toWidget llw
  where
    toWidget xss = matrixMap (\(x,y)->case (x,y) of
                                            (0,c) -> genType0Block c
                                            (1,c) -> genType1Block c
                                            (_,c) -> genType0Block c) xss

drawGrid :: StateSet -> [Widget ()]
drawGrid g = [C.center $ makeGridWidget ((Vc.!) (grids g) (currPos g))]

nextGrid :: StateSet -> EventM () (Next StateSet)
nextGrid stateSet = do
  liftIO $ sendTo (gridCommunicator stateSet) "1"
  let gridComm = (gridCommunicator stateSet)
  str1 <- liftIO $ getFrom gridComm
  if str1 == "EOI" then (liftIO $ terminateCommunicator gridComm) >> halt stateSet else do
    let newGrid = read str1 :: GridState
    let st1 = stateSet { grids = Vc.snoc (grids stateSet) newGrid }
    let st2 = st1 { currPos = clamp 0 (Vc.length (grids st1) -1) (currPos st1 +1),
                    gridCommunicator = gridComm }
    continue st2

quitGrid :: StateSet -> EventM () (Next StateSet)
quitGrid stateSet = do
  liftIO $ sendTo (gridCommunicator stateSet) "q"
  x <- liftIO $ terminateCommunicator (gridCommunicator stateSet)
  liftIO $ putStrLn $ show x
  halt stateSet

handleEvent :: StateSet -> BrickEvent () e -> EventM () (Next StateSet)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'n') [])) = nextGrid g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'u') [])) = continue $ prevGrid g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ g {currPos = 0}
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = quitGrid g
handleEvent g _                                     = continue g

app :: App StateSet e ()
app = App { appDraw = drawGrid
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  gridComm <- newCommunicator 4001 4000
  -- gridComm1 <- verifyCommunicator gridComm
  -- gridComm2 <- waitForInput gridComm1
  str1 <- getFrom gridComm
  let newGrid = read str1 :: GridState
  let stateSet = StateSet (Vc.singleton newGrid) 0 gridComm
  void $ M.defaultMain app stateSet

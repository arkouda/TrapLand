import qualified Data.Vector as Vc (Vector, replicate, length, map, (!), accum)
import qualified Data.Map as M (Map, adjust, fromList, filter, assocs, lookup,  elems, map, null)
import SocketComm
import Data.List (zipWith4)
import Data.Aeson (FromJSON, parseJSON, decode, (.:), (.:?), withObject)
import Data.Text (pack)
import qualified Data.ByteString.Lazy as B (readFile)
import System.Environment (getArgs)
import System.Process (createProcess,shell)
import Control.Monad.State (StateT, lift, get, put, runStateT)
import Control.Concurrent
import Control.Monad (zipWithM, forever)
import Network (PortNumber)
--------------------------Structure------------------------

type Pos = (Int,Int)
type Color = (Int,Int,Int)
type ColorTup = ((Int,Int,Int),(Int,Int,Int))
type Grid = Vc.Vector (Vc.Vector Cell)

data Cell = Cell { cellMem :: ColorTup,
                   effectiveColor :: ColorTup,
                   isWritable :: Bool,
                   isPassable :: Bool } deriving (Eq,Show)

data RoboState = RoboState { rPos :: Pos,
                             rColor :: ColorTup,
                             communicator :: Communicator,
                             tmv :: (ThreadId, MVar RoboRequest),
                             isDead :: Bool } deriving (Eq)

data GridState = GridState { grid :: Grid,
                             roboInfo :: M.Map Int RoboState,
                             dispCommunicator :: Communicator } deriving (Eq)

data RoboConfig = RoboConfig { rID :: Int,
                               progName :: FilePath,
                               _pos :: Pos,
                               color :: ColorTup } deriving (Eq,Show)

data Config = Config { size :: (Int,Int),
                       robos :: [RoboConfig] } deriving (Eq,Show)

data RoboRequest = L | R | U | D | N | Read | Write ColorTup | Halt | Empty | ILEFT | IRIGHT | IUP | IDOWN | FP | FW deriving (Eq,Show,Read)

instance FromJSON RoboConfig where
 parseJSON = withObject "RoboConfig" $ \o-> do
   _rId <- o .: (pack "rId")
   _progName <- o .: (pack "progName")
   _pos <- o .: (pack "pos")
   _color <- o .:? (pack "color")
   case _color of
     Just x -> return $ RoboConfig _rId _progName _pos x
     Nothing -> return $ RoboConfig _rId _progName _pos defCellColor
             
instance FromJSON Config where
 parseJSON = withObject "RoboConfig" $ \o-> do
   _size <- o .: (pack "size")
   _robos <- o .: (pack "robos")
   return $ Config _size _robos

-----------------------------Main--------------------------
   
main :: IO ()
main = do
  args<-getArgs
  if length args /= 1
    then fail "Invalid Args"
    else do
    world <- B.readFile (args!!0)
    let config = case decode world :: Maybe Config of
          Just x -> x
          Nothing -> error "World File Error!!!\nInvalid Syntax or Missing Field"
    gridState <- initWithConfig config
    let newGrid = foldl (\b rs-> changeEffectiveColorAtPos (rPos rs) (rColor rs) b) (grid gridState) (M.elems (roboInfo gridState))
    _<-runStateT runGridState (gridState { grid = newGrid })
    putStrLn $ show config

runGridState :: StateT GridState IO ()
runGridState = do
  gridState <- get
  let roboStates = roboInfo gridState
  lift $ sendTo (dispCommunicator gridState) (show $ toUI gridState)
  let aliveRobos = (M.filter (\x-> not $ isDead x) roboStates)
  if M.null aliveRobos then lift (sendTo (dispCommunicator gridState) "EOI" >> terminateCommunicator (dispCommunicator gridState)) >> return () else do
    lift $ putStrLn $ show $ length aliveRobos
    reqs <- lift $ sequence $ M.map (\x-> getRoboRequest x) aliveRobos
    newGridState <- lift $ executeRequests gridState reqs
    dispAck <- lift $ getFrom (dispCommunicator gridState)
    case dispAck of
      "1" -> put newGridState >> runGridState
      "q" -> lift (terminateCommunicator (dispCommunicator gridState)) >>
             lift (mapM_ (\x-> sendTo (communicator x) "3" >> terminateCommunicator (communicator x) >> killThread (fst $ tmv $ x)) (M.elems aliveRobos)) >>
             return ()

forkedAccept :: Communicator -> MVar RoboRequest -> IO ()
forkedAccept comm mvar = forever $ do
  req <- getFrom comm
  putStrLn req 
  putMVar mvar (read req :: RoboRequest)
  
getRoboRequest :: RoboState -> IO RoboRequest
getRoboRequest roboState = do
  isEmpty <- isEmptyMVar $ snd $ tmv roboState
  putStrLn $ show isEmpty
  case isEmpty of
    True -> return Empty
    False -> takeMVar $ snd $ tmv roboState

initWithConfig :: Config -> IO GridState
initWithConfig config = do
  let grid1 = uncurry rawGrid (size config)
  let roboList = robos config
  _<-mapM (\x-> createProcess $ shell ("./RoboInterpreterS1 " ++ show (rID x) ++ " " ++ show (progName x) ++ " " ++ show (show $ color x))) roboList
  let pos1 = map (\x-> (\(y,z)->(y-1,z-1)) $ _pos x) roboList
  let color1 = map color roboList
  communicators <- sequence $ map (\x-> newCommunicator (read (show (2*(3000+x)))::PortNumber) (read (show (2*(3000+x)+1))::PortNumber)) (map rID roboList)
  mvars <- sequence $ replicate (length communicators) (newEmptyMVar)
  threadIDs <- zipWithM (\c m -> forkIO (forkedAccept c m)) communicators mvars
  let tmvs = zip threadIDs mvars
  let roboStates = (zipWith4 (\p c comms tmv-> RoboState p c comms tmv False) pos1 color1 communicators tmvs)
  dispComm <- newCommunicator 4000 4001
  return $ GridState grid1 (M.fromList $ zip (map rID roboList) roboStates) dispComm

------------------------Command Execution---------------------

executeRequests :: GridState -> M.Map Int RoboRequest -> IO GridState
executeRequests gs reqs = do
  let cmdTupList = M.assocs reqs
  foldl (\g (i,r)-> exec1Req g r i) (return gs) cmdTupList
  
exec1Req :: IO GridState -> RoboRequest -> Int -> IO GridState
exec1Req gsIO req rId = do
  gs <- gsIO
  let roboState = maybe (error "Robo ID mismatch") id $ M.lookup rId $ roboInfo gs
  if isDead roboState then return gs
    else
    case req of
      Halt -> terminateCommunicator (communicator roboState) >>
              killThread (fst $ tmv $ roboState) >>
              return (gs { roboInfo = M.adjust (\a-> a {isDead = True}) rId (roboInfo gs)})
      Read -> sendTo (communicator roboState) ("2"++(show $ getColorFromPos (grid gs) (rPos roboState))) >>
              return (gs { roboInfo = M.adjust (\a-> a {rColor = getColorFromPos (grid gs) (rPos roboState)}) rId (roboInfo gs)})
      Write c -> sendTo (communicator roboState) "1" >>
                 return (if getValueFromPos (grid gs) (rPos roboState) isWritable
                         then (gs {grid=matrixUpdate (\cell-> cell {cellMem=c}) (rPos roboState) (grid gs)})
                         else gs)
      FP -> sendTo (communicator roboState) "1" >>
            return (gs { grid = matrixUpdate (\cell-> cell {isPassable = not $ isPassable cell}) (rPos roboState) (grid gs)})
      FW -> sendTo (communicator roboState) "1" >>
            return (gs { grid = matrixUpdate (\cell-> cell {isPassable = not $ isWritable cell}) (rPos roboState) (grid gs)})
      IUP -> if rPos roboState == rPos (executeMove roboState U (getMaxPos (grid gs)))
             then sendTo (communicator roboState) "True" >> return gs
             else sendTo (communicator roboState) "False" >> return gs
      IRIGHT -> if rPos roboState == rPos (executeMove roboState R (getMaxPos (grid gs)))
             then sendTo (communicator roboState) "True" >> return gs
             else sendTo (communicator roboState) "False" >> return gs
      ILEFT -> if rPos roboState == rPos (executeMove roboState L (getMaxPos (grid gs)))
             then sendTo (communicator roboState) "True" >> return gs
             else sendTo (communicator roboState) "False" >> return gs
      IDOWN -> if rPos roboState == rPos (executeMove roboState D (getMaxPos (grid gs)))
             then sendTo (communicator roboState) "True" >> return gs
             else sendTo (communicator roboState) "False" >> return gs
      Empty -> return gs
      x ->  sendTo (communicator roboState) "1" >>
        return (gs {roboInfo = M.adjust (const newRoboState) rId (roboInfo gs),
                    grid = changeEffectiveColorAtPos (rPos newRoboState) (rColor newRoboState) newGrid})
        where
          newRoboState = if getValueFromPos (grid gs) (rPos newRoboState1) isPassable then newRoboState1 else roboState
          newRoboState1 = (executeMove roboState x (getMaxPos (grid gs)))
          newGrid = if getValueFromPos (grid gs) (rPos newRoboState1) isPassable then newGrid1 else grid gs
          newGrid1 = (setEffectiveColorToCellColor (rPos roboState) (grid gs))
          
executeMove :: RoboState -> RoboRequest -> Pos -> RoboState
executeMove roboState req maxPos = case req of
                      U -> roboState { rPos = (\(x,y)-> (clamp 0 (fst maxPos) (x-1),y)) $ (rPos roboState) }
                      D -> roboState { rPos = (\(x,y)-> (clamp 0 (fst maxPos) (x+1),y)) $ (rPos roboState) }
                      R -> roboState { rPos = (\(x,y)-> (x,clamp 0 (snd maxPos) (y+1))) $ (rPos roboState) }
                      L -> roboState { rPos = (\(x,y)-> (x,clamp 0 (snd maxPos) (y-1))) $ (rPos roboState) }
                      N -> roboState
                      _ -> roboState

------------------------Wrapper Fxns--------------------------

clamp :: Ord a => a -> a -> a -> a
clamp mn mx val = max mn (min val mx)

getMaxPos :: Grid -> Pos
getMaxPos g = (Vc.length g -1 , Vc.length (g Vc.! 0) -1 )

defBgColor :: Color
defBgColor = (50,50,50)

defCellColor :: ColorTup
defCellColor = ((0,255,100),defBgColor)

defCell :: Cell
defCell = Cell defCellColor defCellColor True True

matrixMap :: (a -> b) -> Vc.Vector (Vc.Vector a) -> Vc.Vector (Vc.Vector b)
matrixMap f matrix = Vc.map (\x-> Vc.map f x) matrix

matrixUpdate :: (a -> a) -> Pos -> Vc.Vector (Vc.Vector a) -> Vc.Vector (Vc.Vector a)
matrixUpdate f pos matrix = Vc.accum (\x y-> Vc.accum (\a b-> b a) x [(snd pos,y)]) matrix [(fst pos, f)]

rawGrid :: Int -> Int -> Grid
rawGrid r c = Vc.replicate r $ Vc.replicate c defCell

changeEffectiveColorAtPos :: Pos -> ColorTup -> Grid -> Grid
changeEffectiveColorAtPos pos colorTup matrix = matrixUpdate (\x-> x {effectiveColor = colorTup}) pos matrix

getColorFromPos :: Grid -> Pos -> ColorTup
getColorFromPos matrix pos = cellMem $ (matrix Vc.! (fst pos)) Vc.! (snd pos)

getValueFromPos :: Grid -> Pos -> (Cell -> Bool) -> Bool
getValueFromPos matrix pos f = f $ (matrix Vc.! (fst pos)) Vc.! (snd pos)

setEffectiveColorToCellColor :: Pos -> Grid -> Grid
setEffectiveColorToCellColor pos matrix = matrixUpdate (\x-> x { effectiveColor = cellMem x }) pos matrix

toUI :: GridState -> Vc.Vector (Vc.Vector (Int,ColorTup))
toUI gs = foldl (\g p-> matrixUpdate (\tup->(1,snd tup)) p g) (matrixMap (\x-> (0,effectiveColor x)) (grid gs)) ((rPos) <$> roboInfo gs)

clampPos :: (Int,Int) -> (Int,Int)
clampPos (x,y) = (x-1,y-1)

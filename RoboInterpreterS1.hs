import qualified Data.Map as M
import SocketComm
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token
import Control.Monad (void)
import Data.Either (lefts,rights)
import Data.List (findIndex)
import Control.Monad.State
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Network (PortNumber)

data Condition = IsWall { arg :: Direction }  deriving (Show,Eq,Read)

data Direction = LEFT | RIGHT | UP | DOWN | NONE deriving (Show,Eq,Read)

data NObject = OpBrace | ClBrace | EmptyLine | Comment | Else | Other String deriving (Eq,Show)

type FunctionName = String

data Instruction = Read | 
                   Write | 
                   Move { dir :: Direction } | 
                   If { ifc :: Condition,
                        ifi :: Int,
                        ife :: Int } |
                   EndOfFunction { eo :: FunctionName,
                                   retList :: [Int] } | 
                   NOP NObject | 
                   JumpC { jc :: Int } |
                   Jump { j :: Int } |
                   FlipPassable |
                   FlipWritable |
                   Return | --
                   BlockStart | --
                   BlockEnd | --
                   IfIntermediate { ifinter :: Condition } | --
                   FunctionStart { fs :: FunctionName } | --
                   FunctionDef { fd :: FunctionName } | --
                   FunctionCall { fc :: FunctionName } --
                 deriving (Eq,Show)

type ForInterpreter = [(Instruction,String)]
type CurrentPosition = Int
type FunctionTable = M.Map Int Int

data InterpreterState = InterpreterState { insStr :: ForInterpreter,
                                           pointer :: CurrentPosition,
                                           funcTab :: FunctionTable,
                                           robo :: Robo,
                                           comm :: Communicator } deriving (Eq,Show)

type Color = (Int,Int,Int)
type ColorTup = (Color,Color)
data Robo = Robo { id :: Int,
                   roboColor :: ColorTup } deriving (Eq,Show)

main :: IO ()
main = do
  args<-getArgs
  if length args == 3
    then do
      let rID = read (args!!0)::Int
      comm <- newCommunicator (read (show (2*(3000+rID)+1))::PortNumber) (read (show (2*(3000+rID)))::PortNumber)
      let filePath = args!!1
      let color = read (args!!2)::ColorTup 
      let initRobo = Robo rID color
      -- comm1 <- verifyCommunicator comm
      str1 <- readFile filePath
      let initInterpreterState = (\(x,y,z)->InterpreterState x y z initRobo comm) $ getInterpreterReady str1      
      _<-runStateT runRobo initInterpreterState
      terminateCommunicator comm
      return ()
    else fail "Invalid Args!!!" >> return ()
    
runRobo :: StateT InterpreterState IO ()
runRobo = do
  intprState <- get
  newIntpr <- lift $ evalStateT runInterpreter $ toNextPointer intprState
  if pointer newIntpr <= -1 then lift (sendTo (comm intprState) "Halt") >> return ()
    else do
    lift $ sendRequest newIntpr    
    entry <- lift $ getFrom (comm intprState)    
    let code = head entry
    case code of
      '1' -> put newIntpr >> runRobo
      '2' -> put (newIntpr {robo = ((robo newIntpr) { roboColor = read (tail entry) :: ColorTup })}) >> runRobo
      '3' -> lift (putStrLn ("R" ++ show (Main.id $ robo intprState) ++ " quitting..")) >> return ()
      _ -> fail "Unknown Response from Grid!!"
 
runInterpreter :: StateT InterpreterState IO InterpreterState
runInterpreter = do
  intprState <- get 
  case getCurrentInstruction intprState of
    NOP x -> put (toNextPointer intprState) >> runInterpreter
    Jump i -> put (intprState { pointer = i }) >> runInterpreter
    JumpC i -> put (executeJumpC intprState i) >> runInterpreter
    FlipPassable -> return intprState
    FlipWritable -> return intprState    
    EndOfFunction eo r -> if (eo == "main" && r == [])
                          then return (intprState {pointer = -1})
                          else put (executeEOFunc intprState) >> runInterpreter
    Read -> return intprState
    Write -> return intprState
    Move d -> return intprState
    If c i e -> do
      lift $ sendTo (comm intprState) ("I" ++ show (arg c))
      str1 <- lift $ getFrom (comm intprState)
      lift $ putStrLn $ "  " ++ str1
      let condEval = read str1 :: Bool
      case condEval of
        True -> put (intprState { pointer = i }) >> runInterpreter
        False -> put (intprState { pointer = e }) >> runInterpreter
    _ -> fail "Bad Parse"
        
sendRequest :: InterpreterState -> IO ()
sendRequest intprState = case getCurrentInstruction intprState of
                           Move x -> sendTo (comm intprState) ([head $ show x]) 
                           Read -> sendTo (comm intprState) "Read"
                           Write -> sendTo (comm intprState) ("Write " ++ (show $ roboColor (robo intprState)))
                           FlipPassable -> sendTo (comm intprState) "FP"
                           FlipWritable -> sendTo (comm intprState) "FW"
                           
executeJumpC :: InterpreterState -> Int -> InterpreterState
executeJumpC intprState i = retListUpdated { pointer = i }
  where
    eoFuncPointer = M.findWithDefault (-1) i (funcTab intprState)
    retListUpdated = intprState { insStr = listUpdate (insStr intprState)
                                         ((\(ins,s)-> (ins {retList = (retList ins) ++ [pointer intprState +1]},s)) $
                                          (insStr intprState)!!eoFuncPointer)
                                         eoFuncPointer }

executeEOFunc :: InterpreterState -> InterpreterState
executeEOFunc intprState = state1 { insStr = newInsStr }
  where
    currStr = getCurrentString intprState
    currIns = getCurrentInstruction intprState
    state1 = intprState { pointer = last $ retList currIns } 
    newInsStr = listUpdate (insStr state1) (currIns {retList = init $ retList currIns}, currStr) (pointer intprState)
    
getCurrentInstruction :: InterpreterState -> Instruction
getCurrentInstruction intprState = fst $ (insStr intprState)!!(pointer intprState)

getCurrentString :: InterpreterState -> String
getCurrentString intprState = snd $ (insStr intprState)!!(pointer intprState)

toNextPointer :: InterpreterState -> InterpreterState
toNextPointer intprState = intprState { pointer = (pointer intprState) + 1 }

----------------------------------Make Initial Interpreter State--------------------------

getInterpreterReady :: String -> (ForInterpreter, CurrentPosition, FunctionTable)
getInterpreterReady str1 = (forIntpr, (fst intprInit), (snd intprInit))
  where
    intprInit = interpreterInit insListWithoutFinally
    forIntpr = zip (finally insListWithoutFinally) (lines str1)
    insListWithoutFinally = (resolveReturn $ ifJumpResolve $ verifyIfs $ verifyIfs $  resolveFunctionCalls $ verifyFunctions $ getSource str1)

interpreterInit :: [Instruction] -> (Int, M.Map Int Int)
interpreterInit xs = ((getStartPoint lineFunc),(M.fromList $ toFuncEOFmap lineFunc))
  where
    lineFunc = filter (\ (i,ins)-> case ins of
                          FunctionStart s -> True
                          EndOfFunction s r -> True
                          _ -> False ) (zip [0..] xs)

getStartPoint :: [(Int,Instruction)] -> Int
getStartPoint xs = (\[(i,ins)]-> i) $ filter (\ (i,ins)-> ins == FunctionStart "main") xs

toFuncEOFmap :: [(Int,Instruction)] -> [(Int,Int)]
toFuncEOFmap xs = if xs == [] then [] else [(\xss-> (fst (xss!!0), fst (xss!!1))) $ (take 2 xs)] ++ toFuncEOFmap (drop 2 xs)

---------------------------------Instruction Conversion/Jump Resolution--------------------------------

finally :: [Instruction] -> [Instruction]
finally xs = map (\x-> case x of
                         FunctionDef z -> NOP (Other $ show $ FunctionDef z)
                         FunctionStart z -> NOP (Other $ show $ FunctionStart z)
                         BlockStart -> NOP $ Other "BlockStart"
                         z -> z ) xs

resolveFunctionCalls :: [Instruction] -> [Instruction]
resolveFunctionCalls xs = foldl (\iList (ins,index)-> listUpdate iList (JumpC (nextIndex (FunctionStart (fc ins)) iList)) index) xs functionCallIndices 
  where
    functionCallIndices = filter (\(ins,index)-> case ins of
                                           FunctionCall x -> True
                                           _ -> False) $  zip xs [0..]

resolveReturn :: [Instruction] -> [Instruction]
resolveReturn xs = foldl (\iList (ins,index)-> listUpdate iList (Jump (nextEOFuncIndex index iList)) index) xs functionCallIndices 
  where
    functionCallIndices = filter (\(ins,index)-> case ins of
                                           Return -> True
                                           _ -> False) $  zip xs [0..]

findMatching :: Eq a => a -> a -> Int -> [a] -> Int -> Int
findMatching x y 0 xs n = n
findMatching x y i [] n = -1
findMatching x y i xs n | x == head xs = findMatching x y (i-1) (tail xs) (n+1)
                        | y == head xs = findMatching x y (i+1) (tail xs) (n+1)
                        | otherwise = findMatching x y i (tail xs) (n+1)

getSource :: String -> [Instruction]
getSource str = if lefts parsedLines == [] then rights parsedLines else (map (\x->error $ show x) $ lefts parsedLines)
  where
    parsedLines = map (parser instructionParser) (lines str)

listUpdate :: [a] -> a -> Int -> [a]
listUpdate xs x i = take i xs ++  [x] ++ drop (i+1) xs 

nextIndex :: Eq a => a -> [a] -> Int
nextIndex x xs = maybe (error "Syntax Error") Prelude.id (findIndex (==x) xs)

verifyFunctions :: [Instruction] -> [Instruction]
verifyFunctions xs = foldl (\iList (ins,index)-> listUpdate (listUpdate iList (FunctionStart (fd ins)) (nextOpIndex index iList)) (EndOfFunction (fd ins) []) (findMatching (NOP ClBrace) (NOP OpBrace) 1 (drop (1+nextOpIndex index iList) iList) (nextOpIndex index iList))) xs fDefIndices 
  where
    fDefIndices = filter (\(ins,index)-> case ins of
                                           FunctionDef x -> True
                                           _ -> False) $  zip xs [0..]

verifyIfs :: [Instruction] -> [Instruction]
verifyIfs xs = foldl (\iList (ins,index)-> listUpdate (listUpdate iList (BlockStart) (nextOpIndex index iList)) BlockEnd (findMatching (NOP ClBrace) (NOP OpBrace) 1 (drop (1+nextOpIndex index iList) iList) (nextOpIndex index iList))) xs ifInterIndices 
  where
    ifInterIndices = filter (\(ins,index)-> case ins of
                                           IfIntermediate x -> True
                                           _ -> False) $  zip xs [0..]

ifJumpResolve :: [Instruction] -> [Instruction]
ifJumpResolve xs = foldl (\iList (ins,index)-> listUpdate (listUpdate iList (Jump $ blockEndIndex index iList) (findMatching (BlockEnd) (BlockStart) 1 (drop (1+nextBSIndex index iList) iList) (nextBSIndex index iList))) (Jump $ blockEndIndex index iList) (findMatching (BlockEnd) (BlockStart) 1 (drop (1+ nextBSIndex (1+(findMatching (BlockEnd) (BlockStart) 1 (drop (1+nextBSIndex index iList) iList) (nextBSIndex index iList))) iList) iList) (nextBSIndex (1+(findMatching (BlockEnd) (BlockStart) 1 (drop (1+nextBSIndex index iList) iList) (nextBSIndex index iList))) iList))) ifInterToIf ifInterIndices1
  where
    ifInterToIf = foldl (\iList (ins,index)-> listUpdate iList (If (ifinter ins) (nextBSIndex index iList) (nextBSIndex (findMatching BlockEnd BlockStart 1 (drop (1+nextBSIndex index iList) iList) (nextBSIndex index iList)) iList)) index) xs ifInterIndices1
    ifInterIndices1 = filter (\(ins,index)-> case ins of
                                           IfIntermediate x -> True
                                           _ -> False) $  zip xs [0..]

blockEndIndex ind il = 1+findMatching (BlockEnd) (BlockStart) 1 (drop (1+ nextBSIndex (1+ifend) il) il) (nextBSIndex (1+ifend) il)
  where
    ifend = (findMatching (BlockEnd) (BlockStart) 1 (drop (1+nextBSIndex ind il) il) (nextBSIndex ind il))

nextEOFuncIndex :: Int -> [Instruction] -> Int
nextEOFuncIndex i xs = i + (head $ fst $ unzip $ getEofs)
  where
    getEofs = filter (\x-> case x of
                             (index,EndOfFunction n rl) -> True
                             (index,_) -> False ) $ zip [1..] (drop (i+1) xs)

nextOpIndex :: Int -> [Instruction] -> Int
nextOpIndex ind il = 1 + ind + nextIndex (NOP OpBrace) (drop (ind+1) il)

nextBSIndex :: Int -> [Instruction] -> Int
nextBSIndex ind il = 1 + ind + nextIndex BlockStart (drop (ind+1) il)

nextBEIndex :: Int -> [Instruction] -> Int
nextBEIndex ind il = 1 + ind + nextIndex BlockEnd (drop (ind+1) il)

--------------------Parser------------------

languageDef :: LanguageDef u
languageDef = emptyDef { Token.identStart = letter <|> satisfy (=='_')
                       , Token.identLetter = alphaNum
                       , Token.reservedNames = ["move","up","left","right","down","read","write","main","return","if","else","return"]
                       , Token.caseSensitive = False
                       }

lexer :: Token.TokenParser u
lexer = Token.makeTokenParser languageDef

reserved :: String -> Parser ()
reserved  = Token.reserved lexer
eol :: Parser ()
eol = void (char '\n') <|> eof
parens :: Parser a -> Parser a
parens = Token.parens lexer
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
curlies :: Parser a -> Parser a
curlies p = between (Token.symbol lexer "{") (Token.symbol lexer "}") p
brackets :: Parser a -> Parser a
brackets p = between (Token.symbol lexer "[") (Token.symbol lexer "]") p

try' = Text.ParserCombinators.Parsec.try

parser :: Parser a -> String -> Either ParseError a
parser p x = parse p "" x

instructionParser :: Parser Instruction
instructionParser = try' readWriteParser <|>
                    (try' ifIntermediateParser <|>
                    try' moveParser <|>
                    try' flipPassableParser <|>
                    try' flipWritableParser <|>
                    try' returnParser <|>
                    try' funcDefParser <|>
                    try' nopParser <|>
                    try' functionCallParser) 

ifIntermediateParser :: Parser Instruction
ifIntermediateParser = whiteSpace >> reserved "if" >> ((\x-> IfIntermediate x) <$> parens conditionParser)

returnParser :: Parser Instruction
returnParser = whiteSpace >> reserved "return" >> return Return

flipPassableParser :: Parser Instruction
flipPassableParser = whiteSpace >> reserved "flippassable" >> parens (string "") >> whiteSpace >> return FlipPassable

flipWritableParser :: Parser Instruction
flipWritableParser = whiteSpace >> reserved "flipwritable" >> parens (string "") >> whiteSpace >> return FlipWritable

functionCallParser :: Parser Instruction
functionCallParser = whiteSpace >> ((\x-> FunctionCall x) <$> (many1 alphaNum <* (whiteSpace >> parens (string ""))))

readWriteParser :: Parser Instruction
readWriteParser = try' (whiteSpace >> reserved "read" >> whiteSpace >> parens (string "") >> whiteSpace >> return Read) <|>
                  try' (whiteSpace >> reserved "write" >> whiteSpace >> parens (string "") >> whiteSpace >> return Write)

nopParser :: Parser Instruction
nopParser = (\x-> NOP x) <$> (try' commentParser <|>
                              try' opBraceParser <|>
                              try' clBraceParser <|>
                              try' elseParser <|>
                              try' blankLineParser)

moveParser :: Parser Instruction
moveParser = whiteSpace >> reserved "move" >> Move <$> parens directionParser

funcDefParser :: Parser Instruction
funcDefParser = whiteSpace >> reserved "def" >> whiteSpace >> ((\x-> FunctionDef x) <$> (many1 alphaNum <* (whiteSpace >> parens (string ""))))

conditionParser :: Parser Condition
conditionParser = whiteSpace >> reserved "iswall" >> ((\x-> IsWall x) <$> parens directionParser)

blankLineParser :: Parser NObject
blankLineParser = whiteSpace <* eol >> return EmptyLine

opBraceParser :: Parser NObject
opBraceParser = (many (oneOf "\t ") >> char '{' >> whiteSpace) >> return OpBrace

clBraceParser :: Parser NObject
clBraceParser = (many (oneOf "\t ") >> char '}' >> whiteSpace) >> return ClBrace

elseParser :: Parser NObject
elseParser = (many (oneOf "\t ") >> reserved "else" >> whiteSpace) >> return Else

commentParser :: Parser NObject
commentParser = (whiteSpace >> string "--" >> many anyChar) >> return Comment

directionParser :: Parser Direction
directionParser = (reserved "up" >> return UP) <|>
                  (reserved "right" >> return RIGHT) <|>
                  (reserved "left" >> return LEFT) <|>
                  (reserved "down" >> return DOWN) <|>
                  (many alphaNum >> return NONE)

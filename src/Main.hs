{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import           Data.Bits.Lens
import           Data.Bool
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import           Data.Char
import           Data.List
import           Data.Proxy
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word
import           GHC.TypeLits
import           Numeric
import           System.Environment
import           System.Exit
import           System.IO
import           Test.Hspec

newtype Memory (size :: Nat)
  = Memory { _mem :: Vector Word16 }
  deriving (Show, Eq)

newtype Registers (size :: Nat)
  = Registers { _reg :: Vector Word16 }
 deriving (Show, Eq)

merge :: Word8 -> Word8 -> Word16
merge l r = foldl' go 0x0 (zip [15,14..0] bits)
  where
    go acc (n,True) = setBit acc n
    go acc (n,False) = acc
    bits =
      map (testBit l) [7,6..0] ++
      map (testBit r) [7,6..0]

-- | Combine two-byte chunks into Word16
processBits :: [Word8] -> [Word16]
processBits bytes = map go (chunks 2 bytes)
  where
    go [_] = error "odd number"
    go [x,y] = merge x y

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = do
  let (l,r) = splitAt n xs
  l : chunks n r

data R
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | PC
  | Cond
  | Count
  deriving (Eq, Show, Enum)

reg' :: R -> Lens' (Registers (nat :: Nat)) Word16
reg' n = lens (\(Registers v) -> v V.! fromEnum n) setter
  where
    setter (Registers vec) word16 =
      Registers $ vec V.// [(fromEnum n, word16)]

mem' :: Int -> Lens' (Memory (nat :: Nat)) Word16
mem' n = lens (\(Memory v) -> v V.! n) setter
  where
    setter (Memory vec) word16 =
      Memory $ vec V.// [(n, word16)]

data Machine
  = Machine
  { _machineReg :: Registers 11
  , _machineMem :: Memory 65536
  , _machineStatus :: Status
  }

status :: Lens' Machine Status
status =
  lens _machineStatus $ \p x ->
    p { _machineStatus = x }

data Status
  = Running
  | Halt
  deriving (Show, Eq)

reg :: R -> Lens' Machine Word16
reg r = machineReg . reg' r

mem :: Int -> Lens' Machine Word16
mem n = machineMem . mem' n

machineReg :: Lens' Machine (Registers 11)
machineReg =
  lens _machineReg (\m r -> m { _machineReg = r })

machineMem :: Lens' Machine (Memory 65536)
machineMem =
  lens _machineMem (\m r -> m { _machineMem = r })

registers :: forall n . n ~ 11 => Registers n
registers = Registers (V.replicate n 0x0)
  where
    n = fromIntegral $ natVal (Proxy @ n)

memory :: forall n . n ~ 65536 => Memory n
memory = Memory (V.replicate n 0x0)
  where
    n :: Int
    n = fromIntegral $ natVal (Proxy @ n)

data OpCode
  = BR  -- /* branch */
  | ADD -- /* add  */
  | LD  -- /* load */
  | ST  -- /* store */
  | JSR -- /* jump register */
  | AND -- /* bitwise and */
  | LDR -- /* load register */
  | STR -- /* store register */
  | RTI -- /* unused */
  | NOT -- /* bitwise not */
  | LDI -- /* load indirect */
  | STI -- /* store indirect */
  | JMP -- /* jump */
  | RES -- /* reserved (unused) */
  | LEA -- /* load effective address */
  | TRAP -- /* execute trap */
  deriving (Eq, Ord, Show, Enum)

type Addr = Word16
type Val  = Word16

memWrite :: Addr -> Val -> Routine ()
memWrite addr val = mem (fromIntegral addr) .= val

getKey :: IO Char
getKey = getChar

checkKey :: IO (Maybe Word16)
checkKey = do
  result <- B.hGet stdin 2
  case result of
    x | B.null x -> pure Nothing
      | otherwise -> do
          let [l,r] = B.unpack x
          pure $ Just $ (merge l r)
        where
          go (l,r) x n
            | n < 8 = setBit x $ popCount (testBit r n)
            | otherwise = setBit x $ popCount (testBit l n)

memRead :: Addr -> Routine Val
memRead (fromIntegral -> addr)
  | addr == mrKBSR = handleKey
  | otherwise = use $ mem addr
    where
      handleKey = do
        maybeKey <- liftIO checkKey
        case maybeKey of
          Just key -> do
            mem mrKBSR .= 1 `shiftL` 15
            mem mrKBDR .= key
          Nothing ->
            mem mrKBSR .= 0x0
        use (mem addr)

mrKBSR = 0xFE00 -- /* keyboard status */
mrKBDR = 0xFE02 -- /* keyboard data */

pos, zro, neg :: Word16
pos = 1
zro = 2
neg = 4

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  heap <- readImageFile
  let machine = Machine registers heap Running
  finished <- runRoutine machine routine
  print (finished ^. status)

readImageFile :: IO (Memory 65536)
readImageFile = do
  args <- getArgs
  case args of
    fileName : _ -> do
      (origin:bytes) <- processBits . B.unpack <$> B.readFile fileName
      let pad = V.replicate (fromIntegral origin - 1) (0x0 :: Word16)
          mid = V.fromList (origin:bytes)
          end = V.replicate
            (65536 - (V.length pad + V.length mid)) (0x0 :: Word16)
      -- V.mapM_ print $ (fmap getOp mid)
      pure $ Memory (pad <> mid <> end)
    _ -> do
      putStrLn "Please enter path to LC3 program"
      exitFailure

test :: IO ()
test = hspec tests

type Routine = StateT Machine IO

signExtend :: Word16 -> Int -> Word16
signExtend x bitCount
  | (x `shiftL` (bitCount - 1)) .&. 1 == 1
  = x .|. (0xFFFF `shiftR` bitCount)
  | otherwise = x

updateFlags :: R -> Routine ()
updateFlags r = do
  x <- use (reg r)
  case x of
    z | z == 0 -> reg Cond .= zro
      | z ^. bitAt 15 -> reg Cond .= neg
      | otherwise -> reg Cond .= pos

swap16 :: Word16 -> Word16
swap16 x = x `shiftL` 8 .|. x `shiftR` 8

toE :: Enum e => Word16 -> e
toE = toEnum . fromIntegral

getOp :: Word16 -> OpCode
getOp x = toE (x `shiftR` 12)

io :: MonadIO m => IO a -> m a
io = liftIO

routine :: Routine ()
routine = do
  reg PC .= 0x3000
  fix $ \loop -> do
    s <- use status
    unless (s == Halt) $ do
      go
      reg PC += 1
      loop

dumpRegisters :: Routine ()
dumpRegisters = do
  liftIO (putStrLn mempty)
  instr <- memRead =<< use (reg PC)
  Registers r <- gets _machineReg
  liftIO $ do
    putStrLn (showHexAndBinary instr)
    V.mapM_ (\(n,x) -> putStrLn $ show (toEnum n :: R) ++ ": 0x" ++ showHex x "")
      (V.zip (V.fromList [0..10]) r)

debug :: Bool
debug = True

showBinary :: Word16 -> String
showBinary x = "0b" ++ showIntAtBase 2 (head . show) x ""

showHexAndBinary :: Word16 -> String
showHexAndBinary instr =
  show (getOp instr) ++ " -> 0x" ++ showHex instr "" ++ " " ++ showBinary instr

class ToInstr a where
  toInstr :: Word16 -> a

instance ToInstr Br where toInstr = makeBr
instance ToInstr Add where toInstr = makeAdd
instance ToInstr Ld where toInstr = makeLd
instance ToInstr St where toInstr = makeSt
instance ToInstr Jsr where toInstr = makeJsr
instance ToInstr And where toInstr = makeAnd
instance ToInstr Ldr where toInstr = makeLdr
instance ToInstr Str where toInstr = makeStr
instance ToInstr Not where toInstr = makeNot
instance ToInstr Ldi where toInstr = makeLdi
instance ToInstr Sti where toInstr = makeSti
instance ToInstr Jmp where toInstr = makeJmp
instance ToInstr Lea where toInstr = makeLea
instance ToInstr Trap where toInstr = makeTrap

data Add
  = Add
  { dr :: R
  , sr1 :: R
  , sr2 :: R
  } | AddImm
  { dr :: R
  , sr1 :: R
  , imm :: Word16
  } deriving (Show, Eq)

makeAdd :: Word16 -> Add
makeAdd instr = do
  let dr = toE $ (instr `shiftR` 9) .&. 0x7
      sr1 = toE $ (instr `shiftR` 6) .&. 0x7
      sr2 = toE $ instr .&. 0x7
      imm = signExtend (instr .&. 0x1F) 5
  if instr ^. bitAt 5
    then AddImm dr sr1 imm
    else Add dr sr1 sr2

data Ldi
  = Ldi
  { ldiDR :: R
  , ldiPcOffset :: Word16
  } deriving (Show, Eq)

makeLdi :: Word16 -> Ldi
makeLdi instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      pcOffset = signExtend (instr .&. 0x1ff) 9
  Ldi r0 pcOffset

data And
  = And
  { addDr :: R
  , addSr1 :: R
  , addSr2 :: R
  } | AndImm
  { addDr :: R
  , addSr1 :: R
  , addImm :: Word16
  } deriving (Show, Eq)

makeAnd :: Word16 -> And
makeAnd instr = do
  let dr = toE $ (instr `shiftR` 9) .&. 0x7
      sr1 = toE $ (instr `shiftR` 6) .&. 0x7
      sr2 = toE (instr .&. 0x7)
      imm = signExtend (instr .&. 0x1F) 5
  if instr ^. bitAt 5
    then AndImm dr sr1 imm
    else And dr sr1 sr2

data Not
  = Not
  { notDr :: R
  , notSr :: R
  } deriving (Show, Eq)

makeNot :: Word16 -> Not
makeNot instr = do
  let dr = toE $ (instr `shiftR` 9) .&. 0x7
      sr = toE $ (instr `shiftR` 6) .&. 0x7
  Not dr sr

data Br
  = Br
  { brCondFlag :: Word16
  , brPcOffset :: Word16
  } deriving (Show, Eq)

makeBr :: Word16 -> Br
makeBr instr = do
  let condFlag = (instr `shiftR` 9) .&. 0x7
      pcOffset = signExtend (instr .&. 0x1ff) 9
  Br condFlag pcOffset

data Jmp
  = Jmp
  { jrDr :: R
  } deriving (Show, Eq)

makeJmp :: Word16 -> Jmp
makeJmp instr = do
  let r1 = toE $ (instr `shiftR` 6) .&. 0x7
  Jmp r1

data Jsr
  = Jsr
  { jsrR1 :: R
  , jsrPcOffset :: Word16
  , jsrPcFlag :: Word16
  } deriving (Show, Eq)

makeJsr :: Word16 -> Jsr
makeJsr instr = do
  let r1 = toE $ (instr `shiftR` 6) .&. 0x7
      longPCOffset = signExtend (instr .&. 0x7ff) 11
      longFlag = (instr `shiftR` 11) .&. 1
  Jsr r1 longPCOffset longFlag

data Ld
  = Ld
  { ldR0 :: R
  , ldPcOffset :: Word16
  } deriving (Show, Eq)

makeLd :: Word16 -> Ld
makeLd instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      pcOffset = signExtend (instr .&. 0x1ff) 9
  Ld r0 pcOffset

data Ldr
  = Ldr
  { ldrR0 :: R
  , ldrR1 :: R
  , ldrOffset :: Word16
  } deriving (Show, Eq)

makeLdr :: Word16 -> Ldr
makeLdr instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      r1 = toE $ (instr `shiftR` 6) .&. 0x7
      pcOffset = signExtend (instr .&. 0x3F) 6
  Ldr r0 r1 pcOffset

data Lea
  = Lea
  { leaR0 :: R
  , leaPcOffset :: Word16
  } deriving (Show, Eq)

makeLea :: Word16 -> Lea
makeLea instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      pcOffset = signExtend (instr .&. 0x1ff) 9
  Lea r0 pcOffset

data St
  = St
  { stR0 :: R
  , stPcOffset :: Word16
  } deriving (Show, Eq)

makeSt :: Word16 -> St
makeSt instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      pcOffset = signExtend (instr .&. 0x1ff) 9
  St r0 pcOffset

data Sti
  = Sti
  { stiR0 :: R
  , stiPcOffset :: Word16
  } deriving (Show, Eq)

makeSti :: Word16 -> Sti
makeSti instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      pcOffset = signExtend (instr .&. 0x1ff) 9
  Sti r0 pcOffset

data Str
  = Str
  { strR0 :: R
  , strR1 :: R
  , strPcOffset :: Word16
  } deriving (Show, Eq)

makeStr :: Word16 -> Str
makeStr instr = do
  let r0 = toE $ (instr `shiftR` 9) .&. 0x7
      r1 = toE $ (instr `shiftR` 6) .&. 0x7
      pcOffset = signExtend (instr .&. 0x3F) 9
  Str r0 r1 pcOffset

data Trap
  = Trap
  {
  } deriving (Show, Eq)

data TrapOp
  = Putc
  | Getc

makeTrap :: Word16 -> Trap
makeTrap instr = do
  undefined

go :: Routine ()
go = do
  instr <- memRead =<< use (reg PC)
  when debug dumpRegisters
  case getOp instr of
    ADD -> do
      liftIO $ when debug $ print (toInstr instr :: Add)
      case makeAdd instr of
        AddImm dr sr1 imm -> do
          result <- (imm+) <$> use (reg sr1)
          reg dr .= result
          updateFlags dr
        Add dr sr1 sr2 -> do
          r1 <- use (reg sr1)
          r2 <- use (reg sr2)
          reg dr .= r1 + r2
          updateFlags dr
    LDI -> do
      liftIO $ when debug $ print (toInstr instr :: Ldi)
      case makeLdi instr of
        Ldi dr pcOffset -> do
          pcVal <- use (reg PC)
          r <- memRead =<< memRead (pcVal + pcOffset)
          reg dr .= r
          updateFlags dr
    RTI ->
      pure ()
    RES ->
      pure ()
    AND -> do
      liftIO $ when debug $ print (toInstr instr :: And)
      case makeAnd instr of
        AndImm dr sr1 imm -> do
          r <- use (reg sr1)
          reg dr .= r .&. imm
          updateFlags dr
        And dr sr1 sr2 -> do
          r1 <- use (reg sr1)
          r2 <- use (reg sr2)
          reg dr .= r1 .&. r2
          updateFlags dr
    NOT -> do
      liftIO $ when debug $ print (toInstr instr :: Not)
      case makeNot instr of
        Not dr sr -> do
          r <- use (reg sr)
          reg dr .= complement r
    BR -> do
      liftIO $ when debug $ print (toInstr instr :: Br)
      case makeBr instr of
        Br rcCond pcOffset -> do
          rCond <- use (reg Cond)
          when (rcCond .&. rCond > 0)
            (reg PC += pcOffset)
    JMP -> do
      liftIO $ when debug $ print (toInstr instr :: Jmp)
      case makeJmp instr of
        Jmp r -> do
          r1 <- use (reg r)
          reg PC .= r1
          go
    JSR -> do
      liftIO $ when debug $ print (toInstr instr :: Jsr)
      case makeJsr instr of
        Jsr r1 longPCOffset longFlag -> do
          pc <- use (reg PC)
          r <- use (reg r1)
          reg R7 .= pc
          if longFlag == 1
            then reg PC += longPCOffset
            else reg PC .= r
    LD -> do
      liftIO $ when debug $ print (toInstr instr :: Ld)
      case makeLd instr of
        Ld r0 pcOffset -> do
          pc <- use (reg PC)
          r <- memRead (pc + pcOffset)
          reg r0 .= r
          updateFlags r0
    LDR -> do
      liftIO $ when debug $ print (toInstr instr :: Ldr)
      case makeLdr instr of
        Ldr r0 r1 pcOffset -> do
          r1' <- use (reg r1)
          val <- memRead (r1' + pcOffset)
          reg r0 .= val
          updateFlags r0
    LEA -> do
      liftIO $ when debug $ print (toInstr instr :: Lea)
      case makeLea instr of
        Lea r0 offset -> do
          pc <- use (reg PC)
          reg r0 .= pc + offset
    ST -> do
      liftIO $ when debug $ print (toInstr instr :: St)
      case makeSt instr of
        St r0 offset -> do
          pc <- (offset+) <$> use (reg PC)
          r0' <- use (reg r0)
          memWrite pc r0'
    STI -> do
      liftIO $ when debug $ print (toInstr instr :: Sti)
      case makeSti instr of
        Sti r0 offset -> do
          pc <- use (reg PC)
          r0' <- use (reg r0)
          val <- memRead (pc + offset)
          memWrite val r0'
    STR -> do
      liftIO $ when debug $ print (toInstr instr :: Str)
      case makeStr instr of
        Str r0 r1 offset -> do
          r0' <- use (reg r0)
          r1' <- use (reg r1)
          memWrite (r1' + offset) r0'
    TRAP -> do
--      liftIO $ when debug $ print (toInstr instr :: Trap)
      case instr .&. 0xFF of
        t | trapGetc == t -> do
              r <- fromIntegral . ord <$> liftIO getChar
              reg R0 .= r
          | trapPuts == t -> do
              v <- use (reg R0)
              let loop x = do
                    val <- memRead x
                    liftIO $ putStrLn ("val -> 0x" ++ showHex val "")
                    unless (val == 0x0000) $ do
                      let c = chr (fromIntegral val)
                      liftIO (putChar c)
                      loop (x+1)
              loop v
          | trapPutsp == t -> do
              v <- use (reg R0)
              let loop x = do
                    val <- memRead x
                    unless (val == 0x0000) $ do
                      let char1 = chr (fromIntegral (val .&. 0xFF))
                          char2 = chr (fromIntegral (val `shiftR` 8))
                      liftIO $ mapM_ putChar [char1, char2]
                      loop (x+1)
              loop v
          | trapOut == t -> do
              liftIO . putChar =<<
                chr . fromIntegral <$> use (reg R0)
          | trapIn == t -> do
              r <- fromIntegral . ord <$> liftIO getChar
              reg R0 .= r
          | trapHalt == t -> do
              liftIO (putStrLn "HALT")
              status .= Halt
          | otherwise -> do
              liftIO $ do
                print (getOp instr)
                print instr
                exitFailure


pcStart :: Int
pcStart = fromIntegral 0x3000

runRoutine :: Machine -> Routine () -> IO Machine
runRoutine = flip execStateT

-- in the trap

trapGetc :: Word16
trapGetc = 0x20  --  /* get character from keyboard */

trapOut :: Word16
trapOut = 0x21   --  /* output a character */

trapPuts :: Word16
trapPuts = 0x22  --  /* output a word string */

trapIn :: Word16
trapIn = 0x23    --  /* input a string */

trapPutsp :: Word16
trapPutsp = 0x24 --  /* output a byte string */

trapHalt :: Word16
trapHalt = 0x25  --  /* halt the program */

-- | some tests

tests :: Spec
tests = do
  describe "VM tests" $ do
    addTwoNumbers
    addTwoNumbersImm
    andTwoNumbers
    andTwoNumbersImm
    complementNumber

complementNumber :: SpecWith ()
complementNumber =
  it "Should NOT (complement) a number" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` (-2)
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b1001101011000100
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1

andTwoNumbers :: SpecWith ()
andTwoNumbers =
  it "Should AND two numbers" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 0
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0101101011000100
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 5
           & reg' R4 .~ 2

andTwoNumbersImm :: SpecWith ()
andTwoNumbersImm =
  it "Should AND two numbers w/ immediate" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 1
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0101101011111111
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1

addTwoNumbers :: SpecWith ()
addTwoNumbers =
  it "Should ADD two numbers" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 2
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0001101011000100
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1
           & reg' R4 .~ 1

addTwoNumbersImm :: SpecWith ()
addTwoNumbersImm =
  it "Should ADD two numbers w/ immediate" $ do
    r <- runRoutine ma routine
    r ^. reg R5 `shouldBe` 32
      where
        ma = Machine rs me Running
        me = memory
           & mem' 0x3001 .~ 0b0001101011111111
           & mem' 0x3002 .~ haltInstr
        rs = registers
           & reg' R3 .~ 1

haltInstr = 0b1111000000100101

-- k' = signExtend (0b0001101011111111 .&. 0x1F) 5

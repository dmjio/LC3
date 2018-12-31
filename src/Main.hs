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
module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import           Data.Bits.Lens
import           Data.Bool
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import           Data.List
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word
import           GHC.TypeLits
import           System.IO
import           Test.Hspec

newtype Memory (size :: Nat)
  = Memory { _mem :: Vector Word16 }
  deriving (Show, Eq)

newtype Registers (size :: Nat)
  = Registers { _reg :: Vector Word16 }
 deriving (Show, Eq)

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

registers :: Registers 11
registers = Registers (V.replicate 11 0x0)

memory :: Memory 65536
memory = Memory (V.replicate n 0x0)
  where
    n :: Int
    n = fromIntegral (maxBound :: Word16)

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
  result <- B.hGetNonBlocking stdin 2
  pure $ case result of
    x | B.null x -> Nothing
      | otherwise -> do
          let [l,r] = B.unpack x
          Just $ foldl' (go (l,r)) 0x0 [0..15]
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
main = hspec tests

type Routine = StateT Machine IO

signExtend :: Word16 -> Int -> Word16
signExtend x bitCount
  -- shiftL or shiftR? that is the question...
  | x `shiftL` (bitCount - 1) .&. 1 == 1
  = x .|. (0xFFFF `shiftL` bitCount)
  | otherwise = x

updateFlags :: R -> Routine ()
updateFlags r = do
  x <- use (reg r)
  case x of
    z | z == 0 -> reg Cond .= zro
      | z ^. bitAt 15 -> reg Cond .= neg
      | otherwise -> reg Cond .= pos

swap16 :: Bits a => a -> a
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
  fix $ \go -> do
    s <- use status
    unless (s == Halt) $ do
      reg PC += 1
      loop >> go

loop :: Routine ()
loop = do
  instr <- memRead =<< use (reg PC)
  let immMode = instr ^. bitAt 5
  case getOp instr of
    ADD -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      if immMode
        then do
          let imm5 = signExtend (instr .&. 0x1F) 5
          result <- (imm5+) <$> use (reg r1)
          reg r0 .= result
        else do
          let r2 = toE (instr .&. 0x7)
          r1' <- use (reg r1)
          r2' <- use (reg r2)
          reg r0 .= r1' + r2'
      updateFlags r0
    LDI -> do
      let dr = toE $ (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      -- reg[r0] = mem_read(mem_read(reg[R_PC] + pc_offset))
      pcVal <- use (reg PC)
      r <- use . mem . fromIntegral =<< do
        use $ mem (fromIntegral (pcVal + pcOffset))
      reg dr .= r
      updateFlags dr
    RTI ->
      pure ()
    RES ->
      pure ()
    AND -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      if immMode
        then do
          let imm5 = signExtend (instr .&. 0x1F) 5
          r1' <- use (reg r1)
          reg r0 .= r1' .&. imm5
        else do
          let r2 = toE (instr .&. 0x7)
          r1' <- use (reg r1)
          r2' <- use (reg r2)
          reg r0 .= r1' .&. r2'
      updateFlags r0
    NOT -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      r1' <- use (reg r1)
      reg r0 .= complement r1'
    BR -> do
      let condFlag = (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      rCond <- use (reg Cond)
      when (condFlag .&. rCond /= 0) $
        reg PC += pcOffset
    JMP -> do
      let r1 = toE $ (instr `shiftR` 6) .&. 0x7
      r1' <- use (reg r1)
      reg PC .= r1'
    JSR -> do
      let r1 = toE $ (instr `shiftR` 6) .&. 0x7
          longPCOffset = signExtend (instr .&. 0x7ff) 11
          longFlag = (instr `shiftR` 11) .&. 1
      pc <- use (reg PC)
      reg R7 .= pc
      if longFlag == 1
        then reg PC += longPCOffset
        else reg PC .= r1
    LD -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      pc <- use (reg PC)
      r <- memRead (pc + pcOffset)
      reg r0 .= r
      updateFlags r0
    LDR -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
          offset = signExtend (instr .&. 0x3F) 6
      r1' <- use (reg r1)
      val <- memRead (r1' + offset)
      reg r0 .= val
      updateFlags r0
    LEA ->
      pure ()
    TRAP -> do
      case instr .&. 0xFF of
        t | trapGetc == t -> pure ()
          | trapHalt == t -> do
              liftIO (putStrLn "HALT")
              status .= Halt

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

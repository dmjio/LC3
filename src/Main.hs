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
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word
import           GHC.TypeLits
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
  | COND
  | Count
  deriving (Eq, Show, Enum)

reg :: R -> Lens' (Registers (nat :: Nat)) Word16
reg n = lens (\(Registers v) -> v V.! fromEnum n) setter
  where
    setter (Registers vec) word16 =
      Registers $ vec V.// [(fromEnum n, word16)]

mem :: Int -> Lens' (Memory (nat :: Nat)) Word16
mem n = lens (\(Memory v) -> v V.! n) setter
  where
    setter (Memory vec) word16 =
      Memory $ vec V.// [(n, word16)]

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

pos, zro, neg :: Word16
pos = 1
zro = 2
neg = 4

main :: IO ()
main = print ("foo" :: String)

-- data RoutineState
--   = RoutineState
--   { _rsMemory :: Memory 65536
--   , _rsRegisters :: Registers 11
--   } deriving (Show, Eq)
-- makeLenses ''RoutineState

type Routine a = StateT (Registers 11) IO a

memRead :: Word16 -> Routine Word16
memRead = const (pure x)
  where
    x :: Word16
    --  9876543210
    x = 0b0001101011100011

signExtend :: Word16 -> Int -> Word16
signExtend x bitCount
  | x `shiftR` (bitCount - 1) .&. 1 == 1
    = x .|. (0xFFFF `shiftL` bitCount)
  | otherwise = x

updateFlags :: R -> Routine ()
updateFlags r = do
  x <- use (reg r)
  case x of
    z | z == 0 -> reg COND .= zro
      | z ^. bitAt 15 -> reg COND .= neg
      | otherwise -> reg COND .= pos

toE :: Enum e => Word16 -> e
toE = toEnum . fromIntegral

getOp :: Word16 -> OpCode
getOp x = toE (x `shiftR` 12)

routine :: Routine ()
routine = do
  reg PC .= 0x3000
  reg PC += 1
  x <- use (reg PC)
  instr <- memRead x
  let immMode = instr ^. bitAt 5
  case getOp instr of
    ADD -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          r1 = toE $ (instr `shiftR` 6) .&. 0x7
      if immMode
        then do
          let imm5 = signExtend (instr .&. 0x1F) 5
          r1' <- use (reg r1)
          reg r0 .= r1' + imm5
        else do
          let r2 = toE (instr .&. 0x7)
          r1' <- use (reg r1)
          r2' <- use (reg r2)
          reg r0 .= r1' + r2'
      updateFlags r0
    LDI -> do
      let r0 = toE $ (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      -- reg[r0] = mem_read(mem_read(reg[R_PC] + pc_offset));
      updateFlags r0
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
      reg r0 .= negate r1'
    BR -> do
      let condFlag = (instr `shiftR` 9) .&. 0x7
          pcOffset = signExtend (instr .&. 0x1ff) 9
      rCond <- use (reg COND)
      when (condFlag .&. rCond /= 0) $
        reg PC += pcOffset
    JMP -> do
      let r1 = toE $ (instr `shiftR` 6) .&. 0x7
      r1' <- use (reg r1)
      reg PC .= r1'
    JSR -> do
      pure ()

runRoutine :: Routine () -> IO (Registers 11)
runRoutine = flip execStateT $
  registers & reg R3 .~ 1
            & reg R4 .~ 1

tests :: Spec
tests = do
  describe "vm tests" $ do
    it "should add two numbers" $ do
      r <- runRoutine routine
      r ^. reg R5 `shouldBe` 2

module Register where

import Control.Monad.State (State, runStateT, get, put)
import qualified Data.Map as DM (Map, insert, lookup)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Functor.Identity (runIdentity)
import Control.Monad (guard)
import Data.Maybe (isJust)

type Registers = DM.Map Int Int
type RegisterState a = MaybeT (State Registers) a

runReg :: RegisterState a -> Registers -> Maybe Int
runReg rs registers = DM.lookup 0 (snd $ runIdentity $ runStateT (runMaybeT rs) registers)

load :: Int -> RegisterState ()
load x = do
  registers <- get
  put (DM.insert 0 x registers)
  
write :: Int -> RegisterState ()  
write = write_cond isJust
  
write_cond :: (Maybe Int -> Bool) -> Int -> RegisterState ()  
write_cond p register = do
  registers <- get
  let mX0 = DM.lookup 0 registers
  guard $ p mX0
  let x0 = maybe 0 id mX0
  put (DM.insert register x0 registers)  
  
rread :: Int -> RegisterState ()  
rread = read_cond isJust
   
read_cond :: (Maybe Int -> Bool) -> Int -> RegisterState ()  
read_cond p register = do
   registers <- get
   let mXReg = DM.lookup register registers
   guard $ p mXReg
   let xReg = maybe 0 id mXReg
   put (DM.insert 0 xReg registers)      
  
unop :: (Int -> Int) -> RegisterState ()
unop = unop_cond isJust
  
unop_cond :: (Maybe Int -> Bool) -> (Int -> Int) -> RegisterState ()
unop_cond p f = do
  registers <- get
  let mX0 = DM.lookup 0 registers
  guard $ p mX0
  let x0 = maybe 0 id mX0
  put (DM.insert 0 (f x0) registers) 
  
negt = unop (negate)
sign = unop (signum)  
inc = unop (+1)
dec = unop (+(-1))
  
binop :: (Int -> Int -> Int) -> Int -> RegisterState ()
binop = binop_cond areJust
  
binop_cond :: (Maybe Int -> Maybe Int -> Bool) -> (Int -> Int -> Int) -> Int -> RegisterState ()
binop_cond p f register = do
  registers <- get
  let mX0 = DM.lookup 0 registers
  let mXReg = DM.lookup register registers
  guard $ p mX0 mXReg
  let x0 = maybe 0 id mX0
  let xReg = maybe 0 id mXReg
  put (DM.insert 0 (f x0 xReg) registers)
  
areJust :: Maybe Int -> Maybe Int -> Bool
areJust a b = (isJust a) && (isJust b)

non_zero_2nd :: Maybe Int -> Maybe Int -> Bool
non_zero_2nd a b = (areJust a b) && ((maybe 0 id b) /= 0)

add register = binop (+) register
sub register = binop (-) register
mul register = binop (*) register
rdiv register = binop_cond non_zero_2nd div register
rmod register = binop_cond non_zero_2nd mod register
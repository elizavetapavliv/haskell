import Register
import System.IO
import Data.Map (fromList)
 
rs = do
  load 5
  write 2
  inc 
  rread 2
  dec
  mul 2
  write 1
  sign
  sub 1
  negt
  rdiv 2
  add 1 
  rmod 2

main = do
  putStrLn $ "Result: " ++ (show (runReg rs (fromList [])))
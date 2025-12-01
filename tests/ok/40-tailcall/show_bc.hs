import ByteCompile (bcRead, showBC)
import IO

main :: IO ()
main = do
  bc <- bcRead "test_tailcall.bc32"
  putStrLn $ showBC bc

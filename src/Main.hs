import Reach.Parser.Parser
import Reach.Parser.Conv
import Control.Monad.Except

main = do
  a <- readProg "/home/jon/Github/reach/src/Reach/Parser/test.txt"
  print (runExcept $ conv a) 



import Reach.Parser.Parser
import Reach.Parser.Conv
import Reach.Monad
import Reach.Eval.Basic
import Reach.Env

main = do
  a <- readProg "/home/jon/Github/reach/src/test.txt"
  case runExcept $ conv a of
    Left e -> print e
    Right fs -> do
      let (e,s) = toProg fs
      print (fst $ normalB e s)      




import Reach.Parser.Parser
import Reach.Parser.Conv
import Reach.Eval.Basic
import Reach.Eval

import System.Environment
import System.Console.GetOpt

main :: IO ()
main = do
  (fn:_) <- getArgs
  go fn 

go :: FilePath -> IO ()
go fn = do
  a <- readProg fn
  case runExcept $ conv a of
    Left e -> print e
    Right fs -> do
      let (e,s) = toProg fs
      case fst $ normalB e s of
        Left err -> print err
        Right e -> putStrLn . printVal $ e



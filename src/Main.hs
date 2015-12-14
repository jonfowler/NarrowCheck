

import qualified Reach.Parser.Module as P
import qualified Reach.Parser.Conv as C
import Reach.Eval.Gen
import Reach.Eval.Expr
import Reach.Eval.Env
import Reach.Lens
      
import Control.Monad.Except

import System.Environment
import System.Console.GetOpt
import System.IO.Error


data Flag 
  = DataBound Int

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (ReqArg dbound "NUM")
      "data-depth bound"
  ]
  where dbound s 
          | n >= 0 = DataBound n
          | otherwise = error "DataDepth Bound must be positive"
          where n = read s

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (fs, [fn], []) -> go fn fs
    (_, _ , errs) -> error $ concat errs ++
                      usageInfo header options
  where header = "Usage: reach [OPTION...] FILE.rh"

go :: FilePath -> [Flag] -> IO ()
go fn flags = do
  rf <- readFile fn
  m <- P.parseModule rf
  P.checkModule m
  putStrLn (show m)
  let env = C.convModule m
      fid = env ^. funcIds .at' "reach"
  putStrLn (show env)
  let rs = runF fid env
  printResults (take 5 rs)

printResults :: [(Expr, Env)] -> IO ()
printResults = mapM_ (\(e,env) -> putStrLn (showExpr env e ++ " -> " ++ printFVar env 0))

runF :: FId -> Env -> [(Expr, Env)]
runF fid env = runReach (newFVar >>= (\x -> evalLazy (App (Fun fid) (FVar x)))) env
--  case runExcept $ conv a of
--    Left e -> print e
--    Right fs -> do
--      let (e, xs, s) = toProg fs dd 10
--      let res =  map (\s -> map (getExp s) xs)  $ reachF e s 
--      mapM_ (putStrLn . unwords . map printVal) res
--  where
--    (dd:_) = [d | DataBound d <- flags] ++ [100000000]

--printWithErr :: (Show err) => Either err Exp -> String
--printWithErr (Left err) = show err
--printWithErr (Right e) =  printVal e


import qualified Reach.Parser.Module as P
import qualified Reach.Parser.Conv as C
import Reach.Eval.Cont
import Reach.Eval.Expr
import Reach.Eval.Env
import Reach.Lens
import Reach.Eval.Monad
import Reach.Printer
      
import Control.Monad.Except

import Data.Either       

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

toFileName :: [String] -> FilePath
toFileName [a] = a ++ ".hs"
toFileName (a : as) = a ++ "/" ++ toFileName as
  
go :: FilePath -> [Flag] -> IO ()
go fn flags = do
  rf <- readFile fn
  m <- P.parseModule rf
  let fns = map toFileName $ ["Prelude"] : m ^. P.moduleImports
  ms <- mapM (readFile >=> P.parseModule) fns
  m' <- P.mergeModules m ms
  P.checkModule m'
  let env = C.convModule 4 m'
      fid = env ^. funcIds .at' "reach"
      Func allfunc _ = env ^. funcs . at' (env ^. funcIds .at' "test")
      fal = env ^. constrIds . at' "False"
  let rs = runF fid env
  printResults (take 100 .  rights $ rs)
  -- filter (\(Con cid _, _) -> cid == fal) .


printResults :: [(Atom, Env)] -> IO ()
printResults = mapM_ (\(e,env) -> do {putStrLn (showAtom env e ++ " ->"); printFVars (env ^. topFrees) env})

printFVars :: [Int] -> Env -> IO ()
printFVars xs env = mapM_ (\x -> putStrLn ("  " ++ printFVar env x)) xs 

runF :: FId -> Env -> [Either ReachFail (Atom, Env)]
runF fid env = runReach
                 (do
                    evalBase (Fun fid, [])
                 )
                 env


--runF :: FId -> Env -> [(Expr, Env)]
--runF fid env = runReach (newFVar >>= (\x -> evalLazy (App (Fun fid) (FVar x)) Fin)) env

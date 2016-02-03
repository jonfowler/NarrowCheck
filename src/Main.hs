
import qualified Reach.Parser.Module as P
import qualified Reach.Parser.Conv as C
import Reach.Eval.Lazy
import Reach.Eval.Full
import Reach.Eval.Expr
import Reach.Eval.Env
import Reach.Lens
import Reach.Eval.Monad
import Reach.Printer
      
import Control.Monad.Except

import Data.Either       
import Data.Maybe

import System.Environment
import System.Console.GetOpt
import System.IO.Error

data EvalTypes = EvalInterweave       
               | EvalBasic

data Flag 
  = DataBound Int
  | EvalType EvalTypes
  | NoOutput
  | Refute

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (ReqArg dbound "NUM")
      "data-depth bound",
    Option ['e'] ["eval"] (ReqArg evalType "STRING")
      "Evaluation type I/B for Interweaving/Basic",
    Option [] ["NO","nooutput"] (NoArg NoOutput)
      "No output",
    Option [] ["refute"] (NoArg Refute)
      "Refute expression"
  ]
  where dbound s 
          | n >= 0 = DataBound n
          | otherwise = error "DataDepth Bound must be positive"
          where n = read s
        evalType "I" = EvalType EvalInterweave 
        evalType "B" = EvalType EvalBasic 
        evalType "Interweaving" = EvalType EvalInterweave 
        evalType "Basic" = EvalType EvalBasic 
        evalType _ = error "unrecognised evaluation type"

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
  let env = C.convModule dataBound m'
  --    fid = env ^. funcIds .at' "reach"
      fal = env ^. constrIds . at' "False"
      rs = runReach (evalSetup "reach" >>= evalStrat) env
  when (output && not refute) (printResults (rights rs))
  when (output && refute) (printResults . filter (\(Con cid _, _) -> cid == fal) . rights $ rs)
  print (length . rights $ rs)
    where
      dataBound = fromMaybe 4 (listToMaybe [n | DataBound n <- flags])
      evalStrat = case fromMaybe EvalBasic (listToMaybe [es | EvalType es <- flags]) of
        EvalInterweave -> evalBase -- evalBase
        EvalBasic -> evalLazy
      output = null [() | NoOutput <- flags]
      refute = not (null [() | Refute <- flags])

--  printResults (length . rights $ rs)
  -- filter (\(Con cid _, _) -> cid == fal) .


printResults :: [(Atom, Env)] -> IO ()
printResults = mapM_ (\(e,env) -> do {putStrLn (showAtom env e ++ " ->"); printFVars (env ^. topFrees) env})

printFVars :: [Int] -> Env -> IO ()
printFVars xs env = mapM_ (\x -> putStrLn ("  " ++ printFVar env x)) xs 

runF :: FId -> Env -> [Either ReachFail (Atom, Env)]
runF fid env = runReach (evalLazy (Fun fid, [])) env


--runF :: FId -> Env -> [(Expr, Env)]
--runF fid env = runReach (newFVar >>= (\x -> evalLazy (App (Fun fid) (FVar x)) Fin)) env

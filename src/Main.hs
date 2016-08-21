
import qualified Reach.Parser.Module as P
import qualified Reach.Parser.Conv as C
import Reach.Eval.Narrow
import Reach.Eval.Reduce
import Reach.Eval.Expr
import Reach.Eval.Env
--import Reach.Eval.ExprBase
import Reach.Lens
import Reach.Eval.Monad
import Reach.Printer
import Reach.Eval.Generate
import System.Random
      
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
  | ConstBound Int
  | GenNum Int
  | NoOutput
  | Refute
  | ShowFunctions
  | NotSized
  | Sized Int

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (ReqArg dbound "NUM")
      "data-depth bound",
    Option ['c'] [] (ReqArg cbound "NUM")
      "data-depth bound",
    Option ['g'] ["generate"] (ReqArg gen "NUM")
      "Number of solutions to generate", 
    Option [] ["NO","nooutput"] (NoArg NoOutput)
      "No output",
    Option [] ["refute"] (NoArg Refute)
      "Refute expression",
    Option [] ["functions"] (NoArg ShowFunctions)
      "Show 'compiled' functions",
    Option [] ["nosize"] (NoArg NotSized)
      "No size argument",
    Option ['s'] ["size"] (ReqArg siz "NUM")
      "Maximum input size"
  ]
  where dbound s 
          | n >= 0 = DataBound n
          | otherwise = error "DataDepth Bound must be positive"
          where n = read s
        cbound s 
          | n >= 0 = ConstBound n
          | otherwise = error "DataDepth Bound must be positive"
          where n = read s
        gen s
          | n >= 0 = GenNum n
          | otherwise = error "Generation number must be postiive"
          where n = read s
        siz s
          | n >= 0 = GenNum n
          | otherwise = error "eneration number must be postiive"
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
  let fns = map toFileName $ m ^. P.moduleImports
  ms <- mapM (readFile >=> P.parseModule) fns
  m' <- P.mergeModules m ms
  P.checkModule m'
  r <- getStdGen
  let env = C.convModule constBound dataBound m'
      fal = env ^. constrIds . at' "False"
      tr = env ^. constrIds . at' "True"
--      rs = pullfst <$> runStrat env
      gs = evalState (generating 3 (getSol tr)
                     (runStrat env))
                     r
  when showfuncs $ putStrLn (printDoc (printDefs env))
  when output $ printResults (take genNum gs)
  unless output $ print (length (take 100 gs))
--  when (output && not refute) (printResults (rights rs))
--  printAll rs
--  when (output && refute) (printResults . filter (\(Con cid _, _) -> cid == fal) . rights $ rs)
--  print (length . rights $ rs)
    where
      dataBound = fromMaybe 10000 (listToMaybe [n | DataBound n <- flags])
      constBound = fromMaybe 1000000 (listToMaybe [n | ConstBound n <- flags])
      genNum = fromMaybe 100 (listToMaybe [n | GenNum n <- flags])
      runStrat env = runReach (narrowSetup "reach" >>= narrow) env
      showfuncs = not (null [() | ShowFunctions <- flags])
      output = null [() | NoOutput <- flags]
      refute = not (null [() | Refute <- flags])
      getSol tr (Right (Con cid rs), z) | cid == tr = Just (Con cid rs, z)
      getSol _ _ = Nothing


--      (either (const Nothing) (\(Con cid _) -> cid == tr ) . fst)

pullfst :: (Either a b, c) -> Either (a, c) (b, c)
pullfst (Left a, c) = Left (a , c)
pullfst (Right b, c) = Right (b , c)


printResults :: [(Atom, Env Expr)] -> IO ()
printResults = mapM_ (\(e,env) -> putStrLn (showAtom env e ++ " ->" ++ printXVars (env ^. topFrees) env))

printFail :: [(ReachFail, Env Expr)] -> IO ()
printFail = mapM_ (\(e,env) -> putStrLn (show e ++ " ->" ++ printXVars (env ^. topFrees) env))

--printAll = mapM_ (\e -> case e of
--           Left (e, env) -> putStrLn (show e ++ " ->" ++ printFVars (env ^. topFrees) env)
--           Right (e, env) -> putStrLn (showAtom env e ++ " ->" ++ printFVars (env ^. topFrees) env))

                                



--printFVars :: [Int] -> Env Expr -> IO ()
--printFVars xs env = mapM_ (\x -> putStrLn ("  " ++ printFVar env x)) xs 

--runF :: FId -> Env -> [Either ReachFail (Atom, Env)]
--runF fid env = runReach (evalLazy (Fun fid, [])) env


--runF :: FId -> Env -> [(Expr, Env)]
--runF fid env = runReach (newFVar >>= (\x -> evalLazy (App (Fun fid) (FVar x)) Fin)) env

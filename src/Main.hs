
import qualified Overlap.Parser.Module as P
import qualified Overlap.Parser.Conv as C
import Overlap.Eval.Narrow
import Overlap.Eval.Reduce
import Overlap.Eval.Expr
import Overlap.Eval.Env
--import Overlap.Eval.ExprBase
import Overlap.Lens
import Overlap.Eval.Monad
import Overlap.Printer
import Overlap.Eval.Generate
import System.Random
import Data.Time
import Data.Fixed
      
import Control.Monad.Except

import Data.Either       
import Data.Maybe

import System.Environment
import System.Console.GetOpt
import System.IO.Error

import Debug.Trace

data EvalTypes = EvalInterweave       
               | EvalBasic

data Flag 
  = DataBound Int
  | ConstBound Int
  | GenNum Int
  | NoOutput
  | Generate
  | ShowFunctions
  | NotSized
  | Sized Int
  | PropName String
  | BackTrack Int

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (ReqArg dbound "NUM")
      "data-depth bound",
    Option ['c'] [] (ReqArg cbound "NUM")
      "data-depth bound",
    Option ['g'] ["generate"] (NoArg Generate)
      "Number of solutions to generate", 
    Option ['n'] ["number"] (ReqArg number "NUM")
      "Number of solutions to generate", 
    Option [] ["NO","nooutput"] (NoArg NoOutput)
      "No output",
    Option ['p'] ["property"] (ReqArg  PropName "String")
       "Name of property to be tested",
    Option [] ["functions"] (NoArg ShowFunctions)
      "Show 'compiled' functions",
--    Option [] ["nosize"] (NoArg NotSized)
--      "No size argument",
    Option ['s'] ["size"] (ReqArg siz "NUM")
      "Input size argument",
    Option ['b'] ["backtrack"] (ReqArg backtr "NUM")
      "Backtrack number"

  ]
  where dbound s 
          | n >= 0 = DataBound n
          | otherwise = error "DataDepth Bound must be positive"
          where n = read s
        cbound s 
          | n >= 0 = ConstBound n
          | otherwise = error "DataDepth Bound must be positive"
          where n = read s
        number s
          | n >= 0 = GenNum n
          | otherwise = error "Generation number must be postiive"
          where n = read s
        siz s
          | n >= 0 = Sized n
          | otherwise = error "Size must be postiive"
          where n = read s
        backtr s
          | n >= 0 = BackTrack n
          | otherwise = error "Backtrack number must be postiive"
          where n = read s


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (fs, [fn], []) -> go fn fs
    (_, _ , errs) -> error $ concat errs ++
                      usageInfo header options
  where header = "Usage: overlapCheck [OPTION...] FILE.rh"

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

      nt = env ^. constrIds . at' "NoTest"
      sc = env ^. constrIds . at' "Success"
      fl = env ^. constrIds .at' "Fail"
--      rs = pullfst <$> runStrat env
      genRes = evalState (generating backtrack (return . getSol tr) (runStrat env)) r
      propRes = convBool <$> evalState (generating backtrack (getSolProp nt) (runStrat env)) r

      convBool ((Con cid _), z) | cid == sc = Right z
                                | cid == fl = Left z
      convBool _ = error "should be true or false"
--      gs' = evalState (sizedGenerating maxsize backtrack (return . getSol tr) (runSizedStrat env)) r
  when showfuncs $ putStrLn (printDoc (printDefs env))
  if prop
    then do
       x <- getCurrentTime
       let r = [ z | Left z <- take genNum propRes]
       case r of
         [] -> do
           x' <- getCurrentTime
           let timetaken = diffUTCTime x' x
           when output (putStrLn $ "+++ Ok, successfully passed " ++ show genNum ++ " tests in " ++ showDec timetaken 2)
         (z : e) -> do
           when output $ putStrLn "Failed test:" >> printFailure z
       unless output $ print (length r) 
    else do
       when output . printResults $ take genNum genRes
       unless output $ print (length (take genNum genRes))
--  when (output && not refute) (printResults (rights rs))
--  printAll rs
--  when (output && refute) (printResults . filter (\(Con cid _, _) -> cid == fal) . rights $ rs)
--  print (length . rights $ rs)
    where
      dataBound = fromMaybe 10000 (listToMaybe [n | DataBound n <- flags])
      constBound = fromMaybe 1000000 (listToMaybe [n | ConstBound n <- flags])
      maxsize = fromMaybe 100 (listToMaybe [n | Sized n <- flags])
      prop = null [() | Generate <- flags] 
      genNum = fromMaybe 100 (listToMaybe [n | GenNum n <- flags])
      backtrack = fromMaybe 3 (listToMaybe [n | BackTrack n <- flags])
      propName = fromMaybe "check" (listToMaybe [n | PropName n <- flags])
      sizeArg = listToMaybe [n | Sized n <- flags] 

      setupNarrow = maybe narrowSetup sizedSetup sizeArg

      runStrat env = runOverlap (setupNarrow propName
                                     >>= narrow Nothing) env
 --     runSizedStrat env i = runOverlap (narrowSizedSetup i propName >>= narrow Nothing) env

      showfuncs = not (null [() | ShowFunctions <- flags])
      output = null [() | NoOutput <- flags]
--      refute = not (null [() | Refute <- flags])

      getSolProp nt (Right (Con cid es), z) | cid == nt = return Nothing
      getSolProp _ (Right (Con cid' es), z) = return . Just $ (Con cid' es, z)
      getSolProp _ (Left _, z) = return Nothing
      getSolProp _ (e, _) = error $ "Internal: not evaluated "

      evalRes e z = head <$> generating backtrack (return . Just) (runOverlap (narrow Nothing e) z)

      getSol tr (Right (Con cid rs), z) | cid == tr = Just (Con cid rs, z)
      getSol _ (Right (Con _ _), z) = Nothing
      getSol _ (Left _, z) = Nothing
      getSol _ (e, _) = error $ "Internal: not evaluated " ++ show e


showDec :: RealFrac a => a -> Int -> String
showDec n d = show (floor n :: Int) ++ "." ++ show (floor n' :: Int) ++ "s"
                   where n' = (mod' n 1) * (10 ^ d)

--      (either (const Nothing) (\(Con cid _) -> cid == tr ) . fst)

pullfst :: (Either a b, c) -> Either (a, c) (b, c)
pullfst (Left a, c) = Left (a , c)
pullfst (Right b, c) = Right (b , c)

printFailure :: Env Expr -> IO ()
printFailure env = putStrLn $ printXVars (env ^. topFrees) env


printResults :: [(Atom, Env Expr)] -> IO ()
printResults = mapM_ (\(e,env) -> putStrLn (showAtom env e ++ " ->" ++ printXVars (env ^. topFrees) env))

printSizedResults :: [(Int, (Int, (Atom, Env Expr)))] -> IO ()
printSizedResults = mapM_ (\(i ,(j, (e,env))) -> putStrLn (show i ++ " ==> " ++ showAtom env e ++ " ->" ++ printXVars (env ^. topFrees) env))

printFail :: [(OverlapFail, Env Expr)] -> IO ()
printFail = mapM_ (\(e,env) -> putStrLn (show e ++ " ->" ++ printXVars (env ^. topFrees) env))

--printAll = mapM_ (\e -> case e of
--           Left (e, env) -> putStrLn (show e ++ " ->" ++ printFVars (env ^. topFrees) env)
--           Right (e, env) -> putStrLn (showAtom env e ++ " ->" ++ printFVars (env ^. topFrees) env))

                                



--printFVars :: [Int] -> Env Expr -> IO ()
--printFVars xs env = mapM_ (\x -> putStrLn ("  " ++ printFVar env x)) xs 

--runF :: FId -> Env -> [Either OverlapFail (Atom, Env)]
--runF fid env = runOverlap (evalLazy (Fun fid, [])) env


--runF :: FId -> Env -> [(Expr, Env)]
--runF fid env = runOverlap (newFVar >>= (\x -> evalLazy (App (Fun fid) (FVar x)) Fin)) env


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
import Overlap.Eval.Enumerate
import System.Random
import Data.Time.Clock.POSIX
import Data.Fixed
import Control.DeepSeq
import Text.Printf

import Data.Maybe

import System.Environment
import System.Console.GetOpt

data Flag
  = GenNum Int
  | NoOutput
  | Generate
  | ShowFunctions
  | Sized Int
  | PropName String
  | BackTrack Int
  | Enumerate
  | DepthBound Int
  | BackTrackNum

options :: [OptDescr Flag]
options =
  [ Option ['g'] ["generate"] (NoArg Generate)
      "Generate test cases (instead of refuting property)",
    Option ['n'] ["number"] (ReqArg number "NUM")
      "Number of solutions to generate",
    Option ['s'] ["size"] (ReqArg siz "NUM")
      "Input size argument",
    Option ['b'] ["backtrack"] (ReqArg backtr "NUM")
      "Backtrack number",
    Option ['d'] ["depth"] (ReqArg dep "NUM")
      "Maximum constructor depth",
    Option ['e'] ["enumerate"] (NoArg Enumerate)
      "Enumerate solutions",
    Option [] ["NO","nooutput"] (NoArg NoOutput)
      "No printed output",
    Option ['p'] ["property"] (ReqArg  PropName "String")
       "Name of property to be tested",
    Option [] ["functions"] (NoArg ShowFunctions)
      "Output compiled functions"
  ]
  where number s
          | n >= 0 = GenNum n
          | otherwise = error "Generation number must be postiive"
          where n = read s
        siz s
          | n >= 0 = Sized n
          | otherwise = error "Size must be postiive"
          where n = read s
        dep s
          | n >= 0 = DepthBound n
          | otherwise = error "Maximum depth must be postiive"
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
  let envir = C.convModule 100000000 dataBound m'
      fal = envir ^. constrIds . at' "False"
      tr = envir ^. constrIds . at' "True"

      nt = envir ^. constrIds . at' "NoTest"
      sc = envir ^. constrIds . at' "Success"
      fl = envir ^. constrIds .at' "Fail"
--      rs = pullfst <$> runStrat env
      (genRes, (Sum genResBT, Sum genResFails))
            = runWriter (evalStateT (generating genNum backtrack (getSol tr) (runStrat envir)) r)
      (enumRes, enumResBT, enumResFails) = enumerate (getSolProp nt) (runStrat envir)
      (propRes, (Sum propResBT, Sum propResFails)) = runWriter (evalStateT (generating genNum backtrack
                                      (getSolProp nt)
                                      (runStrat envir)) r)
      outputProp = do
       x <- getTime
       let r = [ z | Left z <- convBool <$> propRes]
       when output $ case r of
         [] -> do
           x' <- getTime
           let timetaken = x' - x
           putStrLn $ "+++ Ok, successfully passed " ++ show genNum ++ " tests in " ++ secs timetaken 
         (z : e) ->
           putStrLn "Failed test:" >> printFailure z
       unless output $ print (length propRes)
       unless output . print $ propResBT
       unless output . print $ propResFails
       unless output  $ printTimeDiff x 

      outputEnum = do
        x <- getTime
        let r = [z | Left z <- convBool <$> enumRes]
        case r of
          [] -> do
            x' <- getTime
            let timetaken = x' - x
            when output (putStrLn $ "+++ Ok, successfully enumerated " ++ show (length enumRes) ++ " tests in " ++
                         secs timetaken)
          es ->
            when output $ mapM_ (\e -> putStrLn "Failed test:" >> printFailure e) es
        unless output $ print (length enumRes)
        unless output . print $ enumResBT
        unless output . print $ enumResFails
        unless output  $ printTimeDiff x

      convBool ((Con cid _), z) | cid == sc = Right z
                                | cid == fl = Left z
      convBool _ = error "should be true or false"


  when showfuncs $ putStrLn (printDoc (printDefs envir))
  () <- return (rnf envir)
  if prop
    then if not enum
         then outputProp
         else outputEnum
    else do
       x <- getTime
       when output . printResults $ genRes
       unless output . print . length $ genRes
       unless output . print $ genResBT
       unless output . print $ genResFails
       unless output  $ printTimeDiff x
--  when (output && not refute) (printResults (rights rs))
--  printAll rs
--  when (output && refute) (printResults . filter (\(Con cid _, _) -> cid == fal) . rights $ rs)
--  print (length . rights $ rs)
    where
      dataBound = fromMaybe 10000 (listToMaybe [n | DepthBound n <- flags])
--      constBound = fromMaybe 1000000 (listToMaybe [n | ConstBound n <- flags])
      enum = not (null [() | Enumerate <- flags])
      prop = null [() | Generate <- flags]
      genNum = fromMaybe 100 (listToMaybe [n | GenNum n <- flags])
      backtrack = fromMaybe 3 (listToMaybe [n | BackTrack n <- flags])
      propName = fromMaybe "check" (listToMaybe [n | PropName n <- flags])
      sizeArg = listToMaybe [n | Sized n <- flags]
      btn = not (null [() | BackTrackNum <- flags])

      setupNarrow = maybe narrowSetup sizedSetup sizeArg

      runStrat env = runOverlap (setupNarrow propName
                                     >>= narrow Nothing) env
 --     runSizedStrat env i = runOverlap (narrowSizedSetup i propName >>= narrow Nothing) env

      showfuncs = not (null [() | ShowFunctions <- flags])
      output = null [() | NoOutput <- flags]
--      refute = not (null [() | Refute <- flags])

      getSolProp nt (Right (Con cid es), z) | cid == nt = Nothing
      getSolProp _ (Right (Con cid' es), z) = Just $ (Con cid' es, z)
      getSolProp _ (Left _, z) = Nothing
      getSolProp _ (e, _) = error $ "Internal: not evaluated "

--      evalRes e z = head <$> generating backtrack (return . Just) (runOverlap (narrow Nothing e) z)

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
printResults = mapM_ (\(e,env) -> putStrLn (showAtom env e ++ " ->" ++
                                       printXVars (env ^. topFrees) env))

printSizedResults :: [(Int, (Int, (Atom, Env Expr)))] -> IO ()
printSizedResults = mapM_ (\(i ,(j, (e,env))) -> putStrLn (show i ++ " ==> " ++ showAtom env e ++ " ->" ++ printXVars (env ^. topFrees) env))

printFail :: [(OverlapFail, Env Expr)] -> IO ()
printFail = mapM_ (\(e,env) -> putStrLn (show e ++ " ->" ++ printXVars (env ^. topFrees) env))

--printAll = mapM_ (\e -> case e of
--           Left (e, env) -> putStrLn (show e ++ " ->" ++ printFVars (env ^. topFrees) env)
--           Right (e, env) -> putStrLn (showAtom env e ++ " ->" ++ printFVars (env ^. topFrees) env))


printTimeDiff :: Double -> IO ()
printTimeDiff x = do
  x' <- getTime
  print (x' - x)


getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with ::  Double -> String -> String
           with t u | t >= 1e9  = printf "%.4g %s" t u
                    | t >= 1e6  = printf "%.0f %s" t u
                    | t >= 1e5  = printf "%.1f %s" t u
                    | t >= 1e4  = printf "%.2f %s" t u
                    | t >= 1e3  = printf "%.3f %s" t u
                    | t >= 1e2  = printf "%.4f %s" t u
                    | t >= 1e1  = printf "%.5f %s" t u
                    | otherwise = printf "%.6f %s" t u



--printFVars :: [Int] -> Env Expr -> IO ()
--printFVars xs env = mapM_ (\x -> putStrLn ("  " ++ printFVar env x)) xs 

--runF :: FId -> Env -> [Either OverlapFail (Atom, Env)]
--runF fid env = runOverlap (evalLazy (Fun fid, [])) env


--runF :: FId -> Env -> [(Expr, Env)]
--runF fid env = runOverlap (newFVar >>= (\x -> evalLazy (App (Fun fid) (FVar x)) Fin)) env

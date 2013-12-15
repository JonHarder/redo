{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, catchJust, IOException)
import Control.Monad (filterM, liftM, unless, guard)
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Digest.Pure.MD5 (md5)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (createDirectoryIfMissing, renameFile, removeFile, removeDirectoryRecursive, getDirectoryContents, doesFileExist)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName, (</>))
import System.IO (hPutStrLn, stderr, hGetLine, withFile, IOMode(..))
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))

traceShow' :: forall b. Show b => b -> b
traceShow' arg = traceShow arg arg

metaDir :: FilePath
metaDir = ".redo"

main :: IO ()
main = do
  mapM_ redo =<< getArgs
  progName <- getProgName
  redoTarget' <- lookupEnv "REDO_TARGET"
  case (progName, redoTarget') of
    ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable."
    _ -> return ()
 where writeMD5  dep redoTarget = writeFile (metaDir </> redoTarget </> dep) =<< md5' dep

redo :: String -> IO ()
redo target = do
   upToDate' <- upToDate target
   unless upToDate' $ maybe missingDo redo' =<< doPath target
 where redo' :: FilePath -> IO ()
       redo' path = do
         catchJust (guard . isDoesNotExistError)
           (removeDirectoryRecursive metaDepsDir)
               (\_ -> return ())
         createDirectoryIfMissing True metaDepsDir
         writeFile (metaDepsDir </> path) =<< md5' path
         oldEnv <- getEnvironment
         let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
         (_,_,_, processHandle) <- createProcess (shell $ cmd path) {env = Just newEnv}
         exit <- waitForProcess processHandle
         case exit of
               ExitSuccess -> renameFile tmp target
                   -- putStrLn $ "Redo for target '" ++ target ++ "' was successfull"
               ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                                      removeFile tmp
       tmp = target ++ "---redoing"
       missingDo = do
         exists <- doesFileExist target
         unless exists $ error $ "No .do file found for target '" ++ target ++ "'"
       cmd path = unwords ["sh -x", path ,"0", takeBaseName target, tmp ,">", tmp]
       metaDepsDir = metaDir </> target

doPath :: FilePath -> IO (Maybe FilePath)
doPath target = listToMaybe `liftM` filterM doesFileExist candidates
    where candidates = ( target ++ ".do" ) : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
upToDate target = catch
    (do exists <- doesFileExist target
        if exists
        then do deps <- getDirectoryContents (metaDir </> target)
                (traceShow' . and) `liftM` mapM depUpToDate deps
        else return False)
    (\(_ :: IOException) -> return False)
 where depUpToDate :: FilePath -> IO Bool
       depUpToDate dep = catch 
           (do oldMD5 <- map toLower `liftM` withFile (metaDir </> target </> dep) ReadMode hGetLine
               newMD5 <- md5' dep
               doScript <- doPath dep
               case doScript of
                 Nothing -> return $ oldMD5 == newMD5
                 Just _ -> do upToDate' <- upToDate dep
                              return $ (oldMD5 == newMD5) && upToDate')
           (\ e -> return (ioeGetErrorType e == InappropriateType))


md5' :: FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path

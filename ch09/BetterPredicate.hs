import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (SomeException(..), bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import RecursiveContents (getRecursiveContents)

type Predicate =  FilePath      -- path to directory entry
               -> Permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            permissions <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return $ p name permissions size modified

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(SomeException _) -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return $ Just size

-- unsafe because openFile will error if file doesn't exist or can't be opened
simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

-- unsafe because handler does not close the file
-- it could open without error but then hFileSize could throw an error
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(SomeException _) -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return $ Just size

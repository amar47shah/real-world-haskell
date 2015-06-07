import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (SomeException(..), bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import RecursiveContents (getRecursiveContents)

type Predicate =  InfoP Bool

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

-- type synonym
type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> UTCTime
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

-- alternative definition
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP  = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

-- alternative definition of liftP, in terms of liftP2
liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f = liftP2 q f . constP
--liftP' q f k = liftP2 q f $ constP k
--liftP' q f k w x y z = f w x y z `q` constP k w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

(&&?) = andP
(||?) = orP
(==?) = equalP
(>?)  = greaterP
-- (<?)  = lesserP

infixr 3 &&?
infixr 2 ||?
infix  4 ==?
infix  4 >?

-- Examples

myTest path _ (Just size) _ =
  takeExtension path == ".hs" && size > 1024
myTest _ _ _ _ = False

myTest2 = (liftPath takeExtension `equalP` ".hs")
          `andP`
          (sizeP `greaterP` 1024)

myTest3 = liftPath takeExtension ==? ".hs" &&? sizeP >? 1024

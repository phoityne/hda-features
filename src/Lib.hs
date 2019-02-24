module Lib where

import Text.Parsec
import qualified Control.Exception.Safe as E

stepInToFunction :: IO Int
stepInToFunction = do
    
  putStrLn "Stepped in ?"

  return 0


  
-- |
--
data DiskSize = DiskSize {
    deviceDiskSize :: String
  , sizeDiskSize :: Double
  , unitDiskSize :: String
  } deriving (Show, Read, Eq, Ord)

-- |
--
getByteSize :: String -> Either String DiskSize
getByteSize input = case parse diskSizeParser "diskSizeParser" input of
  Left   err -> Left $ show err
  Right  val -> Right val
  where
    diskSizeParser = do
      dev  <- manyTill anyChar $ string " "
      val  <- try pointed <|> many1 digit
      unit <- manyTill anyChar eof

      return $ DiskSize dev (read val) unit

    pointed = do
      val1  <- many1 digit 
      char '.'
      val2  <- many1 digit

      return $! val1 ++ "." ++ val2


-- |
-- 
exceptionTest :: IO ()
exceptionTest = flip E.catchAny eHdl $ do

  putStrLn "exceptionTest called."

  flip E.finally finalize $ run

  where
    finalize = do
      putStrLn "finalize called."
      return ()

    eHdl :: E.SomeException -> IO ()
    eHdl e = do
      putStrLn $ show e
      E.throwIO $ userError "throw exception."

    run = do
      putStrLn "run called."

      fail "exception test"


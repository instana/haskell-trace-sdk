module Instana.SDK.IntegrationTest.Util
  ( putStrFlush
  ) where


import           System.IO (hFlush, stdout)


putStrFlush :: String -> IO ()
putStrFlush s = do
  putStr s
  hFlush stdout


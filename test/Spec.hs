import Lib

main :: IO ()
main = do
  
  putStrLn "Introducing haskell debug adapter features."

  putStrLn "Hello"

  stepInToFunction

  putStrLn "Haskell"
  
  let input = [ "/dev/sda 10.0MB"
              , "/dev/sdb 50.5GB"
              , "/dev/sdc 5TB"
              , "this should be error."
              ]

  mapM showByteSize $ map getByteSize input

  putStrLn "End of Introduction. Thank you."

  where
    showByteSize (Left err) = print err
    showByteSize (Right d)  = showByteSize' d

    showByteSize'   (DiskSize _ val "")   = print val
    showByteSize' d@(DiskSize _ val "KB") = showByteSize' d { 
                                            sizeDiskSize = val*1024
                                          , unitDiskSize = ""}
    showByteSize' d@(DiskSize _ val "MB") = showByteSize' d {
                                            sizeDiskSize = val*1024
                                          , unitDiskSize = "KB"}
    showByteSize' d@(DiskSize _ val "GB") = showByteSize' d {
                                            sizeDiskSize = val*1024
                                          , unitDiskSize = "MB" }
    showByteSize' d = print $ "unsupported. " ++ show d

    

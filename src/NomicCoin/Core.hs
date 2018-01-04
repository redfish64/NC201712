module Core where


startSystem :: IO (WBConf NCKey NCObj)
startSystem =
  do
    wbc <- createWBConf ncActionFunc
    

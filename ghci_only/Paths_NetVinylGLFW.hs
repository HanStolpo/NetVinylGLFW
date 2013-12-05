module Paths_NetVinylGLFW where

getDataFileName :: FilePath -> IO FilePath
getDataFileName "Simple2D.frag" = return "Simple2D.frag"
getDataFileName "Simple2D.vert" = return "Simple2D.vert"
getDataFileName s = error $ "unknown file " ++ s

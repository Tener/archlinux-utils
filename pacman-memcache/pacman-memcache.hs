module Main where

import Control.DeepSeq
import System.IO
import System.Directory.Tree
 
main = do
  tree <- readDirectoryWith readFile' "/var/lib/pacman/sync"
  tree `deepseq` return ()

instance (NFData a) => NFData (AnchoredDirTree a) where
    rnf (fp :/ dt) = rnf (fp,dt)

instance (NFData a) => NFData (DirTree a) where
    rnf (Failed p1 p2) = rnf p1
    rnf (Dir p1 p2) = rnf (p1,p2)
    rnf (File p1 p2) = rnf (p1,p2)

readFile' fn = do
  h <- openFile fn ReadMode
  s <- hGetContents h
  let slen = (length s)
  slen `deepseq` hClose h
  return slen
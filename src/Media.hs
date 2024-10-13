{-
   Media.hs (adapted from playfile.c in freealut)
   Copyright (c) Sven Panne 2005-2016
   This file is part of the ALUT package & distributed under a BSD-style license.
   See the file LICENSE.
-}

module Media (createMainThemeSource, createTimerDingSource) where

import Control.Monad ( unless )
import Data.List ( intersperse )
import Sound.ALUT
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

createMainThemeSource :: FilePath -> IO Source
createMainThemeSource fileName = do
  -- The buffer contains the bytes representing the audio to play.
  buf <- createBuffer (File fileName)

  -- We have to associate the buffer with a source, which controls playback.
  source <- genObjectName
  buffer source $= Just buf

  -- We want the main theme to keep repeating so we enable looping.
  loopingMode source $= Looping 

  -- Normally nothing should go wrong above, but one never knows...
  errs <- get alErrors
  unless (null errs) $ do
     hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
     exitFailure

  return source

createTimerDingSource :: FilePath -> IO Source
createTimerDingSource fileName = do
  buf <- createBuffer (File fileName)
  source <- genObjectName
  buffer source $= Just buf

  -- This one does not loop.
  errs <- get alErrors
  unless (null errs) $ do
     hPutStrLn stderr (concat (intersperse "," [ d | ALError _ d <- errs ]))
     exitFailure

  return source

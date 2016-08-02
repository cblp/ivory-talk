#!/usr/bin/env stack
-- stack --resolver=lts-6.10 runhaskell --package=shake --package=shakespeare
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map                        as Map
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Blaze.Html.Renderer.String
import           Text.Hamlet.Runtime

main :: IO ()
main = shakeArgs shakeOptions $ do

    want ["ivory-talk.html"]

    "*.html" %> \outFile -> do
        let srcFile = outFile -<.> "hamlet"
        let layoutFile = "revealjs.hamlet"
        need [srcFile, layoutFile, "build.hs"]
        slides <- toHamletData <$> hamletFile srcFile mempty
        let env = Map.singleton "slides" slides
        out <- hamletFile layoutFile env
        writeFile' outFile $ renderHtml out

  where
    hamletFile srcFile hamletData = liftIO $ do
        src <- readHamletTemplateFile defaultHamletSettings srcFile
        renderHamletTemplate src hamletData

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
            layoutFile = "revealjs.hamlet"
        need [srcFile, layoutFile, "build.hs"]
        -- , "--to=revealjs"
        -- , "--variable=theme:serif"
        -- , "--variable=width:'100%'"
        src <- liftIO $ readHamletTemplateFile defaultHamletSettings srcFile
        slides <- liftIO $ renderHamletTemplate src mempty
        layout <- liftIO $ readHamletTemplateFile defaultHamletSettings layoutFile
        out <- liftIO $ renderHamletTemplate layout (Map.fromList [("slides", toHamletData slides)])
        writeFile' outFile $ renderHtml out

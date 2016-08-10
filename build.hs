#!/usr/bin/env stack
-- stack runhaskell --package=.
{-# OPTIONS -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.List
import qualified Data.Map                        as Map
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Blaze.Html.Renderer.String
import           Text.Hamlet.Runtime

main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["ivory-talk.html", "code/.tested"]

    "*.html" %> \outFile -> do
        let srcFile = outFile -<.> "hamlet"
        let layoutFile = "revealjs.hamlet"
        need [srcFile, layoutFile]
        slides <- toHamletData <$> renderHamletFile srcFile mempty
        let env = Map.singleton "slides" slides
        out <- renderHamletFile layoutFile env
        writeFile' outFile $ renderHtml out

    "code/.tested" %> \out -> do
        let examplesDir = "code"
        codeExamples <- getDirectoryFiles examplesDir ["*.hs"]
        need [examplesDir </> file -<.> "tested" | file <- codeExamples]
        writeFile' out ""

    "code/*.tested" %> \testedFile -> do
        let exeFile = testedFile -<.> "exe"
            expectFile = testedFile -<.> "expect"
        need [exeFile, expectFile]
        Stdout out <- command [] exeFile []
        expect <- readFile' expectFile
        when (out /= expect) $
            fail "Can't satisfy expectation"
        writeFile' testedFile ""

    "code/*.exe" %> \exeFile -> do
        let cFile = exeFile -<.> "c"
        need [cFile]
        command_ [Traced $ "gcc " ++ cFile]
            "gcc" ["-Wall", "-Wextra", "-Werror", "-DIVORY_TEST", "-o", exeFile, cFile]

    "code/*.c" %> \cFile -> do
        let hsFile = cFile -<.> "hs"
        need [hsFile]
        Stdout cCode <-
            command [Traced $ "runhaskell " ++ hsFile]
                "runhaskell" ["-Wall", "-Werror", hsFile, "--src-dir=code"]
        when ("languagec-" `isPrefixOf` takeFileName cFile) $
            writeFile' cFile cCode

  where

    renderHamletFile srcFile hamletData =
        traced ("renderHamletFile " ++ srcFile) $ do
            src <- readHamletTemplateFile defaultHamletSettings srcFile
            renderHamletTemplate src hamletData

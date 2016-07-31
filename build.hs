#!/usr/bin/env stack
-- stack --resolver=lts-6.10 runhaskell --package=shake
{-# OPTIONS -Wall -Werror #-}

import           Development.Shake
import           Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do

    want ["ivory-talk.html"]

    "*.html" %> \html -> do
        let md = html -<.> "md"
        need [md, "build.hs"]
        cmd "pandoc"  [ "--output=" ++ html
                      , "--standalone"
                      , "--to=revealjs"
                      , "--variable=theme:serif"
                      , "--variable=width:'100%'"
                      , md
                      ]

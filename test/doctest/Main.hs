module Main (main) where

import Test.DocTest qualified as DT

main :: IO ()
main = DT.doctest args
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Refined.hs"
  ]

exts :: [String]
exts =
  [ "-XDataKinds",
    "-XDerivingVia",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XMultiParamTypeClasses",
    "-XPatternSynonyms",
    "-XOverloadedStrings",
    "-XStandaloneKindSignatures",
    "-XScopedTypeVariables",
    "-XTypeApplications",
    "-XTypeFamilies",
    "-XTypeOperators"
  ]
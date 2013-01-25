{-# LANGUAGE UnicodeSyntax #-}
module Main where

import ALife.Creatur.Database.FileSystemQC (test)
import ALife.Creatur.UtilQC (test)
import ALife.Creatur.Genetics.CodeQC (test)
import ALife.Creatur.Genetics.CrossoverQC (test)

import Test.Framework as TF (defaultMain, Test)

tests ∷ [TF.Test]
tests = 
  [ 
    ALife.Creatur.Database.FileSystemQC.test,
    ALife.Creatur.UtilQC.test,
    ALife.Creatur.Genetics.CodeQC.test,
    ALife.Creatur.Genetics.CrossoverQC.test
  ]

main ∷ IO ()
main = defaultMain tests

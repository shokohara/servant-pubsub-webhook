{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
-- import Options.Applicative
-- import Data.Semigroup ((<>))
-- import Option
-- import GHC.Int
--
-- percentOpt :: Parser Float
-- percentOpt = option auto (long "percent" <> help "Float")
--
-- gbOpt :: Parser Int64
-- gbOpt = option auto (long "gb" <> help "Int64")
--
-- sample :: Parser Option
-- sample = Option <$> gbOpt <*> percentOpt
--
-- opts :: ParserInfo Option
-- opts = info (sample <**> helper) ( fullDesc
--   <> progDesc "Print a greeting for TARGET"
--       <> header "hello - a test for optparse-applicative" )
--
-- main2 :: IO ()
-- main2 = execParser opts >>= run
--
-- main3 :: IO ()
-- main3 = do
--   options <- execParser opts
--   forever $ do
--     _ <- App.run options
--     threadDelay (1 * 1000000)

main :: IO ()
main = run


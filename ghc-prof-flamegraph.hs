{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Applicative ((<*>), optional, many, pure)
import           Data.Foldable (traverse_)
import           Data.Functor ((<$>))
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import qualified Options.Applicative as Opts
import qualified ProfFile as Prof
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), replaceExtension)
import           System.IO (stderr, stdout, hPutStrLn, hPutStr, hGetContents, IOMode(..), hClose, openFile)
import           System.Process (proc, createProcess, CreateProcess(..), StdStream(..), waitForProcess)

import Paths_reflex_prof_flamegraph (getDataDir)

data Options = Options
  { optionsProfFile        :: Maybe FilePath
  , optionsOutputFile      :: Maybe FilePath
  , optionsFlamegraphFlags :: [String]
  } deriving (Eq, Show)

optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> optional
        (Opts.strArgument
          (Opts.metavar "RFPROF-FILE" <>
           Opts.help "Reflex profiling output to format as flame graph"))
  <*> optional
        (Opts.strOption
          (Opts.short 'o' <>
           Opts.long "output" <>
           Opts.metavar "SVG-FILE" <>
           Opts.help "Optional output file"))
  <*> many
        (Opts.strOption
          (Opts.long "flamegraph-option" <>
           Opts.metavar "STR" <>
           Opts.help "Options to pass to flamegraph.pl"))

-- addUnknown :: ReportType -> (Int, [String]) -> [String]
-- addUnknown Time   = \(entries, frames) ->
--   let unknown = 1000 - entries
--   in if unknown > 0
--      then ("UNKNOWN " ++ show unknown) : frames
--      else frames
-- addUnknown Alloc   = \(entries, frames) ->
--   let unknown = 1000 - entries
--   in if unknown > 0
--      then ("UNKNOWN " ++ show unknown) : frames
--      else frames
-- addUnknown _ = snd

generateFrames :: [Prof.Line] -> [String]
generateFrames lines0 = snd $ go [] lines0
  where
    go :: [String] -> [Prof.Line] -> (Int, [String])
    go _stack [] =
      (0, [])
    go stack (line : lines') =
      let entries = Prof.lIndividualEventCount line
          symbol = Prof.lLabel line
          frame = intercalate ";" (reverse (symbol : stack)) ++ " " ++ show entries
          (childrenEntries, childrenFrames) = go (symbol : stack) (Prof.lChildren line)
          (restEntries, restFrames) = go stack lines'
      in (entries + childrenEntries + restEntries, frame : childrenFrames ++ restFrames)

main :: IO ()
main = do
  options <- Opts.execParser $
    Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  s <- maybe getContents readFile $ optionsProfFile options
  case Prof.parse s of
    Left err -> error err
    Right ls -> do
      dataDir <- getDataDir
      let flamegraphPath = dataDir </> "FlameGraph" </> "flamegraph.pl"
          flamegraphProc = (proc "perl" (flamegraphPath : optionsFlamegraphFlags options))
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            }
      (outputHandle, outputFileName, closeOutputHandle) <-
        case (optionsOutputFile options, optionsProfFile options) of
          (Just path, _)         -> do
            h <- openFile path WriteMode
            pure (h, Just path, hClose h)
          (Nothing,   Just path) -> do
            let path' = path `replaceExtension` "svg"
            h <- openFile path' WriteMode
            pure (h, Just path', hClose h)
          _                      ->
            pure (stdout, Nothing, pure ())
      (Just input, Just flamegraphResult, Nothing, procHandle) <- createProcess flamegraphProc
      traverse_ (hPutStrLn input) $ generateFrames ls
      hClose input
      hGetContents flamegraphResult >>= hPutStr outputHandle
      exitCode <- waitForProcess procHandle
      closeOutputHandle
      case exitCode of
        ExitSuccess   ->
          case outputFileName of
            Nothing   -> pure ()
            Just path -> putStrLn $ "Output written to " <> path
        ExitFailure{} ->
          hPutStrLn stderr $ "Call to flamegraph.pl at " <> flamegraphPath <> " failed"

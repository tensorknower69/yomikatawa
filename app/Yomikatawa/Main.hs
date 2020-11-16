{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.Catch
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Options.Applicative
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Yomikatawa
import Text.URI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T

data ProgramOptions -- ^for scalability
	= Search'ProgramOptions
		{ category'Search'ProgramOptions :: Category
		, inputWord'Search'ProgramOptions :: T.Text
		, printRomaji'Search'ProgramOptions :: Bool
		, printRandomWords'Search'ProgramOptions :: Bool
		, printSameReadingWords'Search'ProgramOptions :: Bool
		}
	| Version'ProgramOptions
	deriving (Show, Eq)

programOptionsParser :: Parser ProgramOptions
programOptionsParser = version <|> search
	where
	version = flag' Version'ProgramOptions
		( short 'v'
		<> long "version"
		)
	search = Search'ProgramOptions <$> search_category <*> search_input_word <*> print_romaji <*> print_random_words <*> print_same_reading_words
	search_category :: Parser Category
	search_category = sei <|> mei <|> kanji <|> pure Kanji
		where
		sei = flag' Sei
			( short 's'
			<> long "sei"
			<> help "Set category to 性"
			)
		mei = flag' Mei
			( short 'm'
			<> long "mei"
			<> help "Set category to 名"
			)
		kanji = flag' Kanji
			( short 'k'
			<> long "kanji"
			<> help "Set category to 漢字, this is the default category"
			)
	search_input_word = T.pack <$> argument str
		( metavar "INPUT_WORD"
		)
	print_romaji = switch
		( short 'r'
		<> long "print-romaji"
		<> help "Print romaji"
		)
	print_random_words = switch
		( short 'R'
		<> long "print-random-words"
		<> help "Print random words"
		)
	print_same_reading_words = switch
		( short 'e'
		<> long "print-same-reading-words"
		<> help "Print words with same reading"
		)

genericUserAgent :: BS.ByteString
genericUserAgent = "python-requests/2.25.0"

data ScalpelException
	= ScalpelException BL.ByteString
	deriving (Show, Eq)
instance Exception ScalpelException

main :: IO ()
main = do
	execParser $ info (programOptionsParser <**> helper) (fullDesc <> progDesc "A haskell CLI for https://yomikatawa.com")
	>>= \case
		Version'ProgramOptions -> putStrLn "0.1.0.1"
		Search'ProgramOptions{..} -> do
			uri <- mkSearchURI category'Search'ProgramOptions inputWord'Search'ProgramOptions
			mgr <- newTlsManager
			req' <- parseRequest (renderStr uri)
			let req = req'
				{ requestHeaders =
					[ (hUserAgent, genericUserAgent)
					, (hHost, "yomikatawa.com")
					, (hAcceptEncoding, "gzip, deflate")
					, (hAccept, "*/*")
					]
				}
			rep <- httpLbs req mgr

			case scrapeStringLike (responseBody rep) yomikatawaScraper of
				Nothing -> throwM $ ScalpelException (responseBody rep)
				Just (Left ErrorOccurred) -> do
					putStrLn $ "Error occurred due to input: " <> T.unpack inputWord'Search'ProgramOptions <> "\nMost likely due to invalid input like '+'."
				Just (Left err) -> throwM err
				Just (Right r) -> do
					when (resultMayNotBeCorrect r) $ do
						putStrLn $ "[!] The translation may not be correct, please check with other dictionary sites."
					T.putStrLn $ "Hiragana: " <> resultHiragana r
					when printRomaji'Search'ProgramOptions $ case resultRomaji r of
						Nothing -> pure ()
						Just y -> T.putStrLn $ "Romaji: " <> y
					when (printSameReadingWords'Search'ProgramOptions && (not . null $ resultSameReadingWords r)) $ do
						T.putStrLn $ "Same reading: " <> (T.intercalate ", " $ resultSameReadingWords r)
					when (printRandomWords'Search'ProgramOptions && (not . null $ resultRandomWords r)) $ do
						T.putStrLn $ "Random words: " <> (T.intercalate ", " $ resultRandomWords r)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.Catch
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Version
import Options.Applicative
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Yomikatawa
import Text.URI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

data ProgramOptions -- ^for scalability
	= Search'ProgramOptions
		{ category'Search'ProgramOptions :: Category
		, inputWord'Search'ProgramOptions :: T.Text
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
	search = Search'ProgramOptions <$> search_category <*> search_input_word
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
		Version'ProgramOptions -> putStrLn "0.1.0.0"
		Search'ProgramOptions category input_word -> do
			uri <- mkSearchURI category input_word
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
				Just (Left err) -> throwM err
				Just (Right x) -> do
					T.putStrLn $ "Hiragana: " <> resultHiragana x
					case resultRomaji x of
						Nothing -> pure ()
						Just y -> T.putStrLn $ "Romaji: " <> y
					unless (null $ resultSameReadingWords x) $ do
						T.putStrLn $ "Same reading: " <> (T.intercalate ", " $ resultSameReadingWords x)
					unless (null $ resultRandomWords x) $ do
						T.putStrLn $ "Random words: " <> (T.intercalate ", " $ resultRandomWords x)

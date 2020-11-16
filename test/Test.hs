{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.HTML.Scalpel.Core
import Text.HTML.Scalpel.Yomikatawa
import Text.URI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

destDir :: FilePath
destDir = "test_download"

tests :: Manager -> TestTree
tests mgr = testGroup "yomikatawa"
	[ testGroup "kanji"
		[ testCase "読み方" $ do
			r <- searchThrow mgr Kanji "読み方"
			assertBool "Correct" (not $ resultMayNotBeCorrect r)
			assertEqual "Input word" (resultInputWord r) "読み方"
			assertEqual "Hiragana" (resultHiragana r) "よみかた"
		, testCase "辛かった" $ do
			r <- searchThrow mgr Kanji "辛かった"
			assertBool "May not be correct alert" (resultMayNotBeCorrect r)
			assertEqual "Input word" (resultInputWord r) "辛かった"
			assertEqual "Hiragana" (resultHiragana r) "からかった"
			-- assertEqual "Hiragana" $ any (resultHiragana r ==) ["からかった", "つらかった"]
		, testCase "+" $ do
			search mgr Kanji "+" >>= \case
				Left ErrorOccurred -> pure ()
				x -> assertFailure $ "Unknown result: " <> show x
		]
	]

data ScalpelException = ScalpelException BL.ByteString
	deriving (Show, Eq)
instance Exception ScalpelException

genericUserAgent :: BS.ByteString
genericUserAgent = "python-requests/2.25.0"

searchThrow :: (MonadIO m, MonadThrow m) => Manager -> Category -> T.Text -> m Result
searchThrow mgr c x = search mgr c x >>= \case
	Left err -> throwM $ err
	Right y -> pure y

search :: (MonadIO m, MonadThrow m) => Manager -> Category -> T.Text -> m (Either YomikatawaException Result)
search mgr c x = do
	uri <- mkSearchURI c x
	req' <- liftIO $ parseRequest (renderStr uri)
	let req = req'
		{ requestHeaders =
			[ (hUserAgent, genericUserAgent) -- required
			, (hHost, "yomikatawa.com")
			, (hAcceptEncoding, "gzip, deflate")
			, (hAccept, "*/*")
			]
		}
	rep <- liftIO $ httpLbs req mgr
	liftIO $ BL.writeFile (destDir </> (T.unpack x <> ".html")) (responseBody rep)
	case scrapeStringLike (responseBody rep) yomikatawaScraper of
		Nothing -> throwM $ ScalpelException (responseBody rep)
		Just y -> pure y

main :: IO ()
main = do
	createDirectoryIfMissing True destDir >> listDirectory destDir >>= void . traverse (removeFile . (destDir </>))

	mgr <- newTlsManager
	defaultMain (tests mgr)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.HTML.Scalpel.Yomikatawa
( Result(..)
, Category(..)

, yomikatawaUri
, mkSearchURI

, yomikatawaScraper
)
where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Text.HTML.Scalpel.Core
import Text.StringLike
import Text.URI hiding (uriPath)
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Category
	= Kanji -- ^漢字
	| Mei -- ^名
	| Sei -- ^性
	deriving (Show, Eq)

categoryPathPiece :: Category -> RText 'PathPiece
categoryPathPiece Kanji = [pathPiece|kanji|]
categoryPathPiece Mei = [pathPiece|mei|]
categoryPathPiece Sei = [pathPiece|sei|]

yomikatawaUri :: URI
yomikatawaUri = [uri|https://yomikatawa.com|] 

mkSearchURI :: (MonadThrow m) => Category -> T.Text -> m URI
mkSearchURI c x = do
	x_pp <- mkPathPiece x
	pure $ yomikatawaUri & uriPath .~ [categoryPathPiece c, x_pp]

data Result
	= Result
		{ resultInputWord :: T.Text
		, resultHiragana :: T.Text
		, resultRomaji :: Maybe T.Text -- TODO: translate it directly
		, resultSameReadingWords :: [T.Text]
		, resultRandomWords :: [T.Text]
		}
	deriving (Show, Eq)

data KomikatawaException
	= NotKanjiError T.Text
	| PleaseDontIncludeAlphaDigits T.Text
	| NotFoundError T.Text
	deriving (Show, Eq)

instance Exception KomikatawaException

linksScraper :: StringLike str => Scraper str [T.Text]
linksScraper = fmap (T.decodeUtf8 . castString) <$> texts "a"

yomikatawaScraper :: StringLike str => Scraper str (Either KomikatawaException Result)
yomikatawaScraper = do
	chroot ("section" @: ["id" @= "content"]) $ inSerial $ do
		input_word <- T.decodeUtf8 . castString <$> seekNext (text ("h1" // "strong") <|> text ("h2" // "strong"))
		hiragana <- T.decodeUtf8 . castString <$> seekNext (text "p")
		if hiragana == "検索結果: 見つかりませんでした。" then do
			pure . Left $ NotFoundError input_word
		else if T.isPrefixOf "漢字を含めてください。" hiragana then do
			pure . Left $ NotKanjiError input_word
		else if T.isSuffixOf "アルファベット・数字を含めることはできません。" hiragana then do
			pure . Left $ PleaseDontIncludeAlphaDigits input_word
		else do
			romaji <- (Just . castString <$> seekNext (text "p")) <|> pure Nothing
			same_reading_words <- (seekNext $ chroot ("section" @: ["id" @= "sameReadingWords", hasClass "links"]) $ linksScraper) <|> pure mempty
			random_words <- seekNext $ chroot ("section" @: ["id" @= "randomWords", hasClass "links"]) $ linksScraper

			pure . Right $ Result input_word hiragana romaji same_reading_words random_words

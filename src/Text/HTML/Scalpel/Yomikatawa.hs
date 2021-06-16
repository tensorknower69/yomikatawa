{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.HTML.Scalpel.Yomikatawa
( Category(..)

, yomikatawaUri
, mkSearchURI

, Result(..)
, YomikatawaException(..)
, yomikatawaScraper
)
where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Data.Functor
import Text.HTML.Scalpel.Core
import Text.StringLike hiding (empty)
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
    { resultMayNotBeCorrect :: Bool
    -- ^True when @\/\/div[\@class="alert"]\/text() == "このワードは、正しくない可能性があります。他の辞書サイトもご確認ください。"@,
    --
    -- translation: This word's translation may not be correct, so please check out other dictionary sites just to make sure.
    , resultInputWord :: T.Text -- ^The input word.
    , resultHiragana :: T.Text -- ^The reading of the input word.
    , resultRomaji :: Maybe T.Text -- ^Romaji of the hiragana. I may implement a translator soon.
    , resultSameReadingWords :: [T.Text] -- ^A list of words with the same reading.
    , resultRandomWords :: [T.Text] -- ^A list of random words suggested by the site.
    }
  deriving (Show, Eq)

data YomikatawaException
  = NotFoundError T.Text -- ^When @検索結果: 見つかりませんでした。@.
  | PleaseInputKanji T.Text -- ^When @漢字を含めてください。@, basically the input is not a kanji.
  | PleaseDon'tInputAlphaDigits T.Text -- ^When @アルファベット・数字を含めることはできません。@, basically the input shouldn't be alphadigits
  | ErrorOccurred -- ^Basically 'PleaseInputKanji' but worse...
  deriving (Show, Eq)

instance Exception YomikatawaException

linksScraper :: (Monad m, StringLike str) => ScraperT str m [T.Text]
linksScraper = fmap (T.decodeUtf8 . castString) <$> texts "a"

yomikatawaScraper :: StringLike str => Scraper str (Either YomikatawaException Result)
yomikatawaScraper = error_scraper <|> normal_scraper
  where
  error_scraper = do
    chroot ("article" @: ["id" @= "content"]) $ do
      inSerial $ do
        msg <- T.decodeUtf8 . castString <$> seekNext (text "h1")
        if msg == "エラーが発生しました。" then pure (Left ErrorOccurred) else empty
  normal_scraper = do
    chroot ("section" @: ["id" @= "content"]) $ do
      runExceptT $ do
        (alert, input_word, hiragana, may_romaji) <- ExceptT . inSerial . runExceptT $ do
          alert <- lift $ (seekNext (text ("div" @: [hasClass "alert"])) $> True) <|> pure False
          input_word <- input_word_scraper
          hiragana <- ExceptT $ parse_hiragana input_word . T.decodeUtf8 . castString <$> seekNext (text "p")
          may_romaji <- lift $ (Just . castString <$> seekNext (text "p")) <|> pure Nothing
          pure (alert, input_word, hiragana, may_romaji)
        same_reading_words <- lift $ chroot ("section" @: ["id" @= "sameReadingWords", hasClass "links"]) linksScraper <|> pure mempty
        random_words <- lift $ chroot ("section" @: ["id" @= "randomWords", hasClass "links"]) linksScraper
        pure $ Result alert input_word hiragana may_romaji same_reading_words random_words
  input_word_scraper = lift $ T.decodeUtf8 . castString <$> seekNext (text ("h1" // "strong") <|> text ("h2" // "strong"))
  parse_hiragana input_word hiragana
    | "検索結果: 見つかりませんでした。" == hiragana
      = Left $ NotFoundError input_word
    | T.isPrefixOf "漢字を含めてください。" hiragana
      = Left $ PleaseInputKanji input_word
    | T.isSuffixOf "アルファベット・数字を含めることはできません。" hiragana
      = Left $ PleaseDon'tInputAlphaDigits input_word
    | True = pure $ hiragana

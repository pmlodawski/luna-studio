{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Doc.Parser where

import           Control.Applicative
import           Data.Default
import           Data.Monoid
import           Data.String                   as S
import           Prelude                       hiding ((++))
import qualified Text.Blaze.Html.Renderer.Utf8 as HTML
import           Text.Blaze.Html5              ((!))
import qualified Text.Blaze.Html5              as HTML
import qualified Text.Blaze.Html5.Attributes   as Attr
import           Text.Parsec                   hiding (many, optional, parse, (<|>))
import           Control.Lens                  hiding(noneOf)

import qualified Text.Doc.Lexer as L
import           Text.Doc.Utils
import          Text.Blaze.Internal as Blaze (stringValue)

data Language = Unknown | String


data TextPosition = TextPosition { _h1 :: Int
                                 , _h2 :: Int
                                 , _h3 :: Int
                                 }

makeLenses ''TextPosition

data ParserState = ParserState { _pos :: TextPosition }

makeLenses ''ParserState

instance Default ParserState where
  def = ParserState def

instance Default TextPosition where
  def = TextPosition def def def


-- HEADINGS
pHeadings = choice [ try pH1, try pH2, pH3 ] <?> "heading"

pH1 = HTML.h1 . pAddAnchor <$> surround L.pH1 (incH1 *> enumarateH1 L.headingTxt)
pH2 = HTML.h2 . pAddAnchor <$> surround L.pH2 (incH2 *> (enumarateH1.enumarateH2) L.headingTxt)
pH3 = HTML.h3 . pAddAnchor <$> surround L.pH3 (incH3 *> (enumarateH1.enumarateH2.enumarateH3) L.headingTxt)

-- FORMATTED TEXT
pFormattedText = choice [ try pTextBoldItalic, try pTextItalic, pTextBold ] <?> "formatted text"

pTextBoldItalic = HTML.b . HTML.i . HTML.toHtml <$> surround L.pTextBoldItalic  L.formattedText
pTextItalic     = HTML.i . HTML.toHtml          <$> surround L.pTextItalic      L.formattedText
pTextBold       = HTML.b . HTML.toHtml          <$> surround L.pTextBold        L.formattedText

-- CODE
pCode = choice [ try pCodeSnippet, pCodeInline ] <?> "code snippet"

-- block code
pBlockLine  p   = p *> (many $ noneOf "\n\r") <* L.eol
pBlock      p   = unlines  <$> (many1 p)

pCodeLine       = pBlockLine L.pCodeLineBegin
pBlockBegin     = L.eol *> L.pCodeLineBegin

pCodeSnippet    = try pBlockBegin *> ( generateBlockCode <$>
                                       pCodeLangLine     <*> 
                                       pBlock pCodeLine
                                     )

generateBlockCode ::  String -> String -> HTML.Html
generateBlockCode  lang content = HTML.pre ! Attr.class_ ( Blaze.stringValue ("prettyprint " ++ lang) ) $ HTML.toHtml content

-- inline code
pCodeInline     = generateInlineCode <$> (surround L.pCodeInline L.inlineCode)

pCodeLangLine  =  try ((between L.pCodeLangBegin L.pCodeLangEnd L.pCodeLang) <* L.eol) <|> pure ""

generateInlineCode content = HTML.div ! Attr.style "display: inline-block" ! Attr.class_ "inline" $
                             HTML.pre ! Attr.class_ "prettyprint" $
                             HTML.toHtml content

-- QUOTE
pQuote = try pQuoteBlock <?> "quote"

pQuoteLine  = pBlockLine L.pQuoteLineBegin
pQuoteBlock = HTML.blockquote . HTML.toHtml <$> pBlock pQuoteLine

-- HR
pHR = HTML.hr <$ L.pHR <?> "hr"

-- LISTS
pLists = choice [ try pListBullet, try pListOrdered ] <?> "list"

pList p li      = p <$> (foldr (++) mempty <$> (map (HTML.li . HTML.toHtml) <$> (L.eol *> many1 (pBlockLine li))))

pListBullet     = pList HTML.ul L.pUlLiBegin
pListOrdered    = pList HTML.ol L.pOlLiBegin

-- LINKS AND IMAGES
pLink = choice [ try pImage, try pAddress', try pAddress ]

pAddress    = (\s -> generateLink s s) <$> url
              where url = ( (++) <$> L.pLinkBegin <*> many L.pNotWhitespace )
pAddress'   = generateLink <$> content <*> url
              where content = between L.pLinkContentBegin L.pLinkContentEnd L.pLinkContent
                    url     = between L.pLinkAddrBegin L.pLinkAddrEnd L.pLinkAddr
pImage      = generateImage <$> content <*> url
              where content = L.pImageBegin *> between L.pLinkContentBegin L.pLinkContentEnd L.pLinkContent
                    url     = between L.pLinkAddrBegin L.pLinkAddrEnd L.pLinkAddr

generateLink content addr = HTML.a ! Attr.href (fromString addr) $ (HTML.toHtml content)
generateImage alt addr    = HTML.img ! Attr.src (fromString addr) ! Attr.alt (fromString alt)

-- ALL
pElement = choice [ pHeadings
                  , pFormattedText
                  , pCode
                  , pQuote
                  , try pHR
                  , pLists
                  , pLink
                  ]

pToken = try pElement
       <|> ((HTML.toHtml . (:[])) <$> anyChar)

pProgram = foldr (++) mempty <$> many pToken <* many(L.eol <* L.pSpaces) <* eof

-- add html structure and js scripts to highlight code snippets
addJSLibs html = HTML.docTypeHtml $ do
    HTML.head $ do
        HTML.title "Markup"
        HTML.meta ! Attr.httpEquiv "Content-Type" ! Attr.content "text/html; charset=utf-8"
        HTML.link
            ! Attr.href "http://fonts.googleapis.com/css?family=Open+Sans+Condensed:300,300italic,700&subset=latin,latin-ext"
            ! Attr.rel "stylesheet" 
            ! Attr.type_ "text/css"
        HTML.link ! Attr.type_ "text/css" ! Attr.rel "stylesheet" ! Attr.href "libs/markup/include/prettify/prettify.css"
        HTML.script "" ! Attr.type_ "text/javascript" ! Attr.src "https://google-code-prettify.googlecode.com/svn/loader/run_prettify.js"
        HTML.link ! Attr.type_ "text/css" ! Attr.rel "stylesheet" ! Attr.href "libs/markup/include/markup.css"
    HTML.body html

parse markup = fmap (HTML.renderHtml . addJSLibs) (runParser pProgram (def::ParserState) "Flowbox Markup Parser" markup)

-- testing by hand
pProgram_test = generateBlockCode <$>
              pCodeLangLine <*>
              (unlines  <$> 
                  (L.eol *> 
                  many1 (L.pCodeLineBegin *> (many $ noneOf "\n\r") <* L.eol))
              )
parse_test markup = 
                    fmap (HTML.renderHtml) 
                    (runParser pCodeSnippet (0::Int) "Flowbox Markup Parser" markup)

-- header numbering and anchoring
anchor text = fromString $ [x | x <- text, x /= ' ']

pAddAnchor :: String -> HTML.Html
pAddAnchor headingText = do
                            (HTML.a ! Attr.name (anchor headingText) $ HTML.toHtml ("" :: String))
                            HTML.toHtml (headingText :: String)

incH1 = do
    s <- getState
    let nh1 = view (pos.h1) s + 1
    setState $ s & set (pos.h1) nh1
                 & set (pos.h2) def
                 & set (pos.h3) def 
             
    return nh1

incH2 = do
    s <- getState
    let nh2 = view (pos.h2) s + 1
    setState $ s & set (pos.h2) nh2
                 & set (pos.h3) def 
             
    return nh2


incH3 = do
    s <- getState
    let nh3 = view (pos.h3) s + 1
    setState (s & set (pos.h3) nh3)
    return nh3


--getH1 :: Int
getH1 = ((flip (++) ".").show._h1._pos) <$> getState
--getH1 = do
--    s <- getState
--    let sn = ((show.h1.pos) <$> s)
--    return sn
getH2 = ((flip (++) ".").show._h2._pos) <$> getState
getH3 = ((flip (++) ".").show._h3._pos) <$> getState

enumarateH1 p = (++) <$> getH1 <*> p
enumarateH2 p = (++) <$> getH2 <*> p
enumarateH3 p = (++) <$> getH3 <*> p

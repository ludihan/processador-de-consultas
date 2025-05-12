{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified RA.Types as RAT
import Sql.Parser (parseSelect)
import Sql.Validator (validateSqlSelect)
import Text.Megaparsec (errorBundlePretty, parse)

import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens
import Monomer
import RA.Converter (sqlSelect2RA)

type Error = Text

parseAndValidateSelect :: String -> Either [Error] RAT.RAExpr
parseAndValidateSelect sel =
    case parse parseSelect "<input>" sel of
        Left bundle -> Left $ map T.pack (Prelude.lines $ errorBundlePretty bundle)
        Right parsed ->
            case validateSqlSelect parsed of
                [] -> Right (sqlSelect2RA parsed)
                xs -> Left (map T.pack xs)

data AppModel = AppModel
    { _sqlInput :: Text
    , _raOutput :: Text
    , _planOutput :: Text
    , _errors :: [Error]
    }
    deriving (Eq, Show)

data AppEvent
    = AppInit
    | ShouldParseRA
    | ShouldParsePlan
    deriving (Eq, Show)

makeLenses ''AppModel

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
        vstack
            [ textArea sqlInput
                `nodeKey` "sqlInput"
                `styleBasic` [padding 10, height 200]
            , spacer
            , hstack
                [ button "Parse to Relational Algebra" ShouldParseRA
                , spacer
                , button "Parse to Query Plan" ShouldParsePlan
                ]
            , spacer
            , vstack
                [ label_
                    ( if Prelude.null (model ^. errors)
                        then T.pack ""
                        else T.concat ["Errors: ", T.unlines (model ^. errors)]
                    )
                    [multiline]
                , label_
                    ( if T.null (model ^. raOutput)
                        then T.pack ""
                        else model ^. raOutput
                    )
                    [multiline]
                , label_
                    ( if T.null (model ^. planOutput)
                        then T.pack ""
                        else model ^. planOutput
                    )
                    [multiline]
                ]
            ]
            `styleBasic` [padding 10]

handleEvent ::
    WidgetEnv AppModel AppEvent ->
    WidgetNode AppModel AppEvent ->
    AppModel ->
    AppEvent ->
    [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt = case evt of
    AppInit -> []
    ShouldParseRA ->
        let sqlInputText = model ^. sqlInput
            parseResult = parseAndValidateSelect (T.unpack sqlInputText)
         in case parseResult of
                Left errs ->
                    [ Model
                        ( model
                            & errors .~ errs
                            & raOutput .~ ""
                            & planOutput .~ ""
                        )
                    ]
                Right parsed ->
                    [ Model
                        ( model
                            & errors .~ []
                            & raOutput .~ (T.pack . show) parsed
                            & planOutput .~ ""
                        )
                    ]
    ShouldParsePlan ->
        let sqlInputText = model ^. sqlInput
            parseResult = parseAndValidateSelect (T.unpack sqlInputText)
         in case parseResult of
                Left errs ->
                    [ Model
                        ( model
                            & errors .~ errs
                            & raOutput .~ ""
                            & planOutput .~ ""
                        )
                    ]
                Right parsed ->
                    [ Model
                        ( model
                            & errors .~ []
                            & raOutput .~ (T.pack . show) parsed
                            & planOutput .~ ""
                        )
                    ]

main :: IO ()
main = do
    startApp model handleEvent buildUI config
  where
    config =
        [ appWindowTitle "Processador de Consultas"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Hack-Regular.ttf"
        , appInitEvent AppInit
        , appModelFingerprint show
        ]
    model = AppModel "" "" "" []

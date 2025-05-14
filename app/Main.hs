{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens
import Monomer
import RA.Converter

type Error = Text

data AppModel = AppModel
    { _sqlInput :: Text
    , _raOutput :: Text
    , _planOutput :: Text
    , _errors :: [Error]
    }
    deriving (Eq, Show)

makeLenses ''AppModel

data AppEvent
    = AppInit
    | ShouldParseRA
    | ShouldParsePlan
    deriving (Eq, Show)

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent
buildUI _ model = widgetTree
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
            parseResult = sqlSelectString2ra (T.unpack sqlInputText)
         in case parseResult of
                Left errs ->
                    [ Model
                        ( model
                            & errors .~ errorsToGui errs
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
            parseResult = sqlSelectString2QueryPlan (T.unpack sqlInputText)
         in case parseResult of
                Left errs ->
                    [ Model
                        ( model
                            & errors .~ errorsToGui errs
                            & raOutput .~ ""
                            & planOutput .~ ""
                        )
                    ]
                Right parsed ->
                    [ Model
                        ( model
                            & errors .~ []
                            & raOutput .~ (T.pack . unlines . reverse) parsed
                            & planOutput .~ ""
                        )
                    ]

errorsToGui :: [String] -> [Text]
errorsToGui = map T.pack

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

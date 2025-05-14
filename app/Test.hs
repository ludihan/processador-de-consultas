module Test (
    module Sql.Examples,
    selectPretty,
    select2RA,
    selectErr,
) where

import Data.List (intercalate)
import qualified RA.Converter as C
import Sql.Examples
import Sql.Parser (parseSelect)
import qualified Sql.Types as T
import Sql.Validator
import Text.Megaparsec

selectPretty :: String -> IO ()
selectPretty sel =
    let a = parse parseSelect "<input>" sel
        formatList = intercalate ", "
     in case a of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right (T.Select cols from joins wher) ->
                putStrLn $
                    "Select\n"
                        ++ "cols: "
                        ++ formatList cols
                        ++ "\nfrom: "
                        ++ from
                        ++ "\njoin: "
                        ++ show joins
                        ++ "\nwhere: "
                        ++ show wher

selectErr :: String -> IO ()
selectErr sel =
    let a = parse parseSelect "<input>" sel
     in case a of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right selParsed -> case validateSqlSelect selParsed of
                [] -> putStrLn "No errors"
                xs -> mapM_ putStrLn xs

select2RA :: String -> IO ()
select2RA sel =
    case parse parseSelect "<input>" sel of
        Left bundle -> print bundle
        Right x -> print (C.sqlSelect2ra x)

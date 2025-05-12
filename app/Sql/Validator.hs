module Sql.Validator where

import Data.Bifunctor
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe
import Database.Schema (lowercaseDatabase)
import Sql.Types

type Error = String

toLowerStr :: String -> String
toLowerStr = map toLower

validateSqlSelect :: Select -> [Error]
validateSqlSelect (Select cols from joins mWhere) =
    let
        lowercaseDb = map (bimap toLowerStr (map toLowerStr)) lowercaseDatabase
        tableNames = map fst lowercaseDb

        mentionedTables =
            nub $
                [toLowerStr from]
                    ++ map (toLowerStr . fst) joins
                    ++ [toLowerStr t | col <- cols, let (t, _) = splitAtDot col, not (null t)]
                    ++ [ toLowerStr t | (_, _, RightPredColumn rcol) <- fromMaybe [] mWhere ++ map snd joins, let (t, _) = splitAtDot rcol, not (null t)
                       ]

        missingTables =
            filter (`notElem` tableNames) mentionedTables

        columnsToCheck =
            cols
                ++ [col | (col, _, _) <- fromMaybe [] mWhere ++ map snd joins]
                ++ [c | (_, _, RightPredColumn c) <- fromMaybe [] mWhere ++ map snd joins]

        columnExists :: String -> Bool
        columnExists col =
            let (tbl, c) = splitAtDot col
                lcTbl = toLowerStr tbl
                lcCol = toLowerStr c
             in if null tbl
                    then any (elem lcCol . snd) lowercaseDb
                    else case lookup lcTbl lowercaseDb of
                        Just cols' -> lcCol `elem` cols'
                        Nothing -> False

        missingColumns =
            filter (not . columnExists) columnsToCheck
     in
        ( ["Missing tables: " <> show missingTables | not (null missingTables)]
            ++ ["Missing columns: " <> show missingColumns | not (null missingColumns)]
        )

splitAtDot :: String -> (String, String)
splitAtDot s =
    let (table, rest) = break (== '.') s
     in case rest of
            '.' : col -> (table, col)
            _ -> ("", s)

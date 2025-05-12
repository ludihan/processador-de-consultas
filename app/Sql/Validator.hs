module Sql.Validator where

import Data.Char
import Database.Schema
import Sql.Types

type Error = String

validadeSqlSelect :: Select -> [Error]
validadeSqlSelect (Select cols from joins wher) =
    let
        columnsFromSelectExist = 
            (let 
                colsWithTable = filter ("." `elem`) (map fst cols)
            )
        isFromPresent =
            ( if map toLower from `elem` map fst lowercaseDatabase
                then "The table/relation \"" ++ from ++ "\" from the \"from\" clause is not present in the schema"
                else ""
            )
        ambiguousColumns = (
            let 
                maybeAmbigousCols = filter (not ("." `elem`)) (map fst cols)
                
        )
     in
        filter (== "") [columnsFromSelectExist, isFromPresent]

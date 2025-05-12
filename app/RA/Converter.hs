module RA.Converter where

import Data.Maybe

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import RA.Types
import qualified Sql.Types as SqlT

-- unoptimized tree
sqlSelect2RA :: SqlT.Select -> RAExpr
sqlSelect2RA (SqlT.Select cols from joins wher) =
    let
        allRelations = from : map fst joins :: [Relation]
        allPreds = map snd joins ++ fromMaybe [] wher
     in
        Projection
            cols
            ( Selection allPreds (crossFromRelations (NE.fromList allRelations))
            )

crossFromRelations :: NonEmpty Relation -> RAExpr
crossFromRelations (x :| []) = Value x
crossFromRelations (x :| xs) = Cross (Value x) (crossFromRelations $ NE.fromList xs)

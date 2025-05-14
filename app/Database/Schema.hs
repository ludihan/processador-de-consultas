module Database.Schema where

import qualified Data.Bifunctor
import Data.Char
import Sql.Types as T

database :: T.Database
database =
    [
        ( "Categoria"
        ,
            [ "idCategoria"
            , "Descricao"
            ]
        )
    ,
        ( "Produto"
        ,
            [ "idProduto"
            , "Nome"
            , "Descricao"
            , "Preco"
            , "QuantEstoque"
            , "Categoria_idCategoria"
            ]
        )
    ,
        ( "TipoCliente"
        ,
            [ "idTipoCliente"
            , "Descricao"
            ]
        )
    ,
        ( "Cliente"
        ,
            [ "idCliente"
            , "Nome"
            , "Email"
            , "Nascimento"
            , "Senha"
            , "TipoCliente_idTipoCliente"
            , "DataRegistro"
            ]
        )
    ,
        ( "TipoEndereco"
        ,
            [ "idTipoEndereco"
            , "Descricao"
            ]
        )
    ,
        ( "Endereco"
        ,
            [ "idEndereco"
            , "EnderecoPadrao"
            , "Logradouro"
            , "Numero"
            , "Complemento"
            , "Bairro"
            , "Cidade"
            , "UF"
            , "CEP"
            , "TipoEndereco_idTipoEndereco"
            , "Cliente_idCliente"
            ]
        )
    ,
        ( "Telefone"
        ,
            [ "Numero"
            , "Cliente_idCliente"
            ]
        )
    ,
        ( "Status"
        ,
            [ "idStatus"
            , "Descricao"
            ]
        )
    ,
        ( "Pedido"
        ,
            [ "idPedido"
            , "Status_idStatus"
            , "DataPedido"
            , "ValorTotalPedido"
            , "Cliente_idCliente"
            ]
        )
    ,
        ( "Pedido_has_Produto"
        ,
            [ "IdPedidoProduto"
            , "Pedido_idPedido"
            , "Produto_idProduto"
            , "Quantidade"
            , "PrecoUnitario"
            ]
        )
    ]

toLowerStr :: String -> String
toLowerStr = map toLower

lowercaseDatabase :: T.Database
lowercaseDatabase = map (Data.Bifunctor.bimap toLowerStr (map toLowerStr)) database

allCols :: Columns
allCols = concatMap snd database

findMatchFromCol :: Column -> Maybe ([Table], Column)
findMatchFromCol col =
    let
        c = reverse $ takeWhile (/= '.') $ reverse col
        found = filter (\x -> toLowerStr x == toLowerStr c) allCols
        t = filter (\x -> toLowerStr c `elem` map toLowerStr (snd x)) database
     in
        if null found
            then
                Nothing
            else
                Just (map fst t, head found)

isColFromTable :: Column -> Table -> Bool
isColFromTable col tab =
    any
        ( \(t, c) ->
            let aaa = reverse $ takeWhile (/= '.') $ reverse col
             in toLowerStr tab == toLowerStr t
                    && any (\alkj -> toLowerStr alkj == toLowerStr aaa ) c)
        lowercaseDatabase

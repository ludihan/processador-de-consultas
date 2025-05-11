module Sql.Data where

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

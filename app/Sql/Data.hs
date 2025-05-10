module Sql.Data where

import Sql.Types as T

database :: T.Database
database =
    [
        ( "Categoria"
        ,
            [ ("idCategoria", T.Int)
            , ("Descricao", T.Varchar 45)
            ]
        )
    ,
        ( "Produto"
        ,
            [ ("idProduto", T.Int)
            , ("Nome", T.Int)
            , ("Descricao", T.Int)
            , ("Preco", T.Int)
            , ("QuantEstoque", T.Int)
            , ("Categoria_idCategoria", T.Int)
            ]
        )
    ,
        ( "TipoCliente"
        ,
            [ ("idTipoCliente", T.Int)
            , ("Descricao", T.Int)
            ]
        )
    ,
        ( "Cliente"
        ,
            [ ("idCliente", T.Int)
            , ("Nome", T.Int)
            , ("Email", T.Int)
            , ("Nascimento", T.Int)
            , ("Senha", T.Int)
            , ("TipoCliente_idTipoCliente", T.Int)
            , ("DataRegistro", T.Int)
            ]
        )
    ,
        ( "TipoEndereco"
        ,
            [ ("idTipoEndereco", T.Int)
            , ("Descricao", T.Int)
            ]
        )
    ,
        ( "Endereco"
        ,
            [ ("idEndereco", T.Int)
            , ("EnderecoPadrao", T.Int)
            , ("Logradouro", T.Int)
            , ("Numero", T.Int)
            , ("Complemento", T.Int)
            , ("Bairro", T.Int)
            , ("Cidade", T.Int)
            , ("UF", T.Int)
            , ("CEP", T.Int)
            , ("TipoEndereco_idTipoEndereco", T.Int)
            , ("Cliente_idCliente", T.Int)
            ]
        )
    ,
        ( "Telefone"
        ,
            [ ("Numero", T.Int)
            , ("Cliente_idCliente", T.Int)
            ]
        )
    ,
        ( "Status"
        ,
            [ ("idStatus", T.Int)
            , ("Descricao", T.Int)
            ]
        )
    ,
        ( "Pedido"
        ,
            [ ("idPedido", T.Int)
            , ("Status_idStatus", T.Int)
            , ("DataPedido", T.Int)
            , ("ValorTotalPedido", T.Int)
            , ("Cliente_idCliente", T.Int)
            ]
        )
    ,
        ( "Pedido_has_Produto"
        ,
            [ ("IdPedidoProduto", T.Int)
            , ("Pedido_idPedido", T.Int)
            , ("Produto_idProduto", T.Int)
            , ("Quantidade", T.Int)
            , ("PrecoUnitario", T.Int)
            ]
        )
    ]

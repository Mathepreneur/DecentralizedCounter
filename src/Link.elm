module Link exposing (Link, kovanFaucetWebsite, metamaskWebsite, newTab)

import Element exposing (Attribute, Element)
import Url exposing (Url)


type Link
    = Link Url


newTab : List (Attribute msg) -> { link : Link, label : Element msg } -> Element msg
newTab attributes { link, label } =
    let
        (Link url) =
            link
    in
    Element.newTabLink attributes
        { url = Url.toString url
        , label = label
        }


metamaskWebsite : Link
metamaskWebsite =
    createLink
        { protocol = Url.Https
        , host = "metamask.io"
        , port_ = Nothing
        , path = "/"
        , query = Nothing
        , fragment = Nothing
        }


kovanFaucetWebsite : Link
kovanFaucetWebsite =
    createLink
        { protocol = Url.Https
        , host = "gitter.im"
        , port_ = Nothing
        , path = "/kovan-testnet/faucet/"
        , query = Nothing
        , fragment = Nothing
        }


createLink : Url -> Link
createLink url =
    Link url

module Network exposing (Network, decoder)

import Json.Decode as Decode exposing (Decoder)


type Network
    = Kovan


decoder : Decoder (Result String Network)
decoder =
    Decode.map fromNetworkId Decode.string


fromNetworkId : String -> Result String Network
fromNetworkId string =
    case string of
        "42" ->
            Ok Kovan

        _ ->
            Err "Decentralized Counter only works with Kovan Test Network. Please switch your network on Metamask."

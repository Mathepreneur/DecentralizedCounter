module Contract exposing
    ( Case
    , Contract
    , Log
    , Outcome
    , counter
    , decrement
    , encode
    , increment
    , initialLog
    , receiveDecode
    )

import Data exposing (Address, UnsignedInteger)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Contract
    = Contract Value


type Log
    = Log
        { id : Int
        , record : Record
        }


type alias Record =
    Dict Int Function


type Method
    = Call
    | SendTransaction


type Function
    = Counter
    | Increment
    | Decrement


type alias Outcome =
    { contract : Contract
    , nextLog : Log
    }


initialLog : Log
initialLog =
    Log
        { id = 0
        , record = Dict.empty
        }


create : Int -> Record -> Log
create id record =
    Log
        { id = id
        , record = record
        }


next : Function -> Log -> Log
next function (Log { id, record }) =
    let
        nextId : Int
        nextId =
            id + 1

        nextRecord : Record
        nextRecord =
            Dict.insert nextId function record
    in
    create nextId nextRecord


idEncode : Log -> Value
idEncode (Log { id }) =
    Encode.int id


parameterEncode : List ( String, Value ) -> Value
parameterEncode parameter =
    Encode.list Encode.object <| List.singleton parameter


jsonrpcEncode : Value
jsonrpcEncode =
    Encode.string "2.0"


methodEncode : Method -> Value
methodEncode method =
    case method of
        Call ->
            Encode.string "eth_call"

        SendTransaction ->
            Encode.string "eth_sendTransaction"


encode : Contract -> Value
encode (Contract value) =
    value


counter : Log -> Outcome
counter log =
    let
        parameter : List ( String, Value )
        parameter =
            [ ( "to", Data.addressEncode Data.contract )
            , ( "data", Data.encode Data.counter )
            ]

        nextLog : Log
        nextLog =
            next Counter log

        contract : Contract
        contract =
            Contract <|
                Encode.object
                    [ ( "id", idEncode nextLog )
                    , ( "jsonrpc", jsonrpcEncode )
                    , ( "method", methodEncode Call )
                    , ( "params", parameterEncode parameter )
                    ]
    in
    Outcome contract nextLog


increment : Address -> Log -> Outcome
increment address log =
    let
        parameter : List ( String, Value )
        parameter =
            [ ( "from", Data.addressEncode address )
            , ( "to", Data.addressEncode Data.contract )
            , ( "data", Data.encode Data.increment )
            ]

        nextLog : Log
        nextLog =
            next Increment log

        contract : Contract
        contract =
            Contract <|
                Encode.object
                    [ ( "id", idEncode nextLog )
                    , ( "jsonrpc", jsonrpcEncode )
                    , ( "method", methodEncode SendTransaction )
                    , ( "params", parameterEncode parameter )
                    ]
    in
    Outcome contract nextLog


decrement : Address -> Log -> Outcome
decrement address log =
    let
        parameter : List ( String, Value )
        parameter =
            [ ( "from", Data.addressEncode address )
            , ( "to", Data.addressEncode Data.contract )
            , ( "data", Data.encode Data.decrement )
            ]

        nextLog : Log
        nextLog =
            next Decrement log

        contract : Contract
        contract =
            Contract <|
                Encode.object
                    [ ( "id", idEncode nextLog )
                    , ( "jsonrpc", jsonrpcEncode )
                    , ( "method", methodEncode SendTransaction )
                    , ( "params", parameterEncode parameter )
                    ]
    in
    Outcome contract nextLog


type alias Case info model msg =
    { counterCase : Maybe UnsignedInteger -> info -> { model | error : Maybe String } -> ( { model | error : Maybe String }, Cmd msg )
    , incrementCase : { model | error : Maybe String } -> ( { model | error : Maybe String }, Cmd msg )
    , decrementCase : { model | error : Maybe String } -> ( { model | error : Maybe String }, Cmd msg )
    }


idDecoder : Decoder Int
idDecoder =
    Decode.field "id" Decode.int


jsonrpcDecoder : Decoder String
jsonrpcDecoder =
    Decode.field "jsonrpc" Decode.string


counterDecoder : Decoder (Result String UnsignedInteger)
counterDecoder =
    Decode.field "result" Data.unsignedIntegerDecoder


receiveDecode : Case info { model | error : Maybe String } msg -> Log -> Value -> info -> { model | error : Maybe String } -> ( { model | error : Maybe String }, Cmd msg )
receiveDecode { counterCase, incrementCase, decrementCase } (Log { record }) value info model =
    let
        id : Result Decode.Error Int
        id =
            value
                |> Decode.decodeValue idDecoder

        function : Result Decode.Error (Maybe Function)
        function =
            record
                |> Ok
                |> Result.map2 Dict.get id

        jsonrpc : Result Decode.Error String
        jsonrpc =
            value
                |> Decode.decodeValue jsonrpcDecoder
    in
    case ( jsonrpc, function ) of
        ( Ok "2.0", Ok (Just Counter) ) ->
            let
                unsignedInteger : Maybe UnsignedInteger
                unsignedInteger =
                    value
                        |> Decode.decodeValue counterDecoder
                        |> Result.map Result.toMaybe
                        |> Result.withDefault Nothing
            in
            counterCase unsignedInteger info model

        ( Ok "2.0", Ok (Just Increment) ) ->
            incrementCase model

        ( Ok "2.0", Ok (Just Decrement) ) ->
            decrementCase model

        ( Ok "2,0", Ok Nothing ) ->
            ( { model | error = Just "Cannot find log of transaction." }
            , Cmd.none
            )

        ( Ok "2.0", Err decodeError ) ->
            ( { model | error = Just <| Decode.errorToString decodeError }
            , Cmd.none
            )

        ( Ok _, _ ) ->
            ( { model | error = Just "Must use JsonRpc 2" }
            , Cmd.none
            )

        ( Err decodeError, _ ) ->
            ( { model | error = Just <| Decode.errorToString decodeError }
            , Cmd.none
            )

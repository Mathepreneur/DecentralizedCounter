module Data exposing
    ( Address
    , Data
    , UnsignedInteger
    , addressDecoder
    , addressEncode
    , contract
    , counter
    , decrement
    , encode
    , fromAddressToText
    , fromAddressToTextShort
    , fromUnsignedIntegerToText
    , increment
    , toUnsignedIntegerFromStringDecimal
    , toUnsignedIntegerFromStringHexadecimal
    , unsignedIntegerDecoder
    , unsignedIntegerEqual
    , unsignedIntegerInputText
    , unsignedIntegerMaxInteger
    , unsignedIntegerZero
    )

import Element exposing (Attribute, Element)
import Element.Input exposing (Label, Placeholder)
import Hexadecimal exposing (Hexadecimal)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Data
    = Data Selector


type Selector
    = Selector Hexadecimal


type Address
    = Address Hexadecimal


type UnsignedInteger
    = UnsignedInteger Hexadecimal



-- DATA


encode : Data -> Value
encode (Data (Selector hexadecimal)) =
    Hexadecimal.encode hexadecimal


counter : Data
counter =
    Data selectorCounter


increment : Data
increment =
    Data selectorIncrement


decrement : Data
decrement =
    Data selectorDecrement



-- SELECTOR


selectorCounter : Selector
selectorCounter =
    Selector Hexadecimal.counter


selectorIncrement : Selector
selectorIncrement =
    Selector Hexadecimal.increment


selectorDecrement : Selector
selectorDecrement =
    Selector Hexadecimal.decrement



-- ADDRESS


addressDecoder : Decoder (Result String Address)
addressDecoder =
    Decode.map toAddressFromString Decode.string


addressEncode : Address -> Value
addressEncode (Address hexadecimal) =
    Hexadecimal.encode hexadecimal


fromAddressToText : Address -> Element msg
fromAddressToText (Address hexadecimal) =
    Hexadecimal.toTextHexadecimal hexadecimal


fromAddressToTextShort : Address -> Element msg
fromAddressToTextShort (Address hexadecimal) =
    Hexadecimal.toTextHexadecimalShort hexadecimal


toAddressFromString : String -> Result String Address
toAddressFromString string =
    let
        resultHexadecimal : Result String Hexadecimal
        resultHexadecimal =
            string
                |> Hexadecimal.fromStringHexadecimal

        resultLength : Result String Int
        resultLength =
            resultHexadecimal
                |> Result.map Hexadecimal.length
    in
    case ( resultHexadecimal, resultLength ) of
        ( Ok hexadecimal, Ok 40 ) ->
            Ok <| Address <| hexadecimal

        ( Ok _, Ok _ ) ->
            Err <| "Address must be 20 bytes"

        ( Err error, _ ) ->
            Err <| error

        ( Ok _, Err error ) ->
            Err <| error


contract : Address
contract =
    Address Hexadecimal.contract



-- UNSIGNED INTEGER


unsignedIntegerDecoder : Decoder (Result String UnsignedInteger)
unsignedIntegerDecoder =
    Decode.map toUnsignedIntegerFromStringHexadecimal Decode.string


fromUnsignedIntegerToHexadecimal : UnsignedInteger -> Hexadecimal
fromUnsignedIntegerToHexadecimal (UnsignedInteger hexadecimal) =
    hexadecimal


fromUnsignedIntegerToText : UnsignedInteger -> Element msg
fromUnsignedIntegerToText (UnsignedInteger hexadecimal) =
    Hexadecimal.toTextDecimal hexadecimal


toUnsignedIntegerFromStringHexadecimal : String -> Result String UnsignedInteger
toUnsignedIntegerFromStringHexadecimal string =
    let
        resultHexadecimal : Result String Hexadecimal
        resultHexadecimal =
            string
                |> Hexadecimal.fromStringHexadecimal

        resultLength : Result String Int
        resultLength =
            resultHexadecimal
                |> Result.map Hexadecimal.length
    in
    case ( resultHexadecimal, resultLength ) of
        ( Ok hexadecimal, Ok length ) ->
            if length <= 64 then
                Ok <| UnsignedInteger hexadecimal

            else
                Err <| "Unsigned Integer must be 32 bytes"

        ( Err error, _ ) ->
            Err error

        ( Ok _, Err error ) ->
            Err error


toUnsignedIntegerFromStringDecimal : String -> Result String UnsignedInteger
toUnsignedIntegerFromStringDecimal string =
    let
        resultHexadecimal : Result String Hexadecimal
        resultHexadecimal =
            string
                |> Hexadecimal.fromStringDecimal

        resultLength : Result String Int
        resultLength =
            resultHexadecimal
                |> Result.map Hexadecimal.length
    in
    case ( resultHexadecimal, resultLength ) of
        ( Ok hexadecimal, Ok length ) ->
            if length <= 64 then
                Ok <| UnsignedInteger hexadecimal

            else
                Err <| "Unsigned Integer must be 32 bytes"

        ( Err error, _ ) ->
            Err error

        ( Ok _, Err error ) ->
            Err error


unsignedIntegerEqual : UnsignedInteger -> UnsignedInteger -> Bool
unsignedIntegerEqual (UnsignedInteger first) (UnsignedInteger second) =
    Hexadecimal.equal first second


type alias InputDetails msg =
    { onChange : String -> msg
    , unsignedInteger : Maybe UnsignedInteger
    , placeholder : Maybe (Placeholder msg)
    , label : Label msg
    }


unsignedIntegerInputText : List (Attribute msg) -> InputDetails msg -> Element msg
unsignedIntegerInputText attributes { onChange, unsignedInteger, placeholder, label } =
    let
        hexadecimal : Maybe Hexadecimal
        hexadecimal =
            Maybe.map fromUnsignedIntegerToHexadecimal unsignedInteger
    in
    Hexadecimal.toInputTextDecimal attributes
        { onChange = onChange
        , hexadecimal = hexadecimal
        , placeholder = placeholder
        , label = label
        }



-- UNSIGNED INTEGERS CONSTANT


unsignedIntegerZero : UnsignedInteger
unsignedIntegerZero =
    UnsignedInteger Hexadecimal.zero


unsignedIntegerMaxInteger : UnsignedInteger
unsignedIntegerMaxInteger =
    UnsignedInteger Hexadecimal.maxUnsignedInteger

module Hexadecimal exposing
    ( Hexadecimal
    , addBy
    , append
    , contract
    , counter
    , decoder
    , decrement
    , encode
    , equal
    , fromStringDecimal
    , fromStringHexadecimal
    , increment
    , length
    , maxUnsignedInteger
    , padUpto64
    , toInputTextDecimal
    , toInputTextHexadecimal
    , toTextDecimal
    , toTextHexadecimal
    , toTextHexadecimalShort
    , zero
    )

import Element exposing (Attribute, Element)
import Element.Input as Input exposing (Label, Placeholder)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Digit
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | LowercaseA
    | UppercaseA
    | LowercaseB
    | UppercaseB
    | LowercaseC
    | UppercaseC
    | LowercaseD
    | UppercaseD
    | LowercaseE
    | UppercaseE
    | LowercaseF
    | UppercaseF


type Hexadecimal
    = Hexadecimal
        { leftMostDigit : Digit
        , otherDigits : List Digit
        }



-- HEXADECIMAL


create : Digit -> List Digit -> Hexadecimal
create leftMostDigit otherDigits =
    Hexadecimal
        { leftMostDigit = leftMostDigit
        , otherDigits = otherDigits
        }


decoder : Decoder (Result String Hexadecimal)
decoder =
    Decode.map fromStringHexadecimal Decode.string


encode : Hexadecimal -> Encode.Value
encode hexadecimal =
    Encode.string <| toStringHexadecimal hexadecimal



-- HEXADECIMAL CONSTRUCTOR


fromStringHexadecimal : String -> Result String Hexadecimal
fromStringHexadecimal string =
    let
        remove0x : List Char -> Result String (List Char)
        remove0x characters =
            case characters of
                [] ->
                    Err "Cannot be empty string"

                '0' :: 'x' :: remainingCharacters ->
                    Ok remainingCharacters

                '0' :: 'X' :: remainingCharacters ->
                    Ok remainingCharacters

                _ ->
                    Err "Must start wiht 0x or 0X"

        initial : List Char -> Result String Hexadecimal
        initial initialCharacters =
            recursive initialCharacters []

        recursive : List Char -> List Digit -> Result String Hexadecimal
        recursive characters accumulatingDigits =
            case ( characters, accumulatingDigits ) of
                ( [], [] ) ->
                    Err "Cannot be empty string after 0x or 0X"

                ( [], singletonDigit :: remainingDigits ) ->
                    Ok <| create singletonDigit remainingDigits

                ( singletonCharacter :: remainingCharacters, digits ) ->
                    let
                        resultDigit : Result String Digit
                        resultDigit =
                            fromCharToDigit singletonCharacter

                        recursiveRemaining : List Digit -> Result String Hexadecimal
                        recursiveRemaining =
                            recursive remainingCharacters
                    in
                    digits
                        |> Result.Ok
                        |> Result.map2 (::) resultDigit
                        |> Result.andThen recursiveRemaining
    in
    string
        |> String.toList
        |> remove0x
        |> Result.map List.reverse
        |> Result.andThen initial
        |> Result.map remove0


fromStringDecimal : String -> Result String Hexadecimal
fromStringDecimal string =
    string
        |> decimalFromString
        |> Result.andThen fromDecimal



-- HEXADECIMAL DECIMAL


toDecimal : Hexadecimal -> Decimal
toDecimal (Hexadecimal { leftMostDigit, otherDigits }) =
    let
        leftMostDecimalDigit : Int
        leftMostDecimalDigit =
            fromDigitToInt leftMostDigit

        otherDecimalDigits : List Int
        otherDecimalDigits =
            otherDigits
                |> List.map fromDigitToInt

        multiplyWith : Int -> Int -> Decimal
        multiplyWith index integer =
            decimalHexadecimalPower index
                |> decimalMultiplyByDigit integer

        decimalZero : Decimal
        decimalZero =
            Decimal 0 []

        leftMostProduct : Decimal
        leftMostProduct =
            let
                otherDigitsLength : Int
                otherDigitsLength =
                    List.length otherDecimalDigits
            in
            decimalHexadecimalPower otherDigitsLength
                |> decimalMultiplyByDigit leftMostDecimalDigit
    in
    otherDecimalDigits
        |> List.reverse
        |> List.indexedMap multiplyWith
        |> List.foldl decimalAddBy decimalZero
        |> decimalAddBy leftMostProduct


fromDecimal : Decimal -> Result String Hexadecimal
fromDecimal decimal =
    let
        recursive : Decimal -> List Digit -> Result String Hexadecimal
        recursive currentDecimal accumulatingDigits =
            let
                quotient : Decimal
                quotient =
                    currentDecimal
                        |> decimalDivide16

                resultRemainder : Result String Digit
                resultRemainder =
                    currentDecimal
                        |> decimalModBy16
                        |> fromIntToDigit

                nextRecursive : List Digit -> Result String Hexadecimal
                nextRecursive =
                    recursive quotient
            in
            if quotient == Decimal 0 [] then
                accumulatingDigits
                    |> Ok
                    |> Result.map2 create resultRemainder

            else
                accumulatingDigits
                    |> Ok
                    |> Result.map2 (::) resultRemainder
                    |> Result.andThen nextRecursive
    in
    recursive decimal []



-- HEXADECIMAL STRING


toStringHexadecimal : Hexadecimal -> String
toStringHexadecimal (Hexadecimal { leftMostDigit, otherDigits }) =
    otherDigits
        |> (::) leftMostDigit
        |> List.map fromDigitToChar
        |> String.fromList
        |> (++) "0x"


toStringDecimal : Hexadecimal -> String
toStringDecimal hexadecimal =
    hexadecimal
        |> toDecimal
        |> decimalToString


toStringHexadecimalShort : Hexadecimal -> String
toStringHexadecimalShort hexadecimal =
    let
        string : String
        string =
            toStringHexadecimal hexadecimal

        left : String
        left =
            String.left 6 string

        right : String
        right =
            String.right 4 string
    in
    left ++ "..." ++ right



-- HEXADECIMAL TEXT


toTextHexadecimal : Hexadecimal -> Element msg
toTextHexadecimal hexadecimal =
    Element.text <| toStringHexadecimal hexadecimal


toTextDecimal : Hexadecimal -> Element msg
toTextDecimal hexadecimal =
    Element.text <| toStringDecimal hexadecimal


toTextHexadecimalShort : Hexadecimal -> Element msg
toTextHexadecimalShort hexadecimal =
    Element.text <| toStringHexadecimalShort hexadecimal



-- HEXADECIMAL INPUT


type alias InputDetails msg =
    { onChange : String -> msg
    , hexadecimal : Maybe Hexadecimal
    , placeholder : Maybe (Placeholder msg)
    , label : Label msg
    }


toInputTextHexadecimal : List (Attribute msg) -> InputDetails msg -> Element msg
toInputTextHexadecimal =
    toInputText toStringHexadecimal


toInputTextDecimal : List (Attribute msg) -> InputDetails msg -> Element msg
toInputTextDecimal =
    toInputText toStringDecimal


toInputText : (Hexadecimal -> String) -> List (Attribute msg) -> InputDetails msg -> Element msg
toInputText function attributes { onChange, hexadecimal, placeholder, label } =
    let
        text : String
        text =
            case hexadecimal of
                Just currentHexadecimal ->
                    function currentHexadecimal

                Nothing ->
                    ""
    in
    Input.text attributes
        { onChange = onChange
        , text = text
        , placeholder = placeholder
        , label = label
        }



-- HEXADECIMAL HELPER


equal : Hexadecimal -> Hexadecimal -> Bool
equal (Hexadecimal first) (Hexadecimal second) =
    first == second


length : Hexadecimal -> Int
length (Hexadecimal { otherDigits }) =
    List.length otherDigits + 1


addBy : Hexadecimal -> Hexadecimal -> Result String Hexadecimal
addBy first second =
    let
        firstDecimal : Decimal
        firstDecimal =
            toDecimal first

        secondDecimal : Decimal
        secondDecimal =
            toDecimal second
    in
    firstDecimal
        |> decimalAddBy secondDecimal
        |> fromDecimal


padUpto64 : Hexadecimal -> Hexadecimal
padUpto64 ((Hexadecimal { leftMostDigit, otherDigits }) as hexadecimal) =
    let
        numberOfZeroes : Int
        numberOfZeroes =
            63 - length hexadecimal

        zeroes : List Digit
        zeroes =
            List.repeat numberOfZeroes Zero
    in
    otherDigits
        |> (::) leftMostDigit
        |> (++) zeroes
        |> create Zero


remove0 : Hexadecimal -> Hexadecimal
remove0 (Hexadecimal { leftMostDigit, otherDigits }) =
    let
        recursive : Digit -> List Digit -> Hexadecimal
        recursive currentLeft currentOther =
            case ( currentLeft, currentOther ) of
                ( Zero, [] ) ->
                    zero

                ( Zero, singleton :: remaining ) ->
                    recursive singleton remaining

                _ ->
                    create currentLeft currentOther
    in
    recursive leftMostDigit otherDigits


append : Hexadecimal -> Hexadecimal -> Hexadecimal
append (Hexadecimal first) (Hexadecimal second) =
    second.otherDigits
        |> (::) second.leftMostDigit
        |> (++) first.otherDigits
        |> create first.leftMostDigit



-- HEXADECIMAL CONSTANT


zero : Hexadecimal
zero =
    create Zero []


maxUnsignedInteger : Hexadecimal
maxUnsignedInteger =
    create LowercaseF <| List.repeat 63 LowercaseF


counter : Hexadecimal
counter =
    Hexadecimal
        { leftMostDigit = Six
        , otherDigits =
            [ One
            , LowercaseB
            , LowercaseC
            , Two
            , Two
            , One
            , LowercaseA
            ]
        }


increment : Hexadecimal
increment =
    Hexadecimal
        { leftMostDigit = LowercaseD
        , otherDigits =
            [ Zero
            , Nine
            , LowercaseD
            , LowercaseE
            , Zero
            , Eight
            , LowercaseA
            ]
        }


decrement : Hexadecimal
decrement =
    Hexadecimal
        { leftMostDigit = Two
        , otherDigits =
            [ LowercaseB
            , LowercaseA
            , LowercaseE
            , LowercaseC
            , LowercaseE
            , LowercaseB
            , Seven
            ]
        }


contract : Hexadecimal
contract =
    Hexadecimal
        { leftMostDigit = LowercaseB
        , otherDigits =
            [ Five
            , LowercaseD
            , Six
            , LowercaseD
            , UppercaseC
            , Four
            , UppercaseD
            , UppercaseA
            , Four
            , LowercaseD
            , Nine
            , Four
            , Five
            , UppercaseD
            , Six
            , UppercaseF
            , UppercaseA
            , Five
            , UppercaseD
            , UppercaseA
            , Four
            , Six
            , UppercaseC
            , Two
            , Zero
            , Five
            , UppercaseB
            , LowercaseD
            , Nine
            , Nine
            , Two
            , Five
            , Two
            , Three
            , UppercaseA
            , LowercaseA
            , Nine
            , Four
            , Zero
            ]
        }



-- DECIMAL


type alias Decimal =
    { leftMostDigit : Int
    , otherDigits : List Int
    }


decimalFromString : String -> Result String Decimal
decimalFromString string =
    let
        initial : List Char -> Result String Decimal
        initial initialCharacters =
            recursive initialCharacters []

        recursive : List Char -> List Int -> Result String Decimal
        recursive characters accumulatingIntegers =
            case ( characters, accumulatingIntegers ) of
                ( [], [] ) ->
                    Err "Cannot be empty string"

                ( [], singletonInteger :: remainingIntegers ) ->
                    Ok <| Decimal singletonInteger remainingIntegers

                ( singletonCharacter :: remainingCharacters, integers ) ->
                    let
                        resultInteger : Result String Int
                        resultInteger =
                            fromCharToInt singletonCharacter

                        recursiveRemaining : List Int -> Result String Decimal
                        recursiveRemaining =
                            recursive remainingCharacters
                    in
                    integers
                        |> Result.Ok
                        |> Result.map2 (::) resultInteger
                        |> Result.andThen recursiveRemaining
    in
    string
        |> String.toList
        |> List.reverse
        |> initial


decimalToString : Decimal -> String
decimalToString decimal =
    let
        normal : Decimal
        normal =
            decimalRemove0 decimal
    in
    normal.otherDigits
        |> (::) normal.leftMostDigit
        |> List.map String.fromInt
        |> String.concat



-- DECIMAL HELPER


decimalRemove0 : Decimal -> Decimal
decimalRemove0 ({ leftMostDigit, otherDigits } as decimal) =
    case ( leftMostDigit, otherDigits ) of
        ( 0, singletonDigit :: remainingDigits ) ->
            decimalRemove0 <| Decimal singletonDigit remainingDigits

        _ ->
            decimal


decimalAddBy : Decimal -> Decimal -> Decimal
decimalAddBy first second =
    let
        long : Decimal
        long =
            if decimalLength first >= decimalLength second then
                first

            else
                second

        shortNoPad : Decimal
        shortNoPad =
            if decimalLength first >= decimalLength second then
                second

            else
                first

        numberOfZeroes : Int
        numberOfZeroes =
            decimalLength long - decimalLength shortNoPad

        short : Decimal
        short =
            decimalPad numberOfZeroes shortNoPad

        leftMostSum : Int
        leftMostSum =
            long.leftMostDigit + short.leftMostDigit

        recursive : Int -> List Int -> List Int -> Decimal
        recursive increase currentList accumulatingList =
            case currentList of
                [] ->
                    Decimal increase accumulatingList

                singleton :: remaining ->
                    let
                        singletonIncrease : Int
                        singletonIncrease =
                            singleton + increase

                        ones : Int
                        ones =
                            singletonIncrease
                                |> modBy 10

                        tens : Int
                        tens =
                            singletonIncrease // 10
                    in
                    accumulatingList
                        |> (::) ones
                        |> recursive tens remaining

        initial : List Int -> Decimal
        initial currentList =
            recursive 0 currentList []

        addLeftMostDigit : Int -> Decimal -> Decimal
        addLeftMostDigit integer { leftMostDigit, otherDigits } =
            let
                leftMostSumIncrease : Int
                leftMostSumIncrease =
                    integer + leftMostDigit

                ones : Int
                ones =
                    leftMostSumIncrease
                        |> modBy 10

                tens : Int
                tens =
                    leftMostSumIncrease // 10
            in
            if tens == 0 then
                Decimal ones otherDigits

            else
                otherDigits
                    |> (::) ones
                    |> Decimal tens
                    |> addLeftMostDigit 0
    in
    List.map2 (+) long.otherDigits short.otherDigits
        |> List.reverse
        |> initial
        |> addLeftMostDigit leftMostSum


decimalMultiplyByDigit : Int -> Decimal -> Decimal
decimalMultiplyByDigit digit decimal =
    let
        leftMostProduct : Int
        leftMostProduct =
            digit * decimal.leftMostDigit

        recursive : Int -> List Int -> List Int -> Decimal
        recursive increase currentList accumulatingList =
            case currentList of
                [] ->
                    Decimal increase accumulatingList

                singleton :: remaining ->
                    let
                        singletonIncrease : Int
                        singletonIncrease =
                            singleton + increase

                        ones : Int
                        ones =
                            singletonIncrease
                                |> modBy 10

                        tens : Int
                        tens =
                            singletonIncrease // 10
                    in
                    accumulatingList
                        |> (::) ones
                        |> recursive tens remaining

        multiplyByDigit : Int -> Int
        multiplyByDigit =
            (*) digit

        initial : List Int -> Decimal
        initial currentList =
            recursive 0 currentList []

        addLeftMostDigit : Int -> Decimal -> Decimal
        addLeftMostDigit integer { leftMostDigit, otherDigits } =
            let
                leftMostSumIncrease : Int
                leftMostSumIncrease =
                    integer + leftMostDigit

                ones : Int
                ones =
                    leftMostSumIncrease
                        |> modBy 10

                tens : Int
                tens =
                    leftMostSumIncrease // 10
            in
            if tens == 0 then
                Decimal ones otherDigits

            else
                otherDigits
                    |> (::) ones
                    |> Decimal tens
                    |> addLeftMostDigit 0
    in
    decimal.otherDigits
        |> List.map multiplyByDigit
        |> List.reverse
        |> initial
        |> addLeftMostDigit leftMostProduct


decimalHexadecimalPower : Int -> Decimal
decimalHexadecimalPower integer =
    case integer of
        0 ->
            Decimal 1 []

        notZero ->
            let
                recursive : Int -> Decimal -> Decimal
                recursive currentInteger accumulatingDecimal =
                    if currentInteger == 0 then
                        accumulatingDecimal

                    else
                        let
                            nextInteger : Int
                            nextInteger =
                                currentInteger - 1
                        in
                        accumulatingDecimal
                            |> decimalMultiplyByDigit 16
                            |> recursive nextInteger

                initial : Int -> Decimal
                initial currentInteger =
                    recursive currentInteger <| Decimal 1 []
            in
            initial notZero


decimalDivide16 : Decimal -> Decimal
decimalDivide16 { leftMostDigit, otherDigits } =
    let
        recursive : Int -> List Int -> List Int -> List Int
        recursive increase integers accumulatingIntegers =
            case integers of
                [] ->
                    accumulatingIntegers

                singletonInteger :: remainingIntegers ->
                    let
                        dividend : Int
                        dividend =
                            increase * 10 + singletonInteger

                        quotient : Int
                        quotient =
                            dividend // 16

                        remainder : Int
                        remainder =
                            dividend
                                |> modBy 16
                    in
                    accumulatingIntegers
                        |> (::) quotient
                        |> recursive remainder remainingIntegers
    in
    case otherDigits of
        [] ->
            Decimal 0 []

        singletonDigit :: remainingDigits ->
            let
                leftMostDividend : Int
                leftMostDividend =
                    leftMostDigit * 10 + singletonDigit

                leftMostQuotient : Int
                leftMostQuotient =
                    leftMostDividend // 16

                remainder : Int
                remainder =
                    leftMostDividend
                        |> modBy 16
            in
            recursive remainder remainingDigits []
                |> List.reverse
                |> Decimal leftMostQuotient


decimalModBy16 : Decimal -> Int
decimalModBy16 { leftMostDigit, otherDigits } =
    let
        recursive : Int -> List Int -> Int
        recursive increase integers =
            case integers of
                [] ->
                    increase

                singletonInteger :: remainingIntegers ->
                    let
                        dividend : Int
                        dividend =
                            increase * 10 + singletonInteger

                        remainder : Int
                        remainder =
                            dividend
                                |> modBy 16
                    in
                    recursive remainder remainingIntegers
    in
    case otherDigits of
        [] ->
            leftMostDigit

        singletonDigit :: remainingDigits ->
            let
                leftMostDividend : Int
                leftMostDividend =
                    leftMostDigit * 10 + singletonDigit

                remainder : Int
                remainder =
                    leftMostDividend
                        |> modBy 16
            in
            recursive remainder remainingDigits


decimalLength : Decimal -> Int
decimalLength { otherDigits } =
    List.length otherDigits + 1


decimalPad : Int -> Decimal -> Decimal
decimalPad amount ({ leftMostDigit, otherDigits } as decimal) =
    case amount of
        0 ->
            decimal

        notZero ->
            let
                numberOfZeroes : Int
                numberOfZeroes =
                    notZero - 1

                zeroes : List Int
                zeroes =
                    List.repeat numberOfZeroes 0
            in
            otherDigits
                |> (::) leftMostDigit
                |> (++) zeroes
                |> Decimal 0



-- OTHER


fromCharToDigit : Char -> Result String Digit
fromCharToDigit character =
    case character of
        '0' ->
            Ok Zero

        '1' ->
            Ok One

        '2' ->
            Ok Two

        '3' ->
            Ok Three

        '4' ->
            Ok Four

        '5' ->
            Ok Five

        '6' ->
            Ok Six

        '7' ->
            Ok Seven

        '8' ->
            Ok Eight

        '9' ->
            Ok Nine

        'a' ->
            Ok LowercaseA

        'A' ->
            Ok UppercaseA

        'b' ->
            Ok LowercaseB

        'B' ->
            Ok UppercaseB

        'c' ->
            Ok LowercaseC

        'C' ->
            Ok UppercaseC

        'd' ->
            Ok LowercaseD

        'D' ->
            Ok UppercaseD

        'e' ->
            Ok LowercaseE

        'E' ->
            Ok UppercaseE

        'f' ->
            Ok LowercaseF

        'F' ->
            Ok UppercaseF

        _ ->
            Err "Not hexadecimalable"


fromDigitToChar : Digit -> Char
fromDigitToChar digit =
    case digit of
        Zero ->
            '0'

        One ->
            '1'

        Two ->
            '2'

        Three ->
            '3'

        Four ->
            '4'

        Five ->
            '5'

        Six ->
            '6'

        Seven ->
            '7'

        Eight ->
            '8'

        Nine ->
            '9'

        LowercaseA ->
            'a'

        UppercaseA ->
            'A'

        LowercaseB ->
            'b'

        UppercaseB ->
            'B'

        LowercaseC ->
            'c'

        UppercaseC ->
            'C'

        LowercaseD ->
            'd'

        UppercaseD ->
            'D'

        LowercaseE ->
            'e'

        UppercaseE ->
            'E'

        LowercaseF ->
            'f'

        UppercaseF ->
            'F'


fromDigitToInt : Digit -> Int
fromDigitToInt digit =
    case digit of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        LowercaseA ->
            10

        UppercaseA ->
            10

        LowercaseB ->
            11

        UppercaseB ->
            11

        LowercaseC ->
            12

        UppercaseC ->
            12

        LowercaseD ->
            13

        UppercaseD ->
            13

        LowercaseE ->
            14

        UppercaseE ->
            14

        LowercaseF ->
            15

        UppercaseF ->
            15


fromCharToInt : Char -> Result String Int
fromCharToInt character =
    case character of
        '0' ->
            Ok 0

        '1' ->
            Ok 1

        '2' ->
            Ok 2

        '3' ->
            Ok 3

        '4' ->
            Ok 4

        '5' ->
            Ok 5

        '6' ->
            Ok 6

        '7' ->
            Ok 7

        '8' ->
            Ok 8

        '9' ->
            Ok 9

        _ ->
            Err "Not decimalable"


fromIntToDigit : Int -> Result String Digit
fromIntToDigit integer =
    case integer of
        0 ->
            Ok Zero

        1 ->
            Ok One

        2 ->
            Ok Two

        3 ->
            Ok Three

        4 ->
            Ok Four

        5 ->
            Ok Five

        6 ->
            Ok Six

        7 ->
            Ok Seven

        8 ->
            Ok Eight

        9 ->
            Ok Nine

        10 ->
            Ok LowercaseA

        11 ->
            Ok LowercaseB

        12 ->
            Ok LowercaseC

        13 ->
            Ok LowercaseD

        14 ->
            Ok LowercaseE

        15 ->
            Ok LowercaseF

        _ ->
            Err "Not Hexadecimalable"

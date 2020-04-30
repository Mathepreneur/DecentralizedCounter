port module Main exposing (main)

-- IMPORT

import Browser
import Browser.Events
import Contract exposing (Case, Contract, Log, Outcome)
import Data exposing (Address, UnsignedInteger)
import Element exposing (Attribute, Color, Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Html exposing (Html)
import Image
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode exposing (Value)
import Link
import Network exposing (Network)
import Time exposing (Posix)



-- MODEL


type alias Model =
    { device : Device
    , state : State
    , error : Maybe String
    }


type State
    = Kovan Info
    | NotConnected
    | NoMetamask


type alias Info =
    { user : Address
    , counter : Maybe UnsignedInteger
    , log : Log
    }



-- MAIN


main : Program Flag Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Flag =
    { width : Int
    , height : Int
    , hasMetamask : Bool
    }


init : Flag -> ( Model, Cmd Msg )
init { width, height, hasMetamask } =
    let
        device : Device
        device =
            Element.classifyDevice
                { width = width
                , height = height
                }

        state : State
        state =
            if hasMetamask then
                NotConnected

            else
                NoMetamask
    in
    ( Model device state Nothing, Cmd.none )



-- MESSAGE


type Msg
    = ChangeWindow Int Int
    | SendConnect
    | ReceiveConnect Value
    | ReceiveUser Value
    | ReceiveNetwork Value
    | ReceiveTransaction Value
    | CheckCounter Posix
    | SendIncrement
    | SendDecrement
    | CloseError



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state } as model) =
    case ( msg, state ) of
        ( ChangeWindow width height, _ ) ->
            updateViewPort width height model

        ( SendConnect, NotConnected ) ->
            ( model, sendConnect () )

        ( ReceiveConnect value, NotConnected ) ->
            updateConnect value model

        ( ReceiveUser value, Kovan info ) ->
            updateUser value info model

        ( ReceiveNetwork value, Kovan info ) ->
            updateNetwork value info model

        ( ReceiveTransaction value, Kovan info ) ->
            updateTransaction value info model

        ( CheckCounter _, Kovan info ) ->
            callCounter info model

        ( SendIncrement, Kovan info ) ->
            sendIncrement info model

        ( SendDecrement, Kovan info ) ->
            sendDecrement info model

        ( CloseError, _ ) ->
            closeError model

        _ ->
            ( model, Cmd.none )


updateViewPort : Int -> Int -> Model -> ( Model, Cmd Msg )
updateViewPort width height model =
    let
        device : Device
        device =
            Element.classifyDevice
                { width = width
                , height = height
                }
    in
    ( { model | device = device }, Cmd.none )


updateConnect : Value -> Model -> ( Model, Cmd Msg )
updateConnect value model =
    let
        resultConnect : Result Decode.Error ResultConnect
        resultConnect =
            Decode.decodeValue decoderConnect value
    in
    case resultConnect of
        Ok { network, user } ->
            case ( network, user ) of
                ( Ok _, Ok address ) ->
                    let
                        outcome : Outcome
                        outcome =
                            Contract.counter Contract.initialLog

                        contract : Contract
                        contract =
                            outcome.contract

                        nextLog : Log
                        nextLog =
                            outcome.nextLog
                    in
                    ( { model | state = Kovan <| Info address Nothing nextLog }
                    , sendTransaction <| Contract.encode contract
                    )

                ( Err error, _ ) ->
                    ( { model | error = Just error }, Cmd.none )

                ( _, Err error ) ->
                    ( { model | error = Just error }, Cmd.none )

        Err decodeError ->
            ( { model | error = Just <| Decode.errorToString decodeError }
            , Cmd.none
            )


type alias ResultConnect =
    { network : Result String Network
    , user : Result String Address
    }


decoderConnect : Decoder ResultConnect
decoderConnect =
    Decode.succeed ResultConnect
        |> Pipeline.required "network" Network.decoder
        |> Pipeline.required "user" Data.addressDecoder


updateUser : Value -> Info -> Model -> ( Model, Cmd Msg )
updateUser value info model =
    let
        resultAddress : Result Decode.Error (Maybe (Result String Address))
        resultAddress =
            Decode.decodeValue (Decode.nullable Data.addressDecoder) value
    in
    case resultAddress of
        Ok (Just (Ok address)) ->
            let
                nextInfo : Info
                nextInfo =
                    { info | user = address }
            in
            ( { model | state = Kovan nextInfo }
            , Cmd.none
            )

        Ok (Just (Err error)) ->
            ( { model | state = NotConnected, error = Just error }
            , Cmd.none
            )

        Ok Nothing ->
            ( { model | state = NotConnected, error = Just "You have been logged out. Decentralized Counter only works with Kovan Test Network. Please switch your network on Metamask." }
            , Cmd.none
            )

        Err decodeError ->
            ( { model | error = Just <| Decode.errorToString decodeError }
            , Cmd.none
            )


updateNetwork : Value -> Info -> Model -> ( Model, Cmd Msg )
updateNetwork value info model =
    let
        resultNetwork : Result Decode.Error (Result String Network)
        resultNetwork =
            Decode.decodeValue Network.decoder value
    in
    case resultNetwork of
        Ok (Ok _) ->
            ( { model | state = Kovan info }, Cmd.none )

        Ok (Err error) ->
            ( { model | state = NotConnected, error = Just error }
            , Cmd.none
            )

        Err decodeError ->
            ( { model | error = Just <| Decode.errorToString decodeError }
            , Cmd.none
            )


updateTransaction : Value -> Info -> Model -> ( Model, Cmd Msg )
updateTransaction value ({ log } as info) model =
    Contract.receiveDecode contractCase log value info model


contractCase : Case Info Model Msg
contractCase =
    { counterCase = counterCase
    , incrementCase = incrementCase
    , decrementCase = decrementCase
    }


counterCase : Maybe UnsignedInteger -> Info -> Model -> ( Model, Cmd Msg )
counterCase unsignedInteger info ({ error } as model) =
    let
        nextInfo : Info
        nextInfo =
            { info | counter = unsignedInteger }
    in
    case error of
        Just _ ->
            ( model, Cmd.none )

        Nothing ->
            ( { model | state = Kovan nextInfo }, Cmd.none )


incrementCase : Model -> ( Model, Cmd Msg )
incrementCase model =
    ( model, Cmd.none )


decrementCase : Model -> ( Model, Cmd Msg )
decrementCase model =
    ( model, Cmd.none )


callCounter : Info -> Model -> ( Model, Cmd Msg )
callCounter ({ log } as info) model =
    let
        outcome : Outcome
        outcome =
            Contract.counter log

        contract : Contract
        contract =
            outcome.contract

        nextLog : Log
        nextLog =
            outcome.nextLog

        nextInfo : Info
        nextInfo =
            { info | log = nextLog }
    in
    ( { model | state = Kovan nextInfo }
    , sendTransaction <| Contract.encode contract
    )


sendIncrement : Info -> Model -> ( Model, Cmd Msg )
sendIncrement ({ user, counter, log } as info) model =
    case counter of
        Just unsignedInteger ->
            if unsignedInteger == Data.unsignedIntegerMaxInteger then
                ( model, Cmd.none )

            else
                let
                    outcome : Outcome
                    outcome =
                        Contract.increment user log

                    contract : Contract
                    contract =
                        outcome.contract

                    nextLog : Log
                    nextLog =
                        outcome.nextLog

                    nextInfo : Info
                    nextInfo =
                        { info | log = nextLog }
                in
                ( { model | state = Kovan nextInfo }
                , sendTransaction <| Contract.encode contract
                )

        Nothing ->
            ( model, Cmd.none )


sendDecrement : Info -> Model -> ( Model, Cmd Msg )
sendDecrement ({ user, counter, log } as info) model =
    case counter of
        Just unsignedInteger ->
            if unsignedInteger == Data.unsignedIntegerZero then
                ( model, Cmd.none )

            else
                let
                    outcome : Outcome
                    outcome =
                        Contract.decrement user log

                    contract : Contract
                    contract =
                        outcome.contract

                    nextLog : Log
                    nextLog =
                        outcome.nextLog

                    nextInfo : Info
                    nextInfo =
                        { info | log = nextLog }
                in
                ( { model | state = Kovan nextInfo }
                , sendTransaction <| Contract.encode contract
                )

        Nothing ->
            ( model, Cmd.none )


closeError : Model -> ( Model, Cmd Msg )
closeError model =
    ( { model | error = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { state } =
    case state of
        Kovan _ ->
            Sub.batch
                [ Browser.Events.onResize ChangeWindow
                , receiveUser ReceiveUser
                , receiveNetwork ReceiveNetwork
                , receiveTransaction ReceiveTransaction
                , Time.every 10000 CheckCounter
                ]

        NotConnected ->
            Sub.batch
                [ Browser.Events.onResize ChangeWindow
                , receiveConnect ReceiveConnect
                ]

        NoMetamask ->
            Browser.Events.onResize ChangeWindow



-- PORT


port sendConnect : () -> Cmd msg


port sendTransaction : Value -> Cmd msg


port receiveConnect : (Value -> msg) -> Sub msg


port receiveUser : (Value -> msg) -> Sub msg


port receiveNetwork : (Value -> msg) -> Sub msg


port receiveTransaction : (Value -> msg) -> Sub msg



-- VIEW


view : Model -> Html Msg
view ({ device, error } as model) =
    let
        inFrontError : List (Attribute Msg)
        inFrontError =
            case error of
                Just string ->
                    [ Element.inFront <| Lazy.lazy2 viewError device string ]

                Nothing ->
                    []
    in
    helper Element.layout
        [ fillWidthAndHeightAttributes
        , inFrontError
        ]
    <|
        viewApp model



-- ERROR


viewError : Device -> String -> Element Msg
viewError { class } error =
    let
        viewErrorWith : String -> Element Msg
        viewErrorWith =
            case class of
                Element.Phone ->
                    viewErrorBoxPhone

                Element.Tablet ->
                    viewErrorBoxTablet

                Element.Desktop ->
                    viewErrorBoxDesktop

                Element.BigDesktop ->
                    viewErrorBoxBigDesktop
    in
    helper Element.el
        [ fillWidthAndHeightAttributes
        , [ Background.color transparentBlack ]
        ]
    <|
        viewErrorWith error


viewErrorBoxPhone : String -> Element Msg
viewErrorBoxPhone error =
    viewErrorBox 300
        [ viewErrorHeaderPhone
        , viewErrorBodyPhone error
        ]


viewErrorBoxTablet : String -> Element Msg
viewErrorBoxTablet error =
    viewErrorBox 360
        [ viewErrorHeaderTablet
        , viewErrorBodyTablet error
        ]


viewErrorBoxDesktop : String -> Element Msg
viewErrorBoxDesktop error =
    viewErrorBox 420
        [ viewErrorHeaderDesktop
        , viewErrorBodyDesktop error
        ]


viewErrorBoxBigDesktop : String -> Element Msg
viewErrorBoxBigDesktop error =
    viewErrorBox 480
        [ viewErrorHeaderBigDesktop
        , viewErrorBodyBigDesktop error
        ]


viewErrorBox : Int -> List (Element Msg) -> Element Msg
viewErrorBox width =
    helper Element.column
        [ alignCenterAttributes
        , [ Element.width <| Element.px width
          , Element.height Element.shrink
          ]
        ]


viewErrorHeaderPhone : Element Msg
viewErrorHeaderPhone =
    viewErrorHeader closeButtonPhone


viewErrorHeaderTablet : Element Msg
viewErrorHeaderTablet =
    viewErrorHeader closeButtonTablet


viewErrorHeaderDesktop : Element Msg
viewErrorHeaderDesktop =
    viewErrorHeader closeButtonDesktop


viewErrorHeaderBigDesktop : Element Msg
viewErrorHeaderBigDesktop =
    viewErrorHeader closeButtonBigDesktop


viewErrorHeader : Element Msg -> Element Msg
viewErrorHeader =
    helper Element.el
        [ fillWidthAttributes
        , [ Background.color sherpaBlue ]
        ]


closeButtonPhone : Element Msg
closeButtonPhone =
    viewCloseButton xDetailsPhone


closeButtonTablet : Element Msg
closeButtonTablet =
    viewCloseButton xDetailsTablet


closeButtonDesktop : Element Msg
closeButtonDesktop =
    viewCloseButton xDetailsDesktop


closeButtonBigDesktop : Element Msg
closeButtonBigDesktop =
    viewCloseButton xDetailsBigDesktop


viewCloseButton : SizeDetails -> Element Msg
viewCloseButton { padding, fontSize } =
    helper Input.button
        [ shrinkWidthAndHeightAttributes
        , fontWhiteAttributes fontSize
        , [ Element.padding padding
          , Element.alignRight
          ]
        ]
        { onPress = Just CloseError
        , label = Element.text "X"
        }


viewErrorBodyPhone : String -> Element Msg
viewErrorBodyPhone =
    viewErrorBody errorDetailsPhone


viewErrorBodyTablet : String -> Element Msg
viewErrorBodyTablet =
    viewErrorBody errorDetailsTablet


viewErrorBodyDesktop : String -> Element Msg
viewErrorBodyDesktop =
    viewErrorBody errorDetailsDesktop


viewErrorBodyBigDesktop : String -> Element Msg
viewErrorBodyBigDesktop =
    viewErrorBody errorDetailsBigDesktop


viewErrorBody : SizeDetails -> String -> Element Msg
viewErrorBody { padding, fontSize } error =
    helper Element.paragraph
        [ fillWidthAttributes
        , fontSherpaBlueAttributes fontSize
        , [ Element.padding padding
          , Background.color white
          ]
        ]
        [ Element.text error ]



-- APP


viewApp : Model -> Element Msg
viewApp { device, state } =
    let
        viewHeader : Element Msg
        viewHeader =
            case ( device.class, state ) of
                ( Element.Phone, Kovan { user } ) ->
                    Lazy.lazy viewHeaderPhone user

                ( Element.Tablet, Kovan { user } ) ->
                    Lazy.lazy viewHeaderTablet user

                ( Element.Desktop, Kovan { user } ) ->
                    Lazy.lazy viewHeaderDesktop user

                ( Element.BigDesktop, Kovan { user } ) ->
                    Lazy.lazy viewHeaderBigDesktop user

                ( Element.Phone, _ ) ->
                    viewHeaderPhoneNoUser

                ( Element.Tablet, _ ) ->
                    viewHeaderTabletNoUser

                ( Element.Desktop, _ ) ->
                    viewHeaderDesktopNoUser

                ( Element.BigDesktop, _ ) ->
                    viewHeaderBigDesktopNoUser

        viewBody : Element Msg
        viewBody =
            case ( device.class, device.orientation, state ) of
                ( Element.Phone, Element.Portrait, Kovan { counter } ) ->
                    Lazy.lazy viewBodyPhonePortrait counter

                ( Element.Tablet, Element.Portrait, Kovan { counter } ) ->
                    Lazy.lazy viewBodyTabletPortrait counter

                ( Element.Desktop, Element.Portrait, Kovan { counter } ) ->
                    Lazy.lazy viewBodyDesktopPortrait counter

                ( Element.BigDesktop, Element.Portrait, Kovan { counter } ) ->
                    Lazy.lazy viewBodyBigDesktopPortrait counter

                ( Element.Phone, Element.Landscape, Kovan { counter } ) ->
                    Lazy.lazy viewBodyPhoneLandscape counter

                ( Element.Tablet, Element.Landscape, Kovan { counter } ) ->
                    Lazy.lazy viewBodyTabletLandscape counter

                ( Element.Desktop, Element.Landscape, Kovan { counter } ) ->
                    Lazy.lazy viewBodyDesktopLandscape counter

                ( Element.BigDesktop, Element.Landscape, Kovan { counter } ) ->
                    Lazy.lazy viewBodyBigDesktopLandscape counter

                ( Element.Phone, Element.Portrait, NotConnected ) ->
                    viewBodyPhonePortraitNotConnected

                ( Element.Tablet, Element.Portrait, NotConnected ) ->
                    viewBodyTabletPortraitNotConnected

                ( Element.Desktop, Element.Portrait, NotConnected ) ->
                    viewBodyDesktopPortraitNotConnected

                ( Element.BigDesktop, Element.Portrait, NotConnected ) ->
                    viewBodyBigDesktopPortraitNotConnected

                ( Element.Phone, Element.Landscape, NotConnected ) ->
                    viewBodyPhoneLandscapeNotConnected

                ( Element.Tablet, Element.Landscape, NotConnected ) ->
                    viewBodyTabletLandscapeNotConnected

                ( Element.Desktop, Element.Landscape, NotConnected ) ->
                    viewBodyDesktopLandscapeNotConnected

                ( Element.BigDesktop, Element.Landscape, NotConnected ) ->
                    viewBodyBigDesktopLandscapeNotConnected

                ( Element.Phone, Element.Portrait, NoMetamask ) ->
                    viewBodyPhonePortraitNoMetamask

                ( Element.Tablet, Element.Portrait, NoMetamask ) ->
                    viewBodyTabletPortraitNoMetamask

                ( Element.Desktop, Element.Portrait, NoMetamask ) ->
                    viewBodyDesktopPortraitNoMetamask

                ( Element.BigDesktop, Element.Portrait, NoMetamask ) ->
                    viewBodyBigDesktopPortraitNoMetamask

                ( Element.Phone, Element.Landscape, NoMetamask ) ->
                    viewBodyPhoneLandscapeNoMetamask

                ( Element.Tablet, Element.Landscape, NoMetamask ) ->
                    viewBodyTabletLandscapeNoMetamask

                ( Element.Desktop, Element.Landscape, NoMetamask ) ->
                    viewBodyDesktopLandscapeNoMetamask

                ( Element.BigDesktop, Element.Landscape, NoMetamask ) ->
                    viewBodyBigDesktopLandscapeNoMetamask
    in
    Element.column fillWidthAndHeightAttributes
        [ viewHeader
        , viewBody
        ]



-- HEADER


viewHeaderPhone : Address -> Element Msg
viewHeaderPhone user =
    viewHeaderWithPadding 12
        [ viewTitlePhone
        , viewUserPhone user
        ]


viewHeaderTablet : Address -> Element Msg
viewHeaderTablet user =
    viewHeaderWithPadding 18
        [ viewTitleTablet
        , viewUserTablet user
        ]


viewHeaderDesktop : Address -> Element Msg
viewHeaderDesktop user =
    viewHeaderWithPadding 24
        [ viewTitleDesktop
        , viewUserDesktop user
        ]


viewHeaderBigDesktop : Address -> Element Msg
viewHeaderBigDesktop user =
    viewHeaderWithPadding 30
        [ viewTitleBigDesktop
        , viewUserBigDesktop user
        ]



-- HEADER USER


viewUserPhone : Address -> Element Msg
viewUserPhone =
    viewUser buttonSizeDetailsPhone


viewUserTablet : Address -> Element Msg
viewUserTablet =
    viewUser buttonSizeDetailsTablet


viewUserDesktop : Address -> Element Msg
viewUserDesktop =
    viewUser buttonSizeDetailsDesktop


viewUserBigDesktop : Address -> Element Msg
viewUserBigDesktop =
    viewUser buttonSizeDetailsBigDesktop


viewUser : ButtonSizeDetails -> Address -> Element Msg
viewUser { width, padding, fontSize } address =
    helper Element.el
        [ shrinkButtonAttributes width
        , fontWhiteAttributes fontSize
        , [ Element.padding padding
          , Element.alignRight
          , Element.centerY
          , Background.color sherpaBlue
          , Border.color white
          , Border.width 6
          ]
        ]
    <|
        Data.fromAddressToTextShort address


viewHeaderWithPadding : Int -> List (Element Msg) -> Element Msg
viewHeaderWithPadding distance =
    helper Element.row
        [ fillWidthAttributes
        , paddingAndSpacing distance
        , [ Background.color sherpaBlue ]
        ]



-- HEADER NO USER


viewHeaderPhoneNoUser : Element Msg
viewHeaderPhoneNoUser =
    viewHeaderNoUserWithPadding 12 viewTitlePhone


viewHeaderTabletNoUser : Element Msg
viewHeaderTabletNoUser =
    viewHeaderNoUserWithPadding 18 viewTitleTablet


viewHeaderDesktopNoUser : Element Msg
viewHeaderDesktopNoUser =
    viewHeaderNoUserWithPadding 24 viewTitleDesktop


viewHeaderBigDesktopNoUser : Element Msg
viewHeaderBigDesktopNoUser =
    viewHeaderNoUserWithPadding 30 viewTitleBigDesktop


viewHeaderNoUserWithPadding : Int -> Element Msg -> Element Msg
viewHeaderNoUserWithPadding distance =
    helper Element.el
        [ fillWidthAttributes
        , [ Element.padding distance
          , Background.color sherpaBlue
          ]
        ]



-- HEADER TITLE


viewTitlePhone : Element Msg
viewTitlePhone =
    viewLogoWithFontSize 36


viewTitleTablet : Element Msg
viewTitleTablet =
    viewLogoWithFontSize 42


viewLogoWithFontSize : Int -> Element Msg
viewLogoWithFontSize =
    viewTitle "-42+"


viewTitleDesktop : Element Msg
viewTitleDesktop =
    viewTitleWithFontSize 48


viewTitleBigDesktop : Element Msg
viewTitleBigDesktop =
    viewTitleWithFontSize 54


viewTitleWithFontSize : Int -> Element Msg
viewTitleWithFontSize =
    viewTitle "Decentralized Counter"


viewTitle : String -> Int -> Element Msg
viewTitle title fontSize =
    helper Element.el
        [ shrinkWidthAndHeightAttributes
        , fontWhiteAttributes fontSize
        , [ Element.alignLeft ]
        ]
    <|
        Element.text title



-- BODY PORTRAIT


viewBodyPhonePortrait : Maybe UnsignedInteger -> Element Msg
viewBodyPhonePortrait =
    viewBodyWith
        { zero = viewBodyPhonePortraitZero
        , maxInteger = viewBodyPhonePortraitMaxInteger
        , complete = viewBodyPhonePortraitComplete
        , loading = viewBodyPhonePortraitLoading
        }


viewBodyTabletPortrait : Maybe UnsignedInteger -> Element Msg
viewBodyTabletPortrait =
    viewBodyWith
        { zero = viewBodyTabletPortraitZero
        , maxInteger = viewBodyTabletPortraitMaxInteger
        , complete = viewBodyTabletPortraitComplete
        , loading = viewBodyTabletPortraitLoading
        }


viewBodyDesktopPortrait : Maybe UnsignedInteger -> Element Msg
viewBodyDesktopPortrait =
    viewBodyWith
        { zero = viewBodyDesktopPortraitZero
        , maxInteger = viewBodyDesktopPortraitMaxInteger
        , complete = viewBodyDesktopPortraitComplete
        , loading = viewBodyDesktopPortraitLoading
        }


viewBodyBigDesktopPortrait : Maybe UnsignedInteger -> Element Msg
viewBodyBigDesktopPortrait =
    viewBodyWith
        { zero = viewBodyBigDesktopPortraitZero
        , maxInteger = viewBodyBigDesktopPortraitMaxInteger
        , complete = viewBodyBigDesktopPortraitComplete
        , loading = viewBodyBigDesktopPortraitLoading
        }


viewBodyPhonePortraitZero : Element Msg
viewBodyPhonePortraitZero =
    viewBodyPhonePortraitWith
        [ viewButtonPhoneIncrement
        , viewCounterPhone Data.unsignedIntegerZero
        , viewButtonPhoneDecrementOff
        ]


viewBodyTabletPortraitZero : Element Msg
viewBodyTabletPortraitZero =
    viewBodyTabletPortraitWith
        [ viewButtonTabletIncrement
        , viewCounterTablet Data.unsignedIntegerZero
        , viewButtonTabletDecrementOff
        ]


viewBodyDesktopPortraitZero : Element Msg
viewBodyDesktopPortraitZero =
    viewBodyDesktopPortraitWith
        [ viewButtonDesktopIncrement
        , viewCounterDesktop Data.unsignedIntegerZero
        , viewButtonDesktopDecrementOff
        ]


viewBodyBigDesktopPortraitZero : Element Msg
viewBodyBigDesktopPortraitZero =
    viewBodyBigDesktopPortraitWith
        [ viewButtonBigDesktopIncrement
        , viewCounterBigDesktop Data.unsignedIntegerZero
        , viewButtonBigDesktopDecrementOff
        ]


viewBodyPhonePortraitMaxInteger : Element Msg
viewBodyPhonePortraitMaxInteger =
    viewBodyPhonePortraitWith
        [ viewButtonPhoneIncrementOff
        , viewCounterPhone Data.unsignedIntegerMaxInteger
        , viewButtonPhoneDecrement
        ]


viewBodyTabletPortraitMaxInteger : Element Msg
viewBodyTabletPortraitMaxInteger =
    viewBodyTabletPortraitWith
        [ viewButtonTabletIncrementOff
        , viewCounterTablet Data.unsignedIntegerMaxInteger
        , viewButtonTabletDecrement
        ]


viewBodyDesktopPortraitMaxInteger : Element Msg
viewBodyDesktopPortraitMaxInteger =
    viewBodyDesktopPortraitWith
        [ viewButtonDesktopIncrementOff
        , viewCounterDesktop Data.unsignedIntegerMaxInteger
        , viewButtonDesktopDecrement
        ]


viewBodyBigDesktopPortraitMaxInteger : Element Msg
viewBodyBigDesktopPortraitMaxInteger =
    viewBodyBigDesktopPortraitWith
        [ viewButtonBigDesktopIncrementOff
        , viewCounterBigDesktop Data.unsignedIntegerMaxInteger
        , viewButtonBigDesktopDecrement
        ]


viewBodyPhonePortraitComplete : UnsignedInteger -> Element Msg
viewBodyPhonePortraitComplete unsignedInteger =
    viewBodyPhonePortraitWith
        [ viewButtonPhoneIncrement
        , viewCounterPhone unsignedInteger
        , viewButtonPhoneDecrement
        ]


viewBodyTabletPortraitComplete : UnsignedInteger -> Element Msg
viewBodyTabletPortraitComplete unsignedInteger =
    viewBodyTabletPortraitWith
        [ viewButtonTabletIncrement
        , viewCounterTablet unsignedInteger
        , viewButtonTabletDecrement
        ]


viewBodyDesktopPortraitComplete : UnsignedInteger -> Element Msg
viewBodyDesktopPortraitComplete unsignedInteger =
    viewBodyDesktopPortraitWith
        [ viewButtonDesktopIncrement
        , viewCounterDesktop unsignedInteger
        , viewButtonDesktopDecrement
        ]


viewBodyBigDesktopPortraitComplete : UnsignedInteger -> Element Msg
viewBodyBigDesktopPortraitComplete unsignedInteger =
    viewBodyBigDesktopPortraitWith
        [ viewButtonBigDesktopIncrement
        , viewCounterBigDesktop unsignedInteger
        , viewButtonBigDesktopDecrement
        ]


viewBodyPhonePortraitLoading : Element Msg
viewBodyPhonePortraitLoading =
    viewBodyPhonePortraitWith
        [ viewButtonPhoneIncrementOff
        , viewLoadingPhone
        , viewButtonPhoneDecrementOff
        ]


viewBodyTabletPortraitLoading : Element Msg
viewBodyTabletPortraitLoading =
    viewBodyTabletPortraitWith
        [ viewButtonTabletIncrementOff
        , viewLoadingTablet
        , viewButtonTabletDecrementOff
        ]


viewBodyDesktopPortraitLoading : Element Msg
viewBodyDesktopPortraitLoading =
    viewBodyDesktopPortraitWith
        [ viewButtonDesktopIncrementOff
        , viewLoadingDesktop
        , viewButtonDesktopDecrementOff
        ]


viewBodyBigDesktopPortraitLoading : Element Msg
viewBodyBigDesktopPortraitLoading =
    viewBodyBigDesktopPortraitWith
        [ viewButtonBigDesktopIncrementOff
        , viewLoadingBigDesktop
        , viewButtonBigDesktopDecrementOff
        ]


viewBodyPhonePortraitWith : List (Element Msg) -> Element Msg
viewBodyPhonePortraitWith =
    viewBodyPortrait 48


viewBodyTabletPortraitWith : List (Element Msg) -> Element Msg
viewBodyTabletPortraitWith =
    viewBodyPortrait 72


viewBodyDesktopPortraitWith : List (Element Msg) -> Element Msg
viewBodyDesktopPortraitWith =
    viewBodyPortrait 96


viewBodyBigDesktopPortraitWith : List (Element Msg) -> Element Msg
viewBodyBigDesktopPortraitWith =
    viewBodyPortrait 120


viewBodyPortrait : Int -> List (Element Msg) -> Element Msg
viewBodyPortrait distance =
    Element.column <| viewBodyAttributes distance



-- BODY LANDSCAPE


viewBodyPhoneLandscape : Maybe UnsignedInteger -> Element Msg
viewBodyPhoneLandscape =
    viewBodyWith
        { zero = viewBodyPhoneLandscapeZero
        , maxInteger = viewBodyPhoneLandscapeMaxInteger
        , complete = viewBodyPhoneLandscapeComplete
        , loading = viewBodyPhoneLandscapeLoading
        }


viewBodyTabletLandscape : Maybe UnsignedInteger -> Element Msg
viewBodyTabletLandscape =
    viewBodyWith
        { zero = viewBodyTabletLandscapeZero
        , maxInteger = viewBodyTabletLandscapeMaxInteger
        , complete = viewBodyTabletLandscapeComplete
        , loading = viewBodyTabletLandscapeLoading
        }


viewBodyDesktopLandscape : Maybe UnsignedInteger -> Element Msg
viewBodyDesktopLandscape =
    viewBodyWith
        { zero = viewBodyDesktopLandscapeZero
        , maxInteger = viewBodyDesktopLandscapeMaxInteger
        , complete = viewBodyDesktopLandscapeComplete
        , loading = viewBodyDesktopLandscapeLoading
        }


viewBodyBigDesktopLandscape : Maybe UnsignedInteger -> Element Msg
viewBodyBigDesktopLandscape =
    viewBodyWith
        { zero = viewBodyBigDesktopLandscapeZero
        , maxInteger = viewBodyBigDesktopLandscapeMaxInteger
        , complete = viewBodyBigDesktopLandscapeComplete
        , loading = viewBodyBigDesktopLandscapeLoading
        }


viewBodyPhoneLandscapeZero : Element Msg
viewBodyPhoneLandscapeZero =
    viewBodyPhoneLandscapeWith
        [ viewButtonPhoneMinusOff
        , viewCounterPhone Data.unsignedIntegerZero
        , viewButtonPhonePlus
        ]


viewBodyTabletLandscapeZero : Element Msg
viewBodyTabletLandscapeZero =
    viewBodyTabletLandscapeWith
        [ viewButtonTabletMinusOff
        , viewCounterTablet Data.unsignedIntegerZero
        , viewButtonTabletPlus
        ]


viewBodyDesktopLandscapeZero : Element Msg
viewBodyDesktopLandscapeZero =
    viewBodyDesktopLandscapeWith
        [ viewButtonDesktopMinusOff
        , viewCounterDesktop Data.unsignedIntegerZero
        , viewButtonDesktopPlus
        ]


viewBodyBigDesktopLandscapeZero : Element Msg
viewBodyBigDesktopLandscapeZero =
    viewBodyBigDesktopLandscapeWith
        [ viewButtonBigDesktopMinusOff
        , viewCounterBigDesktop Data.unsignedIntegerZero
        , viewButtonBigDesktopPlus
        ]


viewBodyPhoneLandscapeMaxInteger : Element Msg
viewBodyPhoneLandscapeMaxInteger =
    viewBodyPhoneLandscapeWith
        [ viewButtonPhoneMinus
        , viewCounterPhone Data.unsignedIntegerMaxInteger
        , viewButtonPhonePlusOff
        ]


viewBodyTabletLandscapeMaxInteger : Element Msg
viewBodyTabletLandscapeMaxInteger =
    viewBodyTabletLandscapeWith
        [ viewButtonTabletMinus
        , viewCounterTablet Data.unsignedIntegerMaxInteger
        , viewButtonTabletPlusOff
        ]


viewBodyDesktopLandscapeMaxInteger : Element Msg
viewBodyDesktopLandscapeMaxInteger =
    viewBodyDesktopLandscapeWith
        [ viewButtonDesktopMinus
        , viewCounterDesktop Data.unsignedIntegerMaxInteger
        , viewButtonDesktopPlusOff
        ]


viewBodyBigDesktopLandscapeMaxInteger : Element Msg
viewBodyBigDesktopLandscapeMaxInteger =
    viewBodyBigDesktopLandscapeWith
        [ viewButtonBigDesktopMinus
        , viewCounterBigDesktop Data.unsignedIntegerMaxInteger
        , viewButtonBigDesktopPlusOff
        ]


viewBodyPhoneLandscapeComplete : UnsignedInteger -> Element Msg
viewBodyPhoneLandscapeComplete unsignedInteger =
    viewBodyPhoneLandscapeWith
        [ viewButtonPhoneMinus
        , viewCounterPhone unsignedInteger
        , viewButtonPhonePlus
        ]


viewBodyTabletLandscapeComplete : UnsignedInteger -> Element Msg
viewBodyTabletLandscapeComplete unsignedInteger =
    viewBodyTabletLandscapeWith
        [ viewButtonTabletMinus
        , viewCounterTablet unsignedInteger
        , viewButtonTabletPlus
        ]


viewBodyDesktopLandscapeComplete : UnsignedInteger -> Element Msg
viewBodyDesktopLandscapeComplete unsignedInteger =
    viewBodyDesktopLandscapeWith
        [ viewButtonDesktopMinus
        , viewCounterDesktop unsignedInteger
        , viewButtonDesktopPlus
        ]


viewBodyBigDesktopLandscapeComplete : UnsignedInteger -> Element Msg
viewBodyBigDesktopLandscapeComplete unsignedInteger =
    viewBodyBigDesktopLandscapeWith
        [ viewButtonBigDesktopMinus
        , viewCounterBigDesktop unsignedInteger
        , viewButtonBigDesktopPlus
        ]


viewBodyPhoneLandscapeLoading : Element Msg
viewBodyPhoneLandscapeLoading =
    viewBodyPhoneLandscapeWith
        [ viewButtonPhoneMinusOff
        , viewLoadingPhone
        , viewButtonPhonePlusOff
        ]


viewBodyTabletLandscapeLoading : Element Msg
viewBodyTabletLandscapeLoading =
    viewBodyTabletLandscapeWith
        [ viewButtonTabletMinusOff
        , viewLoadingTablet
        , viewButtonTabletPlusOff
        ]


viewBodyDesktopLandscapeLoading : Element Msg
viewBodyDesktopLandscapeLoading =
    viewBodyDesktopLandscapeWith
        [ viewButtonDesktopMinusOff
        , viewLoadingDesktop
        , viewButtonDesktopPlusOff
        ]


viewBodyBigDesktopLandscapeLoading : Element Msg
viewBodyBigDesktopLandscapeLoading =
    viewBodyBigDesktopLandscapeWith
        [ viewButtonBigDesktopMinusOff
        , viewLoadingBigDesktop
        , viewButtonBigDesktopPlusOff
        ]


viewBodyPhoneLandscapeWith : List (Element Msg) -> Element Msg
viewBodyPhoneLandscapeWith =
    viewBodyLandscape 48


viewBodyTabletLandscapeWith : List (Element Msg) -> Element Msg
viewBodyTabletLandscapeWith =
    viewBodyLandscape 72


viewBodyDesktopLandscapeWith : List (Element Msg) -> Element Msg
viewBodyDesktopLandscapeWith =
    viewBodyLandscape 96


viewBodyBigDesktopLandscapeWith : List (Element Msg) -> Element Msg
viewBodyBigDesktopLandscapeWith =
    viewBodyLandscape 120


viewBodyLandscape : Int -> List (Element Msg) -> Element Msg
viewBodyLandscape distance =
    Element.row <| viewBodyAttributes distance


viewBodyAttributes : Int -> List (Attribute Msg)
viewBodyAttributes distance =
    List.concat
        [ fillWidthAttributes
        , alignCenterAttributes
        , paddingAndSpacing distance
        ]


type alias BodyStates =
    { zero : Element Msg
    , maxInteger : Element Msg
    , complete : UnsignedInteger -> Element Msg
    , loading : Element Msg
    }


viewBodyWith : BodyStates -> Maybe UnsignedInteger -> Element Msg
viewBodyWith { zero, maxInteger, complete, loading } counter =
    case counter of
        Just unsignedInteger ->
            if unsignedInteger == Data.unsignedIntegerZero then
                zero

            else if unsignedInteger == Data.unsignedIntegerMaxInteger then
                maxInteger

            else
                complete unsignedInteger

        Nothing ->
            loading



-- BODY COUNTER


viewCounterPhone : UnsignedInteger -> Element Msg
viewCounterPhone =
    viewCounter counterSizeDetailsPhone


viewCounterTablet : UnsignedInteger -> Element Msg
viewCounterTablet =
    viewCounter counterSizeDetailsTablet


viewCounterDesktop : UnsignedInteger -> Element Msg
viewCounterDesktop =
    viewCounter counterSizeDetailsDesktop


viewCounterBigDesktop : UnsignedInteger -> Element Msg
viewCounterBigDesktop =
    viewCounter counterSizeDetailsBigDesktop


viewCounter : SizeDetails -> UnsignedInteger -> Element Msg
viewCounter counterSizeDetails unsignedInteger =
    helper Element.el
        [ viewAttributes counterSizeDetails ]
    <|
        Data.fromUnsignedIntegerToText unsignedInteger



-- BODY LOADING


viewLoadingPhone : Element Msg
viewLoadingPhone =
    viewLoading counterSizeDetailsPhone


viewLoadingTablet : Element Msg
viewLoadingTablet =
    viewLoading counterSizeDetailsTablet


viewLoadingDesktop : Element Msg
viewLoadingDesktop =
    viewLoading counterSizeDetailsDesktop


viewLoadingBigDesktop : Element Msg
viewLoadingBigDesktop =
    viewLoading counterSizeDetailsBigDesktop


viewLoading : SizeDetails -> Element Msg
viewLoading counterSizeDetails =
    helper Element.el
        [ viewAttributes counterSizeDetails ]
    <|
        Element.text "LOADING"


viewAttributes : SizeDetails -> List (Attribute Msg)
viewAttributes { padding, fontSize } =
    List.concat
        [ shrinkWidthAndHeightAttributes
        , alignCenterAttributes
        , fontSherpaBlueAttributes fontSize
        , [ Element.padding padding ]
        ]



-- BODY BUTTON ON


viewButtonPhoneIncrement : Element Msg
viewButtonPhoneIncrement =
    viewButtonPhoneOn SendIncrement "INCREMENT"


viewButtonTabletIncrement : Element Msg
viewButtonTabletIncrement =
    viewButtonTabletOn SendIncrement "INCREMENT"


viewButtonDesktopIncrement : Element Msg
viewButtonDesktopIncrement =
    viewButtonDesktopOn SendIncrement "INCREMENT"


viewButtonBigDesktopIncrement : Element Msg
viewButtonBigDesktopIncrement =
    viewButtonBigDesktopOn SendIncrement "INCREMENT"


viewButtonPhoneDecrement : Element Msg
viewButtonPhoneDecrement =
    viewButtonPhoneOn SendDecrement "DECREMENT"


viewButtonTabletDecrement : Element Msg
viewButtonTabletDecrement =
    viewButtonTabletOn SendDecrement "DECREMENT"


viewButtonDesktopDecrement : Element Msg
viewButtonDesktopDecrement =
    viewButtonDesktopOn SendDecrement "DECREMENT"


viewButtonBigDesktopDecrement : Element Msg
viewButtonBigDesktopDecrement =
    viewButtonBigDesktopOn SendDecrement "DECREMENT"


viewButtonPhoneOn : Msg -> String -> Element Msg
viewButtonPhoneOn =
    viewButtonOn buttonSizeDetailsPhone


viewButtonTabletOn : Msg -> String -> Element Msg
viewButtonTabletOn =
    viewButtonOn buttonSizeDetailsTablet


viewButtonDesktopOn : Msg -> String -> Element Msg
viewButtonDesktopOn =
    viewButtonOn buttonSizeDetailsDesktop


viewButtonBigDesktopOn : Msg -> String -> Element Msg
viewButtonBigDesktopOn =
    viewButtonOn buttonSizeDetailsBigDesktop



-- HEADER BUTTON ON SMALL


viewButtonPhonePlus : Element Msg
viewButtonPhonePlus =
    viewButtonSmallPhoneOn SendIncrement "+"


viewButtonTabletPlus : Element Msg
viewButtonTabletPlus =
    viewButtonSmallTabletOn SendIncrement "+"


viewButtonDesktopPlus : Element Msg
viewButtonDesktopPlus =
    viewButtonSmallDesktopOn SendIncrement "+"


viewButtonBigDesktopPlus : Element Msg
viewButtonBigDesktopPlus =
    viewButtonSmallBigDesktopOn SendIncrement "+"


viewButtonPhoneMinus : Element Msg
viewButtonPhoneMinus =
    viewButtonSmallPhoneOn SendDecrement "-"


viewButtonTabletMinus : Element Msg
viewButtonTabletMinus =
    viewButtonSmallTabletOn SendDecrement "-"


viewButtonDesktopMinus : Element Msg
viewButtonDesktopMinus =
    viewButtonSmallDesktopOn SendDecrement "-"


viewButtonBigDesktopMinus : Element Msg
viewButtonBigDesktopMinus =
    viewButtonSmallBigDesktopOn SendDecrement "-"


viewButtonSmallPhoneOn : Msg -> String -> Element Msg
viewButtonSmallPhoneOn =
    viewButtonOn buttonSmallSizeDetailsPhone


viewButtonSmallTabletOn : Msg -> String -> Element Msg
viewButtonSmallTabletOn =
    viewButtonOn buttonSmallSizeDetailsTablet


viewButtonSmallDesktopOn : Msg -> String -> Element Msg
viewButtonSmallDesktopOn =
    viewButtonOn buttonSmallSizeDetailsDesktop


viewButtonSmallBigDesktopOn : Msg -> String -> Element Msg
viewButtonSmallBigDesktopOn =
    viewButtonOn buttonSmallSizeDetailsBigDesktop


viewButtonOn : ButtonSizeDetails -> Msg -> String -> Element Msg
viewButtonOn buttonSizeDetails msg label =
    helper Input.button
        [ viewButtonOnAttributes buttonSizeDetails ]
        { onPress = Just msg
        , label = Element.text label
        }


viewButtonOnAttributes : ButtonSizeDetails -> List (Attribute Msg)
viewButtonOnAttributes =
    viewButtonAttributes sherpaBlue



-- BODY BUTTON OFF


viewButtonPhoneIncrementOff : Element Msg
viewButtonPhoneIncrementOff =
    viewButtonPhoneOff "INCREMENT"


viewButtonTabletIncrementOff : Element Msg
viewButtonTabletIncrementOff =
    viewButtonTabletOff "INCREMENT"


viewButtonDesktopIncrementOff : Element Msg
viewButtonDesktopIncrementOff =
    viewButtonDesktopOff "INCREMENT"


viewButtonBigDesktopIncrementOff : Element Msg
viewButtonBigDesktopIncrementOff =
    viewButtonBigDesktopOff "INCREMENT"


viewButtonPhoneDecrementOff : Element Msg
viewButtonPhoneDecrementOff =
    viewButtonPhoneOff "DECREMENT"


viewButtonTabletDecrementOff : Element Msg
viewButtonTabletDecrementOff =
    viewButtonTabletOff "DECREMENT"


viewButtonDesktopDecrementOff : Element Msg
viewButtonDesktopDecrementOff =
    viewButtonDesktopOff "DECREMENT"


viewButtonBigDesktopDecrementOff : Element Msg
viewButtonBigDesktopDecrementOff =
    viewButtonBigDesktopOff "DECREMENT"


viewButtonPhoneOff : String -> Element Msg
viewButtonPhoneOff =
    viewButtonOff buttonSizeDetailsPhone


viewButtonTabletOff : String -> Element Msg
viewButtonTabletOff =
    viewButtonOff buttonSizeDetailsTablet


viewButtonDesktopOff : String -> Element Msg
viewButtonDesktopOff =
    viewButtonOff buttonSizeDetailsDesktop


viewButtonBigDesktopOff : String -> Element Msg
viewButtonBigDesktopOff =
    viewButtonOff buttonSizeDetailsBigDesktop



-- BODY BUTTON OFF SMALL


viewButtonPhonePlusOff : Element Msg
viewButtonPhonePlusOff =
    viewButtonSmallPhoneOff "+"


viewButtonTabletPlusOff : Element Msg
viewButtonTabletPlusOff =
    viewButtonSmallTabletOff "+"


viewButtonDesktopPlusOff : Element Msg
viewButtonDesktopPlusOff =
    viewButtonSmallDesktopOff "+"


viewButtonBigDesktopPlusOff : Element Msg
viewButtonBigDesktopPlusOff =
    viewButtonSmallBigDesktopOff "+"


viewButtonPhoneMinusOff : Element Msg
viewButtonPhoneMinusOff =
    viewButtonSmallPhoneOff "-"


viewButtonTabletMinusOff : Element Msg
viewButtonTabletMinusOff =
    viewButtonSmallTabletOff "-"


viewButtonDesktopMinusOff : Element Msg
viewButtonDesktopMinusOff =
    viewButtonSmallDesktopOff "-"


viewButtonBigDesktopMinusOff : Element Msg
viewButtonBigDesktopMinusOff =
    viewButtonSmallBigDesktopOff "-"


viewButtonSmallPhoneOff : String -> Element Msg
viewButtonSmallPhoneOff =
    viewButtonOff buttonSmallSizeDetailsPhone


viewButtonSmallTabletOff : String -> Element Msg
viewButtonSmallTabletOff =
    viewButtonOff buttonSmallSizeDetailsTablet


viewButtonSmallDesktopOff : String -> Element Msg
viewButtonSmallDesktopOff =
    viewButtonOff buttonSmallSizeDetailsDesktop


viewButtonSmallBigDesktopOff : String -> Element Msg
viewButtonSmallBigDesktopOff =
    viewButtonOff buttonSmallSizeDetailsBigDesktop


viewButtonOff : ButtonSizeDetails -> String -> Element Msg
viewButtonOff buttonSizeDetails label =
    helper Input.button
        [ viewButtonOffAttributes buttonSizeDetails ]
        { onPress = Nothing
        , label = Element.text label
        }


viewButtonOffAttributes : ButtonSizeDetails -> List (Attribute Msg)
viewButtonOffAttributes =
    viewButtonAttributes gray


viewButtonAttributes : Color -> ButtonSizeDetails -> List (Attribute Msg)
viewButtonAttributes color { width, padding, fontSize } =
    List.concat
        [ shrinkButtonAttributes width
        , alignCenterAttributes
        , fontWhiteAttributes fontSize
        , [ Element.padding padding
          , Background.color color
          ]
        ]



-- BODY NOT CONNECTED


viewBodyPhonePortraitNotConnected : Element Msg
viewBodyPhonePortraitNotConnected =
    viewMainPhonePortrait
        [ viewMetamaskLogoPhone
        , viewNotConnectedParagraphPhone
        , viewMetamaskConnectPhone
        ]


viewBodyTabletPortraitNotConnected : Element Msg
viewBodyTabletPortraitNotConnected =
    viewMainTabletPortrait
        [ viewMetamaskLogoTablet
        , viewNotConnectedParagraphTablet
        , viewMetamaskConnectTablet
        ]


viewBodyDesktopPortraitNotConnected : Element Msg
viewBodyDesktopPortraitNotConnected =
    viewMainDesktopPortrait
        [ viewMetamaskLogoDesktop
        , viewNotConnectedParagraphDesktop
        , viewMetamaskConnectDesktop
        ]


viewBodyBigDesktopPortraitNotConnected : Element Msg
viewBodyBigDesktopPortraitNotConnected =
    viewMainBigDesktopPortrait
        [ viewMetamaskLogoBigDesktop
        , viewNotConnectedParagraphBigDesktop
        , viewMetamaskConnectBigDesktop
        ]


viewBodyPhoneLandscapeNotConnected : Element Msg
viewBodyPhoneLandscapeNotConnected =
    viewMainPhoneLandscape viewMetamaskLogoPhone
        [ viewNotConnectedParagraphPhone
        , viewMetamaskConnectPhone
        ]


viewBodyTabletLandscapeNotConnected : Element Msg
viewBodyTabletLandscapeNotConnected =
    viewMainTabletLandscape viewMetamaskLogoTablet
        [ viewNotConnectedParagraphTablet
        , viewMetamaskConnectTablet
        ]


viewBodyDesktopLandscapeNotConnected : Element Msg
viewBodyDesktopLandscapeNotConnected =
    viewMainDesktopLandscape viewMetamaskLogoDesktop
        [ viewNotConnectedParagraphDesktop
        , viewMetamaskConnectDesktop
        ]


viewBodyBigDesktopLandscapeNotConnected : Element Msg
viewBodyBigDesktopLandscapeNotConnected =
    viewMainBigDesktopLandscape viewMetamaskLogoBigDesktop
        [ viewNotConnectedParagraphBigDesktop
        , viewMetamaskConnectBigDesktop
        ]



-- BODY NO METAMASK


viewBodyPhonePortraitNoMetamask : Element Msg
viewBodyPhonePortraitNoMetamask =
    viewMainPhonePortrait
        [ viewMetamaskLogoPhone
        , viewNoMetamaskParagraphPhone
        , viewMetamaskButtonPhone
        ]


viewBodyTabletPortraitNoMetamask : Element Msg
viewBodyTabletPortraitNoMetamask =
    viewMainTabletPortrait
        [ viewMetamaskLogoTablet
        , viewNoMetamaskParagraphTablet
        , viewMetamaskButtonTablet
        ]


viewBodyDesktopPortraitNoMetamask : Element Msg
viewBodyDesktopPortraitNoMetamask =
    viewMainDesktopPortrait
        [ viewMetamaskLogoDesktop
        , viewNoMetamaskParagraphDesktop
        , viewMetamaskButtonDesktop
        ]


viewBodyBigDesktopPortraitNoMetamask : Element Msg
viewBodyBigDesktopPortraitNoMetamask =
    viewMainBigDesktopPortrait
        [ viewMetamaskLogoBigDesktop
        , viewNoMetamaskParagraphBigDesktop
        , viewMetamaskButtonBigDesktop
        ]


viewMainPhonePortrait : List (Element Msg) -> Element Msg
viewMainPhonePortrait =
    viewMainPortrait 24


viewMainTabletPortrait : List (Element Msg) -> Element Msg
viewMainTabletPortrait =
    viewMainPortrait 48


viewMainDesktopPortrait : List (Element Msg) -> Element Msg
viewMainDesktopPortrait =
    viewMainPortrait 72


viewMainBigDesktopPortrait : List (Element Msg) -> Element Msg
viewMainBigDesktopPortrait =
    viewMainPortrait 96


viewMainPortrait : Int -> List (Element Msg) -> Element Msg
viewMainPortrait distance =
    Element.column <| viewMainAttributes distance


viewBodyPhoneLandscapeNoMetamask : Element Msg
viewBodyPhoneLandscapeNoMetamask =
    viewMainPhoneLandscape viewMetamaskLogoPhone
        [ viewNoMetamaskParagraphPhone
        , viewMetamaskButtonPhone
        ]


viewBodyTabletLandscapeNoMetamask : Element Msg
viewBodyTabletLandscapeNoMetamask =
    viewMainTabletLandscape viewMetamaskLogoTablet
        [ viewNoMetamaskParagraphTablet
        , viewMetamaskButtonTablet
        ]


viewBodyDesktopLandscapeNoMetamask : Element Msg
viewBodyDesktopLandscapeNoMetamask =
    viewMainDesktopLandscape viewMetamaskLogoDesktop
        [ viewNoMetamaskParagraphDesktop
        , viewMetamaskButtonDesktop
        ]


viewBodyBigDesktopLandscapeNoMetamask : Element Msg
viewBodyBigDesktopLandscapeNoMetamask =
    viewMainBigDesktopLandscape viewMetamaskLogoBigDesktop
        [ viewNoMetamaskParagraphBigDesktop
        , viewMetamaskButtonBigDesktop
        ]



-- BODY MAIN


viewMainPhoneLandscape : Element Msg -> List (Element Msg) -> Element Msg
viewMainPhoneLandscape =
    viewMainLandscape 24


viewMainTabletLandscape : Element Msg -> List (Element Msg) -> Element Msg
viewMainTabletLandscape =
    viewMainLandscape 48


viewMainDesktopLandscape : Element Msg -> List (Element Msg) -> Element Msg
viewMainDesktopLandscape =
    viewMainLandscape 72


viewMainBigDesktopLandscape : Element Msg -> List (Element Msg) -> Element Msg
viewMainBigDesktopLandscape =
    viewMainLandscape 96


viewMainLandscape : Int -> Element Msg -> List (Element Msg) -> Element Msg
viewMainLandscape distance logo list =
    helper Element.row
        [ viewMainAttributes distance ]
        [ logo
        , viewMainLandscapeNext distance list
        ]


viewMainLandscapeNext : Int -> List (Element Msg) -> Element Msg
viewMainLandscapeNext distance =
    Element.column <| viewMainAttributes distance


viewMainAttributes : Int -> List (Attribute Msg)
viewMainAttributes distance =
    List.concat
        [ fillWidthAttributes
        , alignCenterAttributes
        , [ Element.paddingXY distance 0
          , Element.spacing distance
          ]
        ]



-- BODY NOT CONNECTED PARAGRAPH


viewNotConnectedParagraphPhone : Element Msg
viewNotConnectedParagraphPhone =
    viewNotConnectedParagraph 36


viewNotConnectedParagraphTablet : Element Msg
viewNotConnectedParagraphTablet =
    viewNotConnectedParagraph 48


viewNotConnectedParagraphDesktop : Element Msg
viewNotConnectedParagraphDesktop =
    viewNotConnectedParagraph 60


viewNotConnectedParagraphBigDesktop : Element Msg
viewNotConnectedParagraphBigDesktop =
    viewNotConnectedParagraph 72


viewNotConnectedParagraph : Int -> Element Msg
viewNotConnectedParagraph =
    viewMainParagraph "Connect Metamask on Kovan Test Network to Login"



-- BODY NO METAMASK PARAGRAPH


viewNoMetamaskParagraphPhone : Element Msg
viewNoMetamaskParagraphPhone =
    viewNoMetamaskParagraph 36


viewNoMetamaskParagraphTablet : Element Msg
viewNoMetamaskParagraphTablet =
    viewNoMetamaskParagraph 48


viewNoMetamaskParagraphDesktop : Element Msg
viewNoMetamaskParagraphDesktop =
    viewNoMetamaskParagraph 60


viewNoMetamaskParagraphBigDesktop : Element Msg
viewNoMetamaskParagraphBigDesktop =
    viewNoMetamaskParagraph 72


viewNoMetamaskParagraph : Int -> Element Msg
viewNoMetamaskParagraph =
    viewMainParagraph "Metamask Extension or Metamask Browser is Required"



-- BODY MAIN PARAGRAPH


viewMainParagraph : String -> Int -> Element Msg
viewMainParagraph text fontSize =
    helper Element.paragraph
        [ shrinkWidthAndHeightAttributes
        , alignCenterAttributes
        , fontSherpaBlueAttributes fontSize
        ]
        [ Element.text text ]



-- BODY METAMASK LOGO


viewMetamaskLogoPhone : Element Msg
viewMetamaskLogoPhone =
    viewMetamaskLogo 240


viewMetamaskLogoTablet : Element Msg
viewMetamaskLogoTablet =
    viewMetamaskLogo 360


viewMetamaskLogoDesktop : Element Msg
viewMetamaskLogoDesktop =
    viewMetamaskLogo 480


viewMetamaskLogoBigDesktop : Element Msg
viewMetamaskLogoBigDesktop =
    viewMetamaskLogo 600


viewMetamaskLogo : Int -> Element Msg
viewMetamaskLogo length =
    helper Image.toElement
        [ alignCenterAttributes
        , [ Element.width <| Element.px length
          , Element.height <| Element.px length
          ]
        ]
        Image.metamaskLogo



-- BODY METAMASK CONNECT


viewMetamaskConnectPhone : Element Msg
viewMetamaskConnectPhone =
    viewMetamaskConnect buttonSizeDetailsPhone


viewMetamaskConnectTablet : Element Msg
viewMetamaskConnectTablet =
    viewMetamaskConnect buttonSizeDetailsTablet


viewMetamaskConnectDesktop : Element Msg
viewMetamaskConnectDesktop =
    viewMetamaskConnect buttonSizeDetailsDesktop


viewMetamaskConnectBigDesktop : Element Msg
viewMetamaskConnectBigDesktop =
    viewMetamaskConnect buttonSizeDetailsBigDesktop


viewMetamaskConnect : ButtonSizeDetails -> Element Msg
viewMetamaskConnect buttonSizeDetails =
    viewButtonOn buttonSizeDetails SendConnect "Connect Metamask"



-- BODY METAMASK LINK BUTTON


viewMetamaskButtonPhone : Element Msg
viewMetamaskButtonPhone =
    viewMetamaskButton buttonSizeDetailsPhone


viewMetamaskButtonTablet : Element Msg
viewMetamaskButtonTablet =
    viewMetamaskButton buttonSizeDetailsTablet


viewMetamaskButtonDesktop : Element Msg
viewMetamaskButtonDesktop =
    viewMetamaskButton buttonSizeDetailsDesktop


viewMetamaskButtonBigDesktop : Element Msg
viewMetamaskButtonBigDesktop =
    viewMetamaskButton buttonSizeDetailsBigDesktop


viewMetamaskButton : ButtonSizeDetails -> Element Msg
viewMetamaskButton buttonSizeDetails =
    helper Link.newTab
        [ viewButtonOnAttributes buttonSizeDetails ]
        { link = Link.metamaskWebsite
        , label = Element.text "Visit Metamask"
        }



-- WIDTH AND HEIGHT


fillWidthAndHeightAttributes : List (Attribute Msg)
fillWidthAndHeightAttributes =
    [ Element.width Element.fill
    , Element.height Element.fill
    ]


fillWidthAttributes : List (Attribute Msg)
fillWidthAttributes =
    [ Element.width Element.fill
    , Element.height Element.shrink
    ]


shrinkWidthAndHeightAttributes : List (Attribute Msg)
shrinkWidthAndHeightAttributes =
    [ Element.width Element.shrink
    , Element.height Element.shrink
    ]


shrinkButtonAttributes : Int -> List (Attribute Msg)
shrinkButtonAttributes width =
    [ Element.width <| Element.px width
    , Element.height Element.shrink
    ]



-- PADDING AND SPACING


paddingAndSpacing : Int -> List (Attribute Msg)
paddingAndSpacing distance =
    [ Element.padding distance
    , Element.spacing distance
    ]



-- ALIGNMENT


alignCenterAttributes : List (Attribute Msg)
alignCenterAttributes =
    [ Element.centerX
    , Element.centerY
    ]



-- FONT


fontSherpaBlueAttributes : Int -> List (Attribute Msg)
fontSherpaBlueAttributes =
    fontAttributes sherpaBlue


fontWhiteAttributes : Int -> List (Attribute Msg)
fontWhiteAttributes =
    fontAttributes white


fontAttributes : Color -> Int -> List (Attribute Msg)
fontAttributes color fontSize =
    [ Font.size fontSize
    , Font.center
    , Font.heavy
    , Font.color color
    , roboto
    ]


roboto : Attribute Msg
roboto =
    Font.family
        [ Font.typeface "Roboto"
        , Font.sansSerif
        ]



-- ELEMENT HELPER


helper : (List (Attribute Msg) -> children -> any) -> List (List (Attribute Msg)) -> children -> any
helper element listAttributes =
    element <| List.concat listAttributes



-- SIZE DETAILS


type alias SizeDetails =
    { padding : Int
    , fontSize : Int
    }


xDetailsPhone : SizeDetails
xDetailsPhone =
    { padding = 12
    , fontSize = 30
    }


xDetailsTablet : SizeDetails
xDetailsTablet =
    { padding = 15
    , fontSize = 36
    }


xDetailsDesktop : SizeDetails
xDetailsDesktop =
    { padding = 18
    , fontSize = 42
    }


xDetailsBigDesktop : SizeDetails
xDetailsBigDesktop =
    { padding = 21
    , fontSize = 48
    }


errorDetailsPhone : SizeDetails
errorDetailsPhone =
    { padding = 18
    , fontSize = 18
    }


errorDetailsTablet : SizeDetails
errorDetailsTablet =
    { padding = 24
    , fontSize = 24
    }


errorDetailsDesktop : SizeDetails
errorDetailsDesktop =
    { padding = 30
    , fontSize = 30
    }


errorDetailsBigDesktop : SizeDetails
errorDetailsBigDesktop =
    { padding = 36
    , fontSize = 36
    }


counterSizeDetailsPhone : SizeDetails
counterSizeDetailsPhone =
    { padding = 12
    , fontSize = 60
    }


counterSizeDetailsTablet : SizeDetails
counterSizeDetailsTablet =
    { padding = 18
    , fontSize = 96
    }


counterSizeDetailsDesktop : SizeDetails
counterSizeDetailsDesktop =
    { padding = 24
    , fontSize = 132
    }


counterSizeDetailsBigDesktop : SizeDetails
counterSizeDetailsBigDesktop =
    { padding = 30
    , fontSize = 168
    }



-- BUTTON SIZE DETAILS


type alias ButtonSizeDetails =
    { width : Int
    , padding : Int
    , fontSize : Int
    }


buttonSizeDetailsPhone : ButtonSizeDetails
buttonSizeDetailsPhone =
    { width = 240
    , padding = 9
    , fontSize = 18
    }


buttonSizeDetailsTablet : ButtonSizeDetails
buttonSizeDetailsTablet =
    { width = 300
    , padding = 12
    , fontSize = 24
    }


buttonSizeDetailsDesktop : ButtonSizeDetails
buttonSizeDetailsDesktop =
    { width = 360
    , padding = 15
    , fontSize = 30
    }


buttonSizeDetailsBigDesktop : ButtonSizeDetails
buttonSizeDetailsBigDesktop =
    { width = 420
    , padding = 18
    , fontSize = 36
    }


buttonSmallSizeDetailsPhone : ButtonSizeDetails
buttonSmallSizeDetailsPhone =
    { width = 90
    , padding = 16
    , fontSize = 48
    }


buttonSmallSizeDetailsTablet : ButtonSizeDetails
buttonSmallSizeDetailsTablet =
    { width = 120
    , padding = 20
    , fontSize = 60
    }


buttonSmallSizeDetailsDesktop : ButtonSizeDetails
buttonSmallSizeDetailsDesktop =
    { width = 150
    , padding = 24
    , fontSize = 72
    }


buttonSmallSizeDetailsBigDesktop : ButtonSizeDetails
buttonSmallSizeDetailsBigDesktop =
    { width = 180
    , padding = 28
    , fontSize = 84
    }



-- COLOR


white : Color
white =
    Element.rgba255 255 255 255 1


gray : Color
gray =
    Element.rgba255 128 128 128 0.3


sherpaBlue : Color
sherpaBlue =
    Element.rgba255 1 50 67 1


transparentBlack : Color
transparentBlack =
    Element.rgba255 0 0 0 0.5

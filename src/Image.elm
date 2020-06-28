module Image exposing (Image, createImage, metamaskLogo, toElement)

import Element exposing (Attribute, Element)


type Image
    = Image { src : String, description : String }


toElement : List (Attribute msg) -> Image -> Element msg
toElement attributes (Image { src, description }) =
    Element.image attributes
        { src = src
        , description = description
        }


metamaskLogo : Image
metamaskLogo =
    createImage "MetamaskLogo.svg" "Metamask Logo"


createImage : String -> String -> Image
createImage filename description =
    Image <| { src = "./asset/image/" ++ filename, description = description }

module View exposing (View, map, placeholder)

import Element exposing (Element)


type alias View msg =
    { title : String
    , body : Element msg
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title =
        if String.isEmpty doc.title then
            baseTitle

        else
            baseTitle ++ " - " ++ doc.title
    , body = Element.map fn doc.body
    }


baseTitle : String
baseTitle =
    "Elm Packages"


placeholder : String -> View msg
placeholder moduleName =
    { title = "Placeholder - " ++ moduleName
    , body = Element.text moduleName
    }

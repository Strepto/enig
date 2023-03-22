module ElmUi.Keyboard exposing (..)

import Element
import Html.Events
import Json.Decode as Decode


onEnterUp : msg -> Element.Attribute msg
onEnterUp msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


onKeyUp : String -> msg -> Element.Attribute msg
onKeyUp keyCode msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == keyCode then
                            Decode.succeed msg

                        else
                            Decode.fail <| "Not the " ++ keyCode ++ " key"
                    )
            )
        )

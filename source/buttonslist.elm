module Buttonslist exposing (view, Msg, update)

import Buttons as Imported
import Html exposing (beginnerProgram, div, button, text, pre)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Array


defaultElementIfNothing =
    0


main =
    beginnerProgram
        { model = []
        , view = view
        , update = update
        }


view model =
    div [ style [ ( "margin", "10px" ) ] ]
        [ button [ onClick (Add) ] [ text "Add" ]
        , div []
            (List.indexedMap
                (\index element ->
                    div
                        [ style
                            [ ( "border", "1px solid #aaa" )
                            , ( "background-color", "#eee" )
                            , ( "display", "inline-block" )
                            , ( "padding", "10px" )
                            , ( "margin", "10px" )
                            ]
                        ]
                        [ Html.map
                            (MsgFromChild index)
                            (div
                                []
                                [ Imported.view element ]
                            )
                        , button [ onClick (Remove index) ] [ text "Remove" ]
                        ]
                )
                model
            )
        , pre [] [ text ("model: " ++ (toString model)) ]
        ]


type Msg
    = MsgFromChild Int Imported.Msg
    | Remove Int
    | Add


getElementFromList list index default =
    list
        |> Array.fromList
        |> Array.get index
        |> replaceMaybeWithDefault default


replaceMaybeWithDefault default element =
    case element of
        Nothing ->
            default

        Just val ->
            val


setElementIntoList list index element =
    list
        |> Array.fromList
        |> Array.set index element
        |> Array.toList


removeElementFromList index list =
    (List.take index list) ++ (List.drop (index + 1) list)


update msg model =
    case msg of
        MsgFromChild index msgFromChild ->
            let
                oldElement =
                    getElementFromList model index defaultElementIfNothing

                newElement =
                    Imported.update msgFromChild oldElement
            in
                setElementIntoList model index newElement

        Remove index ->
            removeElementFromList index model

        Add ->
            List.append model [ defaultElementIfNothing ]

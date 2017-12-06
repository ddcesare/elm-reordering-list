module Components.SortableList exposing (..)

{-| An example of a sortable list using drag and drop
See the README.md file for more information
-}

import Components.Styles as Styles
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Mouse exposing (Position)


-- MODEL


type alias Model =
    { data : List String
    , drag : Maybe Drag
    }


type alias Drag =
    { itemIndex : Int
    , startY : Int
    , currentY : Int
    }



-- UPDATE


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart idx pos ->
            { model
                | drag = Just <| Drag idx pos.y pos.y
            }
                ! []

        DragAt pos ->
            { model
                | drag =
                    Maybe.map (\{ itemIndex, startY } -> Drag itemIndex startY pos.y) model.drag
            }
                ! []

        DragEnd pos ->
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    { model
                        | data =
                            moveItem
                                itemIndex
                                ((currentY
                                    - startY
                                    + (if currentY < startY then
                                        -20
                                       else
                                        20
                                      )
                                 )
                                    // 50
                                )
                                model.data
                        , drag = Nothing
                    }
                        ! []

                Nothing ->
                    { model
                        | drag = Nothing
                    }
                        ! []


moveItem : Int -> Int -> List a -> List a
moveItem fromPos offset list =
    let
        listWithoutMoved =
            List.take fromPos list ++ List.drop (fromPos + 1) list

        moved =
            List.take 1 <| List.drop fromPos list
    in
    List.take (fromPos + offset) listWithoutMoved
        ++ moved
        ++ List.drop (fromPos + offset) listWithoutMoved



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style Styles.pageContainer ]
        [ div
            [ style Styles.listHeader ]
            [ h3
                [ style Styles.headerTitle ]
                [ text "Policies Pipeline" ]
            ]
        , ul
            [ style Styles.listContainer ]
          <|
            List.indexedMap (itemView model) model.data
        ]


itemView : Model -> Int -> String -> Html Msg
itemView model idx item =
    let
        buttonStyle =
            [ ( "display", "inline-block" ) ]

        moveStyle =
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    if itemIndex == idx then
                        [ ( "transform", "translateY( " ++ toString (currentY - startY) ++ "px) translateZ(10px)" )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "willChange", "transform" )
                        ]
                    else
                        []

                Nothing ->
                    []

        makingWayStyle =
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    if (idx < itemIndex) && (currentY - startY) < (idx - itemIndex) * 50 + 20 then
                        [ ( "transform", "translateY(50px)" )
                        , ( "transition", "transform 200ms ease-in-out" )
                        ]
                    else if (idx > itemIndex) && (currentY - startY) > (idx - itemIndex) * 50 - 20 then
                        [ ( "transform", "translateY(-50px)" )
                        , ( "transition", "transform 200ms ease-in-out" )
                        ]
                    else if idx /= itemIndex then
                        [ ( "transition", "transform 200ms ease-in-out" ) ]
                    else
                        []

                Nothing ->
                    []
    in
    li [ style <| Styles.listItem ++ moveStyle ++ makingWayStyle ]
        [ div [ style Styles.itemText ] [ text item ]
        , button
            [ style buttonStyle, onMouseDown <| DragStart idx ]
            [ text "drag" ]
        ]


onMouseDown : (Position -> msg) -> Attribute msg
onMouseDown msg =
    on "mousedown" (Decode.map msg Mouse.position)

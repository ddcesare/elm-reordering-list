module Main exposing (..)

import Components.SortableList as SortableList exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { sortableList : SortableList.Model
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! []


initialModel : Model
initialModel =
    { sortableList =
        { data = initialList |> List.sort
        , drag = Nothing
        }
    }


policies =
    [ { id = "cors-policy"
      , name = "CORS Policy"
      , description = "CORS regulates access resource requests from outside of an originating domain. Configuration required."
      , order = 0
      }
    , { id = "blah-policy"
      , name = "BLAH Policy"
      , description = "BLAH regulates access resource requests from outside of an originating domain. Configuration required."
      , order = 1
      }
    , { id = "foo-policy"
      , name = "FOO Policy"
      , description = "FOO regulates access resource requests from outside of an originating domain. Configuration required."
      , order = 2
      }
    ]


initialList =
    [ "BLAH Policy"
    , "FOO Policy"
    , "CORS Policy"
    ]


subscriptions model =
    let
        sortableListSub =
            SortableList.subscriptions model.sortableList
    in
    Sub.batch [ Sub.map SortableListMsg sortableListSub ]



-- UPDATE


type Msg
    = NoOp
    | SortableListMsg SortableList.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        logMsg message =
            Debug.log "Message: " message
    in
    case logMsg msg of
        NoOp ->
            ( model, Cmd.none )

        SortableListMsg sortableListMsg ->
            let
                ( sortableListModel, sortableListCmd ) =
                    SortableList.update sortableListMsg model.sortableList
            in
            ( { model | sortableList = sortableListModel }
            , Cmd.map SortableListMsg sortableListCmd
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map SortableListMsg (SortableList.view model.sortableList)
        ]

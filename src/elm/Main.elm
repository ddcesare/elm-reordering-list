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


type Policy
    = Policy
        { name : String
        , version : String
        , configuration : String
        , description : String
        }


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


policies : List Policy
policies =
    [ Policy
        { name = "apicast.policy.cors"
        , version = "1.2.3"
        , configuration = "{ methods = [ 'GET' ] }"
        , description = "CORS regulates access resource requests from outside of an originating domain. Configuration required."
        }
    , Policy
        { name = "BLAH Policy"
        , version = ""
        , configuration = ""
        , description = "BLAH regulates access resource requests from outside of an originating domain. Configuration required."
        }
    , Policy
        { name = "FOO Policy"
        , version = ""
        , configuration = ""
        , description = "FOO regulates access resource requests from outside of an originating domain. Configuration required."
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

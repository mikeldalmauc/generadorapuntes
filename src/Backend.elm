module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)

type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { feedbacks = [] }
    , Cmd.none )

update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
     
        AddFeedback feedbackEntry ->
            let
                updatedModel =
                    { model | feedbacks = feedbackEntry :: model.feedbacks }
            in
            ( updatedModel
                , Lamdera.sendToFrontend clientId (UpdateFeedbacks updatedModel.feedbacks)
            )

        RequestFeedbacks ->
            ( model
            , Lamdera.sendToFrontend clientId (UpdateFeedbacks model.feedbacks)
            )
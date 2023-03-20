module Backend exposing (..)

import Cmd.Extra exposing (withCmd, withNoCmd)
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
    ( { message = "Hello!"
      , votes = []
      }
    , Cmd.none
    )


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

        StartedNewRoundToBackend ->
            { model | votes = [] } |> withCmd (Lamdera.broadcast (VotesUpdated []))

        ChangedVoteToBackend vote ->
            let
                newVotes =
                    { vote = vote, client = clientId } :: model.votes |> List.take 100
            in
            { model | votes = newVotes } |> withCmd (Lamdera.broadcast (VotesUpdated (newVotes |> List.map .vote)))

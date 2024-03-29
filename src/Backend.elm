module Backend exposing (..)

import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Dict
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Maybe
import Random
import Set
import Types exposing (..)
import Utils


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , rooms = Dict.empty
      , clientRooms = Dict.empty
      , randomNext = Random.initialSeed 1
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientConnected sessionid clientId ->
            ( model, Cmd.none )

        ClientDisconnected sessionId clientId ->
            cleanup model clientId


cleanup : Model -> ClientId -> ( Model, Cmd BackendMsg )
cleanup model clientId =
    let
        existingClientRoomId =
            model.clientRooms |> Dict.get clientId

        maybeCleanedExistingRoom =
            existingClientRoomId |> Maybe.andThen (\roomId -> model.rooms |> Dict.get roomId |> Maybe.map (removeClientFromRoom clientId))

        notifyOthersInRoom =
            maybeCleanedExistingRoom |> Maybe.map sendRoomStateToRoomClients |> Maybe.withDefault []

        newRooms =
            (case maybeCleanedExistingRoom of
                Just room ->
                    model.rooms
                        |> Dict.insert room.roomId room

                Nothing ->
                    model.rooms
            )
                -- Remove empty rooms
                |> Dict.filter (\key value -> value.clients |> (not << Set.isEmpty))
    in
    { model | clientRooms = model.clientRooms |> Dict.remove clientId, rooms = newRooms }
        |> withCmds notifyOthersInRoom


removeClientFromRoom : ClientId -> Room -> Room
removeClientFromRoom clientId r =
    { r
        | clients = r.clients |> Set.remove clientId
        , votes = r.votes |> List.filter (\x -> x.client /= clientId)
    }


sendUpdatedRoomVotesToClients : Room -> List (Cmd backendMsg)
sendUpdatedRoomVotesToClients room =
    room.clients |> Set.toList |> List.map (\cId -> Lamdera.sendToFrontend cId (VotesUpdated (room.votes |> List.map .vote)))


sendUpdatedUserCountToClients : Room -> List (Cmd backendMsg)
sendUpdatedUserCountToClients room =
    room.clients |> Set.toList |> List.map (\cId -> Lamdera.sendToFrontend cId (UsersInRoomUpdated (room.clients |> Set.size)))


sendUpdatedVoteVisibilityToClients : Room -> List (Cmd backendMsg)
sendUpdatedVoteVisibilityToClients room =
    room.clients |> Set.toList |> List.map (\cId -> Lamdera.sendToFrontend cId (VoteVisibilityUpdatedToFrontend { displayVotesWhileVoting = room.votesVisibleWhileVoting, revealVotesThisRound = room.displayVotesForThisRound }))


sendRoomStateToRoomClients : Room -> List (Cmd backendMsg)
sendRoomStateToRoomClients room =
    sendUpdatedRoomVotesToClients room
        ++ sendUpdatedUserCountToClients room
        ++ sendUpdatedVoteVisibilityToClients room


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        doNothing =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        StartedNewRoundToBackend roomId ->
            updateRoom model roomId (\r -> { r | votes = [], displayVotesForThisRound = r.votesVisibleWhileVoting })

        ChangedVoteToBackend roomId vote ->
            updateRoom model roomId (\r -> addVoteToRoom r vote clientId)

        JoinedRoomToBackend roomIdInput ->
            let
                ( roomId, nextSeed ) =
                    let
                        trimmedRoomId =
                            roomIdInput |> String.toLower |> String.trim |> String.left 20
                    in
                    if trimmedRoomId |> String.isEmpty then
                        Random.step Utils.getRandomWord model.randomNext

                    else
                        ( trimmedRoomId, model.randomNext )

                previousRoomForClient =
                    model.clientRooms |> Dict.get clientId |> Maybe.andThen (\rid -> model.rooms |> Dict.get rid |> Maybe.map (removeClientFromRoom clientId))

                existingRoom =
                    model.rooms |> Dict.get roomId

                newRoomData =
                    case existingRoom of
                        Just room ->
                            let
                                clients =
                                    room.clients |> Set.insert clientId
                            in
                            { room | clients = clients }

                        Nothing ->
                            { roomId = roomId
                            , clients = Set.fromList [ clientId ]
                            , votes = []
                            , votesVisibleWhileVoting = True
                            , displayVotesForThisRound = True
                            }

                updatedRooms =
                    [ previousRoomForClient, Just newRoomData ] |> List.filterMap identity

                newRooms =
                    updatedRooms |> List.foldr (\r acc -> acc |> Dict.insert r.roomId r) model.rooms
            in
            { model | rooms = newRooms, clientRooms = model.clientRooms |> Dict.insert clientId roomId, randomNext = nextSeed }
                |> withCmds
                    ([ Lamdera.sendToFrontend clientId (JoinedRoomWithIdToFrontend roomId (newRoomData.votes |> List.map .vote)) ]
                        ++ (updatedRooms |> List.concatMap sendRoomStateToRoomClients)
                    )

        ShowVotesInRoomForRound roomId show ->
            updateRoom model roomId (\r -> { r | displayVotesForThisRound = show })

        SetShowVotesInRoomSettingToBackend roomId show ->
            updateRoom model roomId (\r -> { r | votesVisibleWhileVoting = show })


updateRoom : Model -> RoomId -> (Room -> Room) -> ( Model, Cmd BackendMsg )
updateRoom model roomId roomUpdate =
    let
        maybeRoom =
            model.rooms
                |> Dict.get roomId
    in
    case maybeRoom of
        Nothing ->
            ( model, Cmd.none )

        Just existingRoom ->
            let
                updatedRoom =
                    roomUpdate existingRoom
            in
            { model | rooms = model.rooms |> Dict.insert roomId updatedRoom }
                |> withCmds (sendRoomStateToRoomClients updatedRoom)


dictInsertIfJust key maybeValue dict =
    maybeValue |> Maybe.map (\v -> Dict.insert key v dict) |> Maybe.withDefault dict


addVoteToRoom : Room -> Vote -> ClientId -> Room
addVoteToRoom r vote clientId =
    let
        newVotes =
            ({ vote = vote, client = clientId }
                :: r.votes
            )
                |> List.Extra.uniqueBy (\x -> x.client)
                -- Limit to max 100 votes
                |> List.take 100
    in
    { r
        | clients = r.clients |> Set.insert clientId
        , votes = newVotes
    }

module Evergreen.V22.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Random
import Set
import Url


type alias RoomId =
    String


type Route
    = LobbyPage
    | RoomPage RoomId


type Vote
    = TFB
    | NFC
    | One


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Route
    , mySelectedVote : Maybe Vote
    , othersVotes : List Vote
    , userCount : Int
    , hideVotes : Bool
    , roomHideVotesState : Bool
    , roomIdInput : String
    , roomId : String
    }


type alias VoteWithOwner =
    { client : Lamdera.ClientId
    , vote : Vote
    }


type alias Room =
    { roomId : String
    , clients : Set.Set Lamdera.SessionId
    , votes : List VoteWithOwner
    , votesVisibleWhileVoting : Bool
    , displayVotesForThisRound : Bool
    }


type alias BackendModel =
    { message : String
    , rooms : Dict.Dict String Room
    , clientRooms : Dict.Dict Lamdera.ClientId RoomId
    , randomNext : Random.Seed
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | ChangedRoomIdInput String
    | ChangedVoteFrontendMsg Vote
    | ToggeledHiddenVotes Bool
    | ToggeledHiddenVotesForRoom Bool
    | StartedNewRoundFrontendMsg
    | JoinedRoomFrontendMsg RoomId


type ToBackend
    = NoOpToBackend
    | ChangedVoteToBackend RoomId Vote
    | StartedNewRoundToBackend RoomId
    | JoinedRoomToBackend RoomId
    | ShowVotesInRoomForRound RoomId Bool
    | SetShowVotesInRoomSettingToBackend RoomId Bool


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | VotesUpdated (List Vote)
    | UsersInRoomUpdated Int
    | VoteVisibilityUpdatedToFrontend
        { displayVotesWhileVoting : Bool
        , revealVotesThisRound : Bool
        }
    | JoinedRoomWithIdToFrontend RoomId (List Vote)

module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict
import Lamdera exposing (ClientId, SessionId)
import Random exposing (Seed)
import Set
import Url exposing (Url)


type Route
    = LobbyPage
    | RoomPage RoomId


type alias FrontendModel =
    { key : Key
    , route : Route
    , mySelectedVote : Maybe Vote
    , othersVotes : List Vote
    , userCount : Int
    , hideVotes : Bool
    , roomHideVotesState : Bool
    , roomIdInput : String
    , roomId : String
    }


type alias Room =
    { roomId : String
    , clients : Set.Set SessionId
    , votes : List VoteWithOwner
    , votesVisibleWhileVoting : Bool
    , displayVotesForThisRound : Bool
    }


type alias BackendModel =
    { message : String
    , rooms : Dict.Dict String Room
    , clientRooms : Dict.Dict ClientId RoomId
    , randomNext : Seed
    }


type alias VoteWithOwner =
    { client : ClientId
    , vote : Vote
    }


type Vote
    = TFB
    | NFC
    | One


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | ChangedRoomIdInput String
    | ChangedVoteFrontendMsg Vote
    | ToggeledHiddenVotes Bool
    | ToggeledHiddenVotesForRoom Bool
    | StartedNewRoundFrontendMsg
    | JoinedRoomFrontendMsg RoomId


type alias RoomId =
    String


type ToBackend
    = NoOpToBackend
    | ChangedVoteToBackend RoomId Vote
    | StartedNewRoundToBackend RoomId
    | JoinedRoomToBackend RoomId
    | ShowVotesInRoomForRound RoomId Bool
    | SetShowVotesInRoomSettingToBackend RoomId Bool


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | VotesUpdated (List Vote)
    | UsersInRoomUpdated Int
    | VoteVisibilityUpdatedToFrontend { displayVotesWhileVoting : Bool, revealVotesThisRound : Bool }
    | JoinedRoomWithIdToFrontend RoomId (List Vote)

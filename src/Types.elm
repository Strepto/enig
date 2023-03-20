module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , mySelectedVote : Maybe Vote
    , othersVotes : List Vote
    }


type alias BackendModel =
    { message : String
    , votes : List VoteWithOwner
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
    | ChangedVoteFrontendMsg Vote
    | StartedNewRoundFrontendMsg


type ToBackend
    = NoOpToBackend
    | ChangedVoteToBackend Vote
    | StartedNewRoundToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | VotesUpdated (List Vote)

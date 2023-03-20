module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Url


type Vote
    = TFB
    | NFC
    | One


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , mySelectedVote : Maybe Vote
    , othersVotes : List Vote
    }


type alias VoteWithOwner =
    { client : Lamdera.ClientId
    , vote : Vote
    }


type alias BackendModel =
    { message : String
    , votes : List VoteWithOwner
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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

module Evergreen.Migrate.V9 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.com/docs/evergreen> for more info.

-}

import Evergreen.V8.Types
import Evergreen.V9.Types
import Lamdera.Migrations exposing (..)
import List
import Maybe


frontendModel : Evergreen.V8.Types.FrontendModel -> ModelMigration Evergreen.V9.Types.FrontendModel Evergreen.V9.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V8.Types.BackendModel -> ModelMigration Evergreen.V9.Types.BackendModel Evergreen.V9.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V8.Types.FrontendMsg -> MsgMigration Evergreen.V9.Types.FrontendMsg Evergreen.V9.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V8.Types.ToBackend -> MsgMigration Evergreen.V9.Types.ToBackend Evergreen.V9.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V8.Types.BackendMsg -> MsgMigration Evergreen.V9.Types.BackendMsg Evergreen.V9.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V8.Types.ToFrontend -> MsgMigration Evergreen.V9.Types.ToFrontend Evergreen.V9.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_FrontendModel : Evergreen.V8.Types.FrontendModel -> Evergreen.V9.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , route = old.route |> migrate_Types_Route
    , mySelectedVote = old.mySelectedVote |> Maybe.map migrate_Types_Vote
    , othersVotes = old.othersVotes |> List.map migrate_Types_Vote
    , userCount = 0
    , roomIdInput = old.roomIdInput
    , roomId = old.roomId
    }


migrate_Types_Route : Evergreen.V8.Types.Route -> Evergreen.V9.Types.Route
migrate_Types_Route old =
    case old of
        Evergreen.V8.Types.LobbyPage ->
            Evergreen.V9.Types.LobbyPage

        Evergreen.V8.Types.RoomPage p0 ->
            Evergreen.V9.Types.RoomPage p0


migrate_Types_ToFrontend : Evergreen.V8.Types.ToFrontend -> Evergreen.V9.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V8.Types.NoOpToFrontend ->
            Evergreen.V9.Types.NoOpToFrontend

        Evergreen.V8.Types.VotesUpdated p0 ->
            Evergreen.V9.Types.VotesUpdated (p0 |> List.map migrate_Types_Vote)

        Evergreen.V8.Types.JoinedRoomWithIdToFrontend p0 p1 ->
            Evergreen.V9.Types.JoinedRoomWithIdToFrontend p0 (p1 |> List.map migrate_Types_Vote)


migrate_Types_Vote : Evergreen.V8.Types.Vote -> Evergreen.V9.Types.Vote
migrate_Types_Vote old =
    case old of
        Evergreen.V8.Types.TFB ->
            Evergreen.V9.Types.TFB

        Evergreen.V8.Types.NFC ->
            Evergreen.V9.Types.NFC

        Evergreen.V8.Types.One ->
            Evergreen.V9.Types.One

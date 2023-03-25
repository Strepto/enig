module Evergreen.Migrate.V8 exposing (..)

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

import Evergreen.V5.Types
import Evergreen.V8.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V5.Types.FrontendModel -> ModelMigration Evergreen.V8.Types.FrontendModel Evergreen.V8.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V5.Types.BackendModel -> ModelMigration Evergreen.V8.Types.BackendModel Evergreen.V8.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V5.Types.FrontendMsg -> MsgMigration Evergreen.V8.Types.FrontendMsg Evergreen.V8.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V5.Types.ToBackend -> MsgMigration Evergreen.V8.Types.ToBackend Evergreen.V8.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V5.Types.BackendMsg -> MsgMigration Evergreen.V8.Types.BackendMsg Evergreen.V8.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V5.Types.ToFrontend -> MsgMigration Evergreen.V8.Types.ToFrontend Evergreen.V8.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_ToFrontend : Evergreen.V5.Types.ToFrontend -> Evergreen.V8.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V5.Types.NoOpToFrontend ->
            Evergreen.V8.Types.NoOpToFrontend

        Evergreen.V5.Types.VotesUpdated p0 ->
            Evergreen.V8.Types.VotesUpdated (p0 |> List.map migrate_Types_Vote)

        Evergreen.V5.Types.JoinedRoomWithIdToFrontend p0 ->
            Evergreen.V8.Types.JoinedRoomWithIdToFrontend p0
                []


migrate_Types_Vote : Evergreen.V5.Types.Vote -> Evergreen.V8.Types.Vote
migrate_Types_Vote old =
    case old of
        Evergreen.V5.Types.TFB ->
            Evergreen.V8.Types.TFB

        Evergreen.V5.Types.NFC ->
            Evergreen.V8.Types.NFC

        Evergreen.V5.Types.One ->
            Evergreen.V8.Types.One
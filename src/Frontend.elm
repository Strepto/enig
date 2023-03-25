module Frontend exposing (..)

import AppUrl
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cmd.Extra exposing (addCmd, addCmds, andThen, withCmd, withCmds, withNoCmd)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import ElmUi.Cursor as Cursor
import ElmUi.Keyboard
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        route =
            urlParser url
    in
    ( { key = key
      , route = route
      , mySelectedVote = Nothing
      , othersVotes = []
      , roomIdInput = ""
      , roomId = ""
      }
    , case route of
        RoomPage room ->
            Lamdera.sendToBackend (JoinedRoomToBackend room)

        LobbyPage ->
            Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                route =
                    urlParser url
            in
            case route of
                RoomPage code ->
                    { model | route = route, roomId = code } |> withNoCmd

                _ ->
                    ( { model | route = urlParser url, roomId = "" }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ChangedVoteFrontendMsg vote ->
            { model | mySelectedVote = Just vote, othersVotes = vote :: model.othersVotes } |> withCmd (Lamdera.sendToBackend (ChangedVoteToBackend model.roomId vote))

        StartedNewRoundFrontendMsg ->
            { model | mySelectedVote = Nothing, othersVotes = [] } |> withCmd (Lamdera.sendToBackend StartedNewRoundToBackend)

        JoinedRoomFrontendMsg roomId ->
            model |> withCmd (Lamdera.sendToBackend (JoinedRoomToBackend roomId))

        ChangedRoomIdInput roomIdInput ->
            { model | roomIdInput = roomIdInput } |> withNoCmd


urlParser url =
    let
        appUrl =
            AppUrl.fromUrl url
    in
    case appUrl.path of
        [ "plan", x ] ->
            RoomPage x

        _ ->
            LobbyPage


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        VotesUpdated v ->
            let
                -- Reset vote if this is a new round
                myVote =
                    if v |> List.isEmpty then
                        Nothing

                    else
                        model.mySelectedVote
            in
            { model | othersVotes = v, mySelectedVote = myVote } |> withNoCmd

        JoinedRoomWithIdToFrontend roomId votes ->
            { model | roomId = roomId, othersVotes = votes, mySelectedVote = Nothing } |> withCmd (Nav.pushUrl model.key ("/plan/" ++ roomId))


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Enig"
    , body =
        [ Element.layout [ Bg.color (rgb255 240 240 240) ] (viewContent model)
        ]
    }


hasJoinedRoom model =
    (model.roomId |> String.trim |> String.isEmpty) |> not


viewContent model =
    column [ width fill, height fill ]
        ([ viewHeader
         ]
            ++ [ if hasJoinedRoom model then
                    viewWhenJoinedRoom model

                 else
                    viewJoinRoom model
               ]
        )


viewWhenJoinedRoom model =
    column [ width fill ]
        [ viewPickedCards model
        , model.mySelectedVote |> Maybe.map viewSelectedCard |> Maybe.withDefault (viewCardRow model)
        , el [ centerX, padding 40 ]
            (paragraph []
                [ text "You are in session: '"
                , el [ Font.family [ Font.monospace ] ] (text model.roomId)
                , text "'. Share the code or url with others for them to join you."
                ]
            )
        ]


viewSelectedCard card =
    column [ centerX ]
        [ paragraph [] [ text ("You voted: " ++ cardTypeToString card) ]
        , Input.button [ Border.rounded 4, Bg.color (rgb255 240 180 180), padding 12 ] { label = el [] (text "Start new voting round\nfor everyone ♻"), onPress = Just StartedNewRoundFrontendMsg }
        ]


viewJoinRoom model =
    column [ centerX, width (480 |> px), spacing 10, padding 64 ]
        [ paragraph []
            [ text "Enig is another estimation app! It cuts estimation to the bare minimum."
            ]
        , el [ height (30 |> px) ] none
        , column [ Bg.color colorWhite, Border.rounded 10, padding 10, spacing 10 ]
            [ paragraph []
                [ text "Start a new session or join an ongoing!"
                ]
            , Input.button
                [ Bg.color (rgb255 100 255 100)
                , padding 10
                , Border.rounded 4
                ]
                { label = el [] (text "Start Session"), onPress = Just (JoinedRoomFrontendMsg "") }
            , el [ height (25 |> px) ] (text "")
            , Input.text
                [ ElmUi.Keyboard.onEnterUp (JoinedRoomFrontendMsg model.roomIdInput)
                ]
                { onChange = \text -> ChangedRoomIdInput text
                , text = model.roomIdInput
                , placeholder = Just (Input.placeholder [] (text "Session code"))
                , label = Input.labelAbove [] (el [] (text "Join an existing session"))
                }
            , Input.button
                [ if model.roomIdInput |> String.trim |> String.isEmpty then
                    Bg.color (rgb255 90 90 90)

                  else
                    Bg.color (rgb255 100 255 100)
                , padding 10
                , Border.rounded 4
                ]
                { label = el [] (text "Join Session"), onPress = Just (JoinedRoomFrontendMsg model.roomIdInput) }
            ]
        ]


viewHeader =
    row [ width fill, height (48 |> px), padding 10, Bg.color (rgb255 100 240 200) ] [ text "Enig: Agree on anything" ]


cardTypes : List Vote
cardTypes =
    [ TFB, NFC, One ]


viewPickedCards model =
    wrappedRow [ centerX, spacing -20 ] (model.othersVotes |> List.reverse |> List.map viewCard)


viewCardRow : Model -> Element FrontendMsg
viewCardRow model =
    column [ centerX, centerY ]
        [ row [ centerX, centerY, spacing 10 ]
            (cardTypes |> List.map (\c -> viewCardClickable c))
        , paragraph [] [ text "Choose your vote by clicking a card." ]
        ]


viewCardClickable card =
    Input.button []
        { label = viewCard card
        , onPress = Just <| ChangedVoteFrontendMsg card
        }


viewOtherVotes votes =
    votes |> List.map viewCard


cardColorScheme card =
    case card of
        TFB ->
            [ rgb255 230 100 100, rgb255 200 120 100 ]

        NFC ->
            [ rgb255 255 230 50, rgb255 255 230 0 ]

        One ->
            [ rgb255 130 255 130, rgb255 180 255 80 ]


viewCard card =
    column
        [ width (100 |> px)
        , height (160 |> px)
        , Border.rounded 10
        , Bg.gradient { angle = 20, steps = cardColorScheme card }
        , padding 30
        , Element.behindContent (el [ centerX, centerY, Font.bold, rotate (degrees 80), Font.size 80, Element.alpha 0.5 ] (text (cardTypeToShortString card)))
        ]
        [ el [ centerX, Bg.color colorWhite, padding 8, Border.rounded 10 ] (paragraph [] [ text (cardTypeToString card) ]) ]


colorWhite =
    rgb255 252 252 252


colorBlack =
    rgb255 10 10 10


cardTypeToString cardType =
    case cardType of
        TFB ->
            "Too Big 🙋"

        NFC ->
            "No Clue 🤷"

        One ->
            "1"


cardTypeToShortString cardType =
    case cardType of
        TFB ->
            "TFB"

        NFC ->
            "NFC"

        One ->
            "1"

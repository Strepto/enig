module Frontend exposing (..)

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
import Html
import Html.Attributes exposing (disabled)
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
    ( { key = key
      , mySelectedVote = Nothing
      , othersVotes = []
      }
    , Cmd.none
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
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ChangedVoteFrontendMsg vote ->
            { model | mySelectedVote = Just vote } |> withCmd (Lamdera.sendToBackend (ChangedVoteToBackend vote))

        StartedNewRoundFrontendMsg ->
            { model | mySelectedVote = Nothing, othersVotes = [] } |> withCmd (Lamdera.sendToBackend StartedNewRoundToBackend)


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


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Plannin' 🃏"
    , body =
        [ Element.layout [] (viewContent model)
        ]
    }


viewContent model =
    column [ width fill ]
        [ viewHeader
        , viewPickedCards model
        , model.mySelectedVote |> Maybe.map viewSelectedCard |> Maybe.withDefault (viewCardRow model)
        ]


viewSelectedCard card =
    column [ centerX ]
        [ paragraph [] [ text ("You voted: " ++ cardTypeToString card) ]
        , Input.button [ Border.rounded 4, Bg.color (rgb255 240 180 180), padding 12 ] { label = el [] (text "Start new voting round\nfor everyone ♻"), onPress = Just StartedNewRoundFrontendMsg }
        ]


viewHeader =
    row [ width fill, height (48 |> px), padding 10, Bg.color (rgb255 100 240 200) ] [ text "Planner" ]


cardTypes : List Vote
cardTypes =
    [ TFB, NFC, One ]


viewPickedCards model =
    wrappedRow [ centerX, spacing -20 ] (model.othersVotes |> List.map viewCard)


viewCardRow : Model -> Element FrontendMsg
viewCardRow model =
    row [ centerX, centerY, spacing 10 ]
        (cardTypes |> List.map (\c -> viewCardClickable c))


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

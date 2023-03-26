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
import Element.Region
import ElmUi.Cursor as Cursor
import ElmUi.Keyboard
import Html.Attributes
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
      , userCount = 0
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

        UsersInRoomUpdated count ->
            { model | userCount = count } |> withNoCmd

        JoinedRoomWithIdToFrontend roomId votes ->
            { model | roomId = roomId, othersVotes = votes, mySelectedVote = Nothing } |> withCmd (Nav.pushUrl model.key ("/plan/" ++ roomId))


view : Model -> Browser.Document FrontendMsg
view model =
    { title =
        "Enig 🤝"
            ++ (if hasJoinedRoom model then
                    " " ++ model.roomId

                else
                    ""
               )
    , body =
        [ Element.layout [ Bg.color (rgb255 240 240 240) ]
            (column [ width fill, height fill ]
                [ viewHeader
                , viewContent model
                , viewFooter
                ]
            )
        ]
    }


viewFooter =
    row [ alignBottom, centerX, Font.color (colorBlackWithAlpha01 0.3) ]
        [ newTabLink []
            { url = "https://github.com/Strepto/enig"
            , label =
                el
                    [ Font.family [ Font.monospace ]
                    , Font.size 12
                    ]
                    (text "Enig by Nils Henrik Hals")
            }
        ]


hasJoinedRoom model =
    (model.roomId |> String.trim |> String.isEmpty) |> not


viewContent model =
    column [ width fill, height fill, Element.Region.mainContent ]
        [ if hasJoinedRoom model then
            viewWhenJoinedRoom model

          else
            viewJoinRoom model
        ]


viewWhenJoinedRoom model =
    column [ width fill, spacing 16, paddingXY 0 10 ]
        [ viewPickedCards model
        , el [ width fill, height (px 1), Bg.color <| colorBlackWithAlpha01 0.1 ] none
        , model.mySelectedVote |> Maybe.map viewSelectedCard |> Maybe.withDefault (viewCardRow model)
        , el [ centerX, padding 40 ]
            (paragraph []
                [ text "You are in session '"
                , el [ Font.family [ Font.monospace ] ] (text model.roomId)
                , text "'. "
                , if model.userCount < 2 then
                    paragraph []
                        [ text <| "Share the code '" ++ model.roomId ++ "' (or url) with others for them to join you."
                        ]

                  else
                    text <| "" ++ String.fromInt model.userCount ++ " people are here."
                ]
            )
        ]


viewSelectedCard card =
    column [ centerX, spacing 10 ]
        [ paragraph [] [ text "You voted: ", el [] (text (cardTypeToString card)) ]
        , viewButton { label = "Start new round ♻", action = StartedNewRoundFrontendMsg }
        ]


viewJoinRoom model =
    column [ centerX, width (520 |> px), spacing 10, padding 64 ]
        [ paragraph [ padding 10 ]
            [ text "Enig is just another estimation app! It cuts estimation to the bare minimum."
            ]
        , el [ height (30 |> px) ] none
        , column
            [ Bg.color colorWhite
            , Border.rounded 10
            , padding 10
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 2
                , blur = 2
                , color = colorBlackWithAlpha01 0.1
                }
            ]
            [ column
                [ spacing 10
                , padding 10
                ]
                [ paragraph []
                    [ text "Start a new session, or use a code to join your colleagues!"
                    ]
                , viewButton { label = "New Session", action = JoinedRoomFrontendMsg "" }
                , el [ height (25 |> px) ] (text "")
                , Input.text
                    [ ElmUi.Keyboard.onEnterUp (JoinedRoomFrontendMsg model.roomIdInput)
                    , htmlAttribute (Html.Attributes.style "text-transform" "lowercase")
                    ]
                    { onChange = \text -> ChangedRoomIdInput text
                    , text = model.roomIdInput
                    , placeholder = Just (Input.placeholder [] (text "Session code"))
                    , label = Input.labelAbove [] (el [] (text "Join your colleagues:"))
                    }
                , if model.roomIdInput |> String.isEmpty |> not then
                    viewButton { label = "Join session", action = JoinedRoomFrontendMsg "" }

                  else
                    none
                ]
            ]
        ]


viewHeader =
    row
        [ width fill
        , height (48 |> px)
        , paddingXY 4 0
        , Bg.color (rgb255 100 240 200)
        , Border.shadow
            { offset = ( -2, 0 )
            , size = 0
            , blur = 5
            , color = colorBlack
            }
        ]
        [ link []
            { url = "/"
            , label =
                row []
                    [ el [ Font.size 25, centerY, paddingXY 6 0 ] (text "🤝")
                    , el
                        [ centerY
                        , Border.widthEach
                            { bottom = 1
                            , left = 0
                            , right = 0
                            , top = 0
                            }
                        , Border.color colorTransparent
                        , mouseOver
                            [ Border.color colorBlack
                            ]
                        ]
                        (text "Enig: Agree on anything")
                    ]
            }
        ]


cardTypes : List Vote
cardTypes =
    [ TFB, NFC, One ]


viewPickedCards : Model -> Element FrontendMsg
viewPickedCards model =
    row [ Element.Region.announce, width fill, Element.Region.description "Picked cards" ]
        [ if model.othersVotes |> List.isEmpty |> not then
            wrappedRow [ centerX, spacing -20 ] (model.othersVotes |> List.reverse |> List.map viewCard)

          else
            row [ centerX, height (px 160), Font.color (colorBlackWithAlpha01 0.8) ] [ paragraph [] [ text "Nobody has voted yet" ] ]
        ]


viewCardRow : Model -> Element FrontendMsg
viewCardRow model =
    column [ centerX, centerY ]
        [ row [ centerX, centerY, spacing 10 ]
            (cardTypes |> List.map (\c -> viewCardClickable c))
        , paragraph [] [ text "Choose your vote by clicking a card." ]
        ]


viewCardClickable card =
    Input.button
        [ Border.width 1
        , Border.rounded 10
        , Border.color colorTransparent
        , mouseOver
            [ Border.color (colorBlackWithAlpha01 0.5)
            ]
        ]
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
        [ Element.Region.description ("Card: " ++ cardTypeToString card)
        , width (100 |> px)
        , height (160 |> px)
        , Border.rounded 10
        , Bg.gradient { angle = 20, steps = cardColorScheme card }
        , padding 30
        , Element.behindContent (el [ centerX, centerY, Font.bold, rotate (degrees 80), Font.size 80, Element.alpha 0.5, Element.Region.description "" ] (text (cardTypeToShortString card)))
        , htmlAttribute (Html.Attributes.style "transition" "opacity 0.1s")
        , Border.shadow
            { offset = ( -2, 1 )
            , size = 0
            , blur = 3
            , color = colorBlackWithAlpha01 0.3
            }
        ]
        [ el [ centerX, centerY, Bg.color colorWhite, padding 8, Border.rounded 10 ] (paragraph [ Font.center ] [ text (cardTypeToString card) ]) ]


colorWhite =
    rgb255 252 252 252


colorBlack =
    rgb255 10 10 10


colorTransparent =
    colorBlackWithAlpha01 0


colorBlackWithAlpha01 alpha =
    rgba255 0 0 0 alpha


cardTypeToString cardType =
    case cardType of
        TFB ->
            "Too Big 🙋"

        NFC ->
            "No Clue 🤷"

        One ->
            "1 🎉"


borderOutline =
    htmlAttribute (Html.Attributes.style "outline" "3px solid ")


viewButton : { label : String, action : msg } -> Element msg
viewButton { label, action } =
    let
        bgColor =
            rgb255 0 122 60

        shadowColor =
            rgb255 0 42 24

        bgHoverColor =
            rgb255 0 90 48

        focusOutlineColor =
            rgb255 255 221 0
    in
    Input.button
        [ paddingEach
            { top = 8
            , right = 10
            , bottom = 7
            , left = 10
            }
        , Bg.color bgColor
        , Font.color colorWhite
        , Border.width 2
        , Border.color bgColor
        , Border.shadow
            { offset = ( 0, 3 )
            , size = 0
            , blur = 0
            , color = shadowColor
            }
        , Border.rounded 4
        , mouseOver
            [ Bg.color bgHoverColor
            , Border.color bgHoverColor
            , Font.color colorWhite
            ]
        , focused
            [ Border.color focusOutlineColor
            , Bg.color focusOutlineColor
            , Font.color colorBlack
            , Border.shadow
                { offset = ( 0, 0 )
                , size = -2
                , blur = 0
                , color = focusOutlineColor
                }
            ]
        ]
        { onPress = Just action
        , label = el [] (Element.text label)
        }


cardTypeToShortString cardType =
    case cardType of
        TFB ->
            "TFB"

        NFC ->
            "NFC"

        One ->
            "1"

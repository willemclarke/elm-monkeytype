module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Heroicons.Outline
import Html exposing (Html, div, samp, text)
import Html.Attributes exposing (class, id, tabindex)
import Html.Attributes.Extra
import Html.Events exposing (on)
import Html.Extra as HE
import Json.Decode as JD
import Random
import Random.List
import Svg.Attributes
import Svg.Events
import Task
import Time



-- MODEL


type alias Model =
    { gameState : GameState
    , input : String
    , userInput : String
    , timeRemaining : Int
    , correct : Int
    , mistakes : Int
    }


type UserInput
    = Key String
    | Control String


type GameState
    = Idle
    | Playing
    | Finished


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , Cmd.batch [ generateRandomInputCmd, Dom.focus "outermost" |> Task.attempt (always NoOp) ]
    )


defaultModel : Model
defaultModel =
    { gameState = Idle, input = "", userInput = "", timeRemaining = 20, correct = 0, mistakes = 0 }



-- UPDATE


type Msg
    = HandleKeyboardEvent UserInput
    | Tick Time.Posix
    | Retry
    | GenerateInput (List String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent userInput ->
            let
                newUserInput =
                    handleUserInput userInput model.userInput

                { correct, mistakes } =
                    computeGameResults model.input newUserInput
            in
            ( { model
                | userInput = newUserInput
                , gameState = Playing
                , correct = correct
                , mistakes = mistakes
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model
                | gameState = computeGameState model.timeRemaining
                , timeRemaining = computeTimeRemaining model.timeRemaining
              }
            , Cmd.none
            )

        Retry ->
            ( defaultModel, generateRandomInputCmd )

        GenerateInput randomStrings ->
            ( { model | input = formatGeneratedInput randomStrings }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


computeGameState : Int -> GameState
computeGameState timeRemaining =
    if timeRemaining == 1 then
        Finished

    else
        Playing


computeTimeRemaining : Int -> Int
computeTimeRemaining timeRemaining =
    timeRemaining - 1


handleUserInput : UserInput -> String -> String
handleUserInput newUserInput prevUserInput =
    case newUserInput of
        Key string ->
            prevUserInput ++ string

        Control string ->
            if string == "Backspace" then
                prevUserInput |> String.dropRight 1

            else
                prevUserInput


keyDecoder : JD.Decoder UserInput
keyDecoder =
    JD.map toKey (JD.field "key" JD.string)


toKey : String -> UserInput
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Key (String.fromChar char)

        _ ->
            Control string


gameInputToListPairs : String -> String -> List ( String, String )
gameInputToListPairs input userInput =
    let
        inputWords =
            String.words input

        userInputWords =
            String.words userInput
    in
    List.map2 Tuple.pair inputWords userInputWords


computeGameResults : String -> String -> { correct : Int, mistakes : Int }
computeGameResults input userInput =
    let
        correctWords =
            gameInputToListPairs input userInput
                |> List.filter (\( iWord, uiWord ) -> iWord == uiWord)
                |> List.length

        incorrectWords =
            gameInputToListPairs input userInput
                |> List.filter (\( iWord, uiWord ) -> iWord /= uiWord)
                |> List.length
    in
    { correct = correctWords, mistakes = incorrectWords }


gameStateToString : GameState -> String
gameStateToString gameState =
    case gameState of
        Idle ->
            "Idle"

        Playing ->
            "Playing"

        Finished ->
            "Finished"


isGameDisabled : GameState -> Bool
isGameDisabled gameState =
    case gameState of
        Playing ->
            True

        _ ->
            False


formatGeneratedInput : List String -> String
formatGeneratedInput randomStrings =
    randomStrings
        |> List.take 20
        |> List.map String.toLower
        |> String.join " "


generateRandomInputCmd : Cmd Msg
generateRandomInputCmd =
    Random.List.shuffle randomWords
        |> Random.generate GenerateInput



-- VIEW


view : Model -> Html Msg
view model =
    let
        keydown =
            if model.gameState == Finished then
                Html.Attributes.Extra.empty

            else
                on "keydown" <| JD.map HandleKeyboardEvent keyDecoder
    in
    div
        [ class "flex justify-center h-full w-full bg-[#2d3748]"
        , keydown
        , id "outermost"
        , tabindex 0
        ]
        [ div [ class "flex flex-col items-center w-96" ]
            [ div [ class "font-bold italic text-[#ecc94b] my-6 text-3xl" ]
                [ text "monkeytype" ]
            , viewGameStatus model
            , div [ class "relative text-[#a0aec0] text-2xl font-bold my-2 break-all" ]
                [ viewUserInput { input = model.input, userInput = model.userInput }
                , samp [] [ text model.input ]
                ]
            , viewGameResults model
            , retryButton model.gameState
            ]
        ]


viewGameStatus : Model -> Html msg
viewGameStatus model =
    div [ class "w-full" ]
        [ div [ class "font-bold text-[#d0cfc4] text-sm" ]
            [ text <| "Status: " ++ gameStateToString model.gameState ]
        , div [ class "font-bold text-[#d0cfc4] text-sm" ]
            [ text <| "Time remaining: " ++ String.fromInt model.timeRemaining ]
        ]


viewUserInput : { input : String, userInput : String } -> Html msg
viewUserInput { input, userInput } =
    let
        typedCharacters =
            String.split "" userInput

        splitInput =
            String.split "" input

        pairs =
            List.map2 Tuple.pair typedCharacters splitInput
    in
    div [ class "absolute inset-0 text-[#d0cfc4] text-2xl font-bold whitespace-pre-wrap" ]
        (List.map (\( typedChar, inputChar ) -> viewCharacter { actual = typedChar, expected = inputChar }) pairs)


viewCharacter : { actual : String, expected : String } -> Html msg
viewCharacter { actual, expected } =
    let
        isValidChar =
            actual == expected

        isWhiteSpace =
            expected == " "

        textColor =
            if not isValidChar && not isWhiteSpace then
                "text-red-400"

            else
                "text-[#ecc94b]"

        bgColor =
            if not isValidChar && isWhiteSpace then
                "bg-red-500"

            else
                "bg-transparent"
    in
    samp
        [ class <| String.join " " [ textColor, bgColor, "h-8" ] ]
        [ text expected ]


viewGameResults : Model -> Html msg
viewGameResults model =
    div [ class "w-full h-6" ]
        [ case model.gameState of
            Finished ->
                div []
                    [ div [ class "text-sm text-[#d0cfc4] font-extrabold" ]
                        [ text <| "Mistakes: " ++ String.fromInt model.mistakes ]
                    , div [ class "text-sm text-[#d0cfc4] font-extrabold" ]
                        [ text <| String.join "" [ "Correct: ", String.fromInt model.correct, "/", String.fromInt (List.length <| String.words model.input) ] ]
                    ]

            _ ->
                HE.nothing
        ]


retryButton : GameState -> Html Msg
retryButton gameState =
    let
        cursor =
            if isGameDisabled gameState then
                "cursor-not-allowed"

            else
                "cursor-pointer"

        onClick_ =
            if isGameDisabled gameState then
                NoOp

            else
                Retry
    in
    Heroicons.Outline.refresh
        [ Svg.Attributes.class <| String.join " " [ cursor, "my-12 h-6 w-6 text-[#d0cfc4]" ]
        , Svg.Events.onClick onClick_
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Playing ->
            if model.timeRemaining == 0 then
                Sub.none

            else
                Time.every 1000 Tick

        _ ->
            Sub.none


randomWords : List String
randomWords =
    [ "selector"
    , "reseller"
    , "and"
    , "from"
    , "piece"
    , "brockton"
    , "publishi"
    , "promised"
    , "bandages"
    , "woodford"
    , "unmanned"
    , "crabtree"
    , "staffing"
    , "infotrac"
    , "schubert"
    , "maturity"
    , "playtime"
    , "inaction"
    , "mischief"
    , "dandruff"
    , "embolism"
    , "alcohols"
    , "sheridan"
    , "levinson"
    , "notaries"
    , "wstnsand"
    , "homewood"
    , "confuses"
    , "pretrial"
    , "hardwood"
    , "vomiting"
    , "unspoken"
    , "pursuing"
    , "earphone"
    , "postpost"
    , "ambrosia"
    , "wildfire"
    , "machined"
    , "mortgage"
    , "charging"
    , "provides"
    , "nobility"
    , "sundance"
    , "benedict"
    , "reformed"
    , "floating"
    , "sardinia"
    , "scotsman"
    , "gathered"
    , "naturist"
    , "doorways"
    , "paraffin"
    , "devolved"
    , "eclectic"
    , "cambodia"
    , "eyeballs"
    , "wearable"
    , "salesmen"
    , "balances"
    , "uploader"
    , "formerly"
    , "bringing"
    , "georgina"
    , "gremlins"
    , "severely"
    , "contempt"
    , "speeding"
    , "consular"
    , "molasses"
    , "waukesha"
    , "wrenches"
    , "starting"
    , "lockport"
    , "markings"
    , "loveland"
    , "payables"
    , "sparkles"
    , "opponent"
    , "required"
    , "davidoff"
    , "february"
    , "calamity"
    , "kulkarni"
    , "meridien"
    , "foremost"
    , "homeland"
    , "wildcats"
    , "diazepam"
    , "rockfish"
    , "vampires"
    , "lindberg"
    , "solemnly"
    , "evidence"
    , "muscular"
    , "recenter"
    , "suspects"
    , "ensuring"
    , "abortion"
    , "doorbell"
    , "arrested"
    , "unshaved"
    , "stickers"
    , "mariners"
    ]

import Html exposing (Html, div, text, code, button)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import List exposing (head, repeat, map, filter, maximum)
import Random exposing (Seed, step, initialSeed)
import Time exposing (Time, millisecond)
import Keyboard exposing (KeyCode)

import Levenshtein exposing (levenshtein)
import Mutate exposing (mutate)

main : Program Never
main =
  program
  {
    init = init "ABBA" 1
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- MODEL


type State =
    Pauzed
  | Running
  | Finished


type alias TrackRecord =
  {
    best : String
  , bestDistance : Int
  , current : String
  , currentDistance : Int
  }


type alias Model =
  {
    target : String
  , state : State
  , trackRecords: List TrackRecord
  , size : Int
  , seed : Seed
  }


initialTrackRecord : String -> TrackRecord
initialTrackRecord target =
  let
    current = ""

    best = current

    distance = levenshtein target current
  in
    {
      best = best
    , bestDistance = distance
    , current = current
    , currentDistance = distance
    }


init : String -> Int -> (Model, Cmd Message)
init target n =
  ({
    target = target
  , state = Running
  , trackRecords = repeat n (initialTrackRecord target)
  , size = n
  , seed = initialSeed 0
  }
  , Cmd.none)


-- UPDATE


type Message =
    Mutate
  | Toggle
  | Restart
  | DoNothing


next : String -> TrackRecord -> Seed -> (TrackRecord, Seed)
next target trackRecord seed =
  let
    (nextCurrent, seed') = step (mutate trackRecord.best) seed

    currentBestDistance = trackRecord.bestDistance

    nextCurrentDistance = levenshtein target nextCurrent

    distance = min currentBestDistance nextCurrentDistance

    nextTrackRecord =
      if nextCurrentDistance < currentBestDistance then
        {
          best = nextCurrent
        , bestDistance = nextCurrentDistance
        , current = nextCurrent
        , currentDistance = nextCurrentDistance
        }
      else
        { trackRecord |
            current = nextCurrent
        , currentDistance = nextCurrentDistance
        }
  in
    (nextTrackRecord, seed')


update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Mutate ->
      if model.state == Running then
        let
          trackRecord =
            case head model.trackRecords of
              Just trackRecord -> trackRecord

              Nothing -> initialTrackRecord ""

          (nextTrackRecord, seed') =
            next model.target trackRecord model.seed

          nextState =
            if nextTrackRecord.bestDistance == 0 then
              Finished
            else
              model.state
        in
          ({ model |
            state = nextState
          , trackRecords = [ nextTrackRecord ]
          , seed = seed'
           }, Cmd.none)
      else
        (model, Cmd.none)

    Toggle ->
      let
        nextState =
          case model.state of
            Pauzed -> Running

            Running -> Pauzed

            _ -> model.state
      in
        ({ model | state = nextState }, Cmd.none)

    Restart ->
      init model.target model.size

    _ ->
      (model, Cmd.none)


-- VIEW


view : Model -> Html Message
view model =
  let
    trackRecord =
      case head model.trackRecords of
        Just trackRecord -> trackRecord

        Nothing -> initialTrackRecord ""

    bestDistance =
      case maximum <| map (\tr -> tr.bestDistance) model.trackRecords of
        Just distance -> distance

        Nothing -> 0

    best =
      case head <| filter (\tr -> tr.bestDistance == bestDistance) model.trackRecords of
        Just trackRecord -> trackRecord.best

        Nothing -> ""

    currentDistance =
      case maximum <| map (\tr -> tr.currentDistance) model.trackRecords of
        Just distance -> distance

        Nothing -> 0

    current =
      case head <| filter (\tr -> tr.currentDistance == currentDistance) model.trackRecords of
        Just trackRecord -> trackRecord.current

        Nothing -> ""
  in
    div []
    [
      div [] [
        text "current:"
      , code []
          [
            text "\""
          , text current
          , text "\""
          ]
      , text " "
      , text "best:"
      , code []
          [
            text "\""
          , text best
          , text "\""
          ]
      ]
    , div [ class "debug" ]
      [
        text <| toString <| currentDistance
      , text " "
      , text <| toString <| bestDistance
      ]
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  Sub.batch [
    Keyboard.downs handlePress
  , Time.every (100 * millisecond) (\_ -> Mutate)
  ]

handlePress : KeyCode -> Message
handlePress keycode =
  case keycode of
    80 -> Toggle

    82 -> Restart

    _ -> DoNothing

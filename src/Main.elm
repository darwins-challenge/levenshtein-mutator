import Html exposing (Html, div, text, code, button)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Random exposing (Seed, step, initialSeed)
import Time exposing (Time, millisecond)
import Keyboard exposing (KeyCode)

import Levenshtein exposing (levenshtein)
import Mutate exposing (mutate)

main : Program Never
main =
  program
  {
    init = init "ABBA"
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
  , current : String
  , best : (String, Int)
  , trackRecord: TrackRecord
  , seed : Seed
  }


init : String -> (Model, Cmd Message)
init target =
  let
    current = ""

    best = current

    distance = levenshtein target current
  in
    ({
      target = target
    , state = Running
    , current = current
    , best = (best, distance)
    , trackRecord =
        {
          best = best
        , bestDistance = distance
        , current = current
        , currentDistance = distance
        }
    , seed = initialSeed 0
    }
    , Cmd.none)

-- UPDATE


type Message =
    Mutate
  | Toggle
  | Restart
  | DoNothing


update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Mutate ->
      if model.state == Running then
        let
          trackRecord = model.trackRecord

          currentBest = trackRecord.best

          currentBestDistance = trackRecord.bestDistance

          (nextCurrent, seed') = step (mutate trackRecord.best) model.seed

          nextCurrentDistance = levenshtein model.target nextCurrent

          distance = min currentBestDistance nextCurrentDistance

          nextState =
            if distance == 0 then
              Finished
            else
              model.state

          nextBest =
            if nextCurrentDistance < currentBestDistance then
              (nextCurrent, nextCurrentDistance)
            else
              (currentBest, currentBestDistance)

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
          ({ model
          | current = nextCurrent
          , state = nextState
          , best = nextBest
          , trackRecord = nextTrackRecord
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
      init model.target

    _ ->
      (model, Cmd.none)


-- VIEW


view : Model -> Html Message
view model =
  let
    trackRecord = model.trackRecord

    best = trackRecord.best

    bestDistance = trackRecord.bestDistance

    current = trackRecord.current

    currentDistance = trackRecord.currentDistance
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

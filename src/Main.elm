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

          (currentBest, currentDistance) = model.best

          (next, seed') = step (mutate trackRecord.best) model.seed

          nextDistance = levenshtein model.target next

          distance = min currentDistance nextDistance

          nextState =
            if distance == 0 then
              Finished
            else
              model.state

          best =
            if nextDistance < currentDistance then
              (next, nextDistance)
            else
              model.best

          nextTrackRecord =
            {
              best = (fst best)
            , bestDistance = (snd best)
            , current = next
            , currentDistance = nextDistance
            }
        in
          ({ model
          | current = next
          , state = nextState
          , best = best
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
    best = (fst model.best)

    distance = (snd model.best)
  in
    div []
    [
      div [] [
        text "current:"
      , code []
          [
            text "\""
          , text model.current
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
        text <| toString <| (levenshtein model.target model.current)
      , text " "
      , text <| toString <| distance
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

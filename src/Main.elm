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

type alias Model =
  {
    target : String
  , state : State
  , current : String
  , best : String
  , distance : Int
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
    , best = best
    , distance = distance
    , seed = initialSeed 0
    }
    , Cmd.none)

-- UPDATE


type Message =
    Mutate
  | Toggle
  | DoNothing


update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Mutate ->
      if model.state == Running then
        let
          (next, seed') = step (mutate model.best) model.seed

          nextDistance = levenshtein model.target next

          distance = min model.distance nextDistance

          best =
            if nextDistance < model.distance then
              next
            else
              model.best
        in
          ({ model
          | current = next
          , distance = distance
          , best = best
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
    _ ->
      (model, Cmd.none)


-- VIEW


view : Model -> Html Message
view model =
  let
    distance = levenshtein model.target model.current
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
          , text model.best
          , text "\""
          ]
      ]
    , div [ class "debug" ]
      [
        text <| toString <| (levenshtein model.target model.current)
      , text " "
      , text <| toString <| model.distance
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

    _ -> DoNothing

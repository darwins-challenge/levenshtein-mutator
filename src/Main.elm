import Html exposing (Html, div, text, code, button)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Random exposing (Seed, step, initialSeed)
import Time exposing (Time, millisecond)
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


type alias Model =
  {
    target : String
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
    , current = current
    , best = best
    , distance = distance
    , seed = initialSeed 0
    }
    , Cmd.none)

-- UPDATE


type Message =
    Mutate 
  | DoNothing


update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Mutate ->
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
  Time.every (100 * millisecond) (\_ -> Mutate)

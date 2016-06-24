import Html exposing (Html, div, text, code, button)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random exposing (Seed, step, initialSeed)
import Levenshtein exposing (levenshtein)
import Mutate exposing (mutate)

main : Program Never
main =
  beginnerProgram
  {
    model = init "ABBA"
  , update = update
  , view = view
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


init : String -> Model
init target =
  let
    current = ""

    best = current

    distance = levenshtein target current
  in
    {
      target = target
    , current = current
    , best = best
    , distance = distance
    , seed = initialSeed 0
    }

-- UPDATE


type Message =
    Mutate
  | DoNothing


update : Message -> Model -> Model
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
        { model
          | current = next
          , distance = distance
          , best = best
          , seed = seed' }
    _ ->
      model


-- VIEW


view : Model -> Html Message
view model =
  let
    distance = levenshtein model.target model.current
  in
    div []
    [
      button [ onClick Mutate ] [ text "Mutate" ]
    , div [] [
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

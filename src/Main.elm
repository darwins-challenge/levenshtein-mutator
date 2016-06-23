import Html exposing (Html, div, text, code)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (class)
import Random exposing (Seed, step, initialSeed)
import Levenshtein exposing (levenshtein)
import Mutate exposing (mutate)

main : Program Never
main =
  beginnerProgram
  {
    model = model
  , update = update
  , view = view
  }


-- MODEL


type alias Model =
  {
    target : String
  , current : String
  , seed : Seed
  }


model : Model
model =
  {
    target = "Hello, World!"
  , current = ""
  , seed = initialSeed 0
  }


-- UPDATE


type Message =
  DoNothing


update : Message -> Model -> Model
update message model =
  model


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
      ]
    , div [ class "debug" ]
      [
        text <| toString <| (levenshtein model.target model.current)
      ]
    ]

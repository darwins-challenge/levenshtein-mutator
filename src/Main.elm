import Html exposing (Html, text)
import Html.App exposing (beginnerProgram)
import Random exposing (Seed, step, initialSeed)
import Levenshtein exposing (levenshtein)

u : String
u = "kangaroo"

v : String
v = "koala"

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
  text <| toString <| levenshtein model.target model.current

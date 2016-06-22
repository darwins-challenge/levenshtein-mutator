import Html exposing (Html, text)
import Levenshtein exposing (levenshtein)

u : String
u = "kangaroo"

v : String
v = "koala"

main = text <| toString <| levenshtein u v

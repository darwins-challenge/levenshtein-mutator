module Levenshtein exposing (levenshtein)

import String exposing (uncons, length)

levenshtein : String -> String -> Int
levenshtein u v =
  case uncons u of
    Just (uc, tailu) ->
         case uncons v of
           Nothing -> length u
           Just (vc, tailv) ->
                let
                  edit_cost =
                    if uc == vc then
                      0
                    else
                      1
                in
                  minimum
                    ((levenshtein tailu v) + 1)
                    ((levenshtein u tailv) + 1)
                    ((levenshtein tailu tailv) + edit_cost)
    Nothing ->
        length v


minimum : Int -> Int -> Int -> Int
minimum a b c =
  if a < b then
    if a < c then
      a
    else
      c
  else
    if b < c then
      b
    else
      c


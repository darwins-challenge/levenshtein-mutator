module Mutate exposing (mutate)

import Random exposing (Generator, Seed, int, map, andThen, pair)
import String exposing (length, isEmpty, left, dropLeft, concat, fromChar)
import Char exposing (fromCode)


type Mutation =
    Insert
  | Delete
  | Edit


mutation : Generator Mutation
mutation =
  let
    chooseMutation : Int -> Mutation
    chooseMutation n =
      case n `rem` 3 of
        0 -> Insert
        1 -> Delete
        _ -> Edit
  in
    map chooseMutation (int 0 2)


upper : Generator Char
upper =
  let
    char : Int -> Int -> Generator Char
    char low high =
      map fromCode (int low high)
  in
    char 65 90

identity : String -> Generator String
identity word =
  map (\_ -> word) (int 0 1)


inserter : String -> Generator String
inserter word =
  let
    insertAt : (Char, Int) -> String
    insertAt (character, n) =
      concat
        [
          left n word
        , fromChar character
        , dropLeft n word
        ]

    insert : (Char, Int) -> Generator String
    insert data =
      identity (insertAt data)
  in
    pair upper (int 0 (length word)) `andThen` insert


deleter : String -> Generator String
deleter word =
  let
    deleteAt : Int -> String
    deleteAt n =
      concat
        [
          left (n - 1) word
        , dropLeft n word
        ]

    delete : Int -> Generator String
    delete n =
      if (isEmpty word) then
        identity word
      else
        identity (deleteAt n)
  in
    (int 0 ((length word) - 1)) `andThen` delete


editer : String -> Generator String
editer word =
  let
    replaceAt : (Char, Int) -> String
    replaceAt (character, n) =
      concat
        [
          left (n - 1) word
        , fromChar character
        , dropLeft n word
        ]

    replace : (Char, Int) -> Generator String
    replace data =
        identity (replaceAt data)
  in
    pair upper (int 0 ((length word) - 1)) `andThen` replace



mutate : String -> Generator String
mutate word =
  let
    selectMutator : Mutation -> Generator String
    selectMutator mutation =
      case mutation of
        Insert -> (inserter word)

        Delete -> (deleter word)

        Edit -> (editer word)
  in
    mutation `andThen` selectMutator

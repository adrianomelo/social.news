module Topic exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput, onClick)

main : Program Never
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

init : String -> (List String) -> Model
init name search_terms =
  { name = name
  , search_terms = search_terms
  , temp = ""
  }

-- MODEL

type alias Model =
  { name : String
  , search_terms : List String
  , temp : String
  }

model : Model
model =
  Model "Futebol" [ "#barca", "Real Madrid" ] ""

-- UPDATE

type Msg
    = Name String
    | AddTerm
    | ModifyNewTerm String
    | RemoveTerm String
    | SearchTerms (List String)

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    ModifyNewTerm term ->
      { model | temp = term }

    AddTerm ->
      { model
      | search_terms = model.search_terms ++ [ model.temp ]
      , temp = ""
      }

    RemoveTerm term ->
      { model | search_terms = List.filter (\item -> item /= term) model.search_terms }

    SearchTerms search_terms ->
      { model | search_terms = search_terms }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ ] [ text model.name ]
    , viewSearchTerms model.search_terms
    , input [ onInput ModifyNewTerm ] [ text "Add" ]
    , button [ onClick AddTerm ] [ text "Add" ]
    ]

viewSearchTerm : String -> Html Msg
viewSearchTerm search_term =
  Html.li [ ] [ text search_term
    , button [ onClick (RemoveTerm search_term) ] [ text "Remove " ]
    ]

viewSearchTerms : (List String) -> Html Msg
viewSearchTerms search_terms = 
  Html.ul [ ] (List.map viewSearchTerm search_terms)

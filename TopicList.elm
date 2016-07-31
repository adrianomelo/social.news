import Topic
import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (disabled, placeholder, href)

main : Program Never
main =
  App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }

-- MODEL

type alias Model =
  { topics : List IndexedTopic
  , uid : Int
  , newTopicName : String
  }

type alias IndexedTopic =
  { id : Int
  , model : Topic.Model
  }

init : Model
init =
  { topics = []
  , uid = 0
  , newTopicName = ""
  }

-- UPDATE

type Msg
  = Insert
  | RemoveTopic Int
  | UpdateNewTopicName String
  | Modify Int Topic.Msg

update : Msg -> Model -> Model
update message ({topics, uid} as model) =
  case message of
    Insert ->
      { model
      | uid = uid + 1
      , topics = topics ++ [ IndexedTopic uid (Topic.init model.newTopicName []) ]
      }

    RemoveTopic id ->
      { model
      | topics = List.filter (\m -> m.id /= id) topics 
      }

    Modify id msg ->
      { model | topics = List.map (updateHelp id msg) topics }

    UpdateNewTopicName name ->
      { model | newTopicName = name }

updateHelp : Int -> Topic.Msg -> IndexedTopic -> IndexedTopic
updateHelp targetId msg {id, model} =
  IndexedTopic id (if targetId == id then Topic.update msg model else model)

-- VIEW

view : Model -> Html Msg
view model =
  let
    isNewTopicNameUsed =
      List.any (\item -> item.model.name == model.newTopicName) model.topics

    insert =
      button [ disabled isNewTopicNameUsed, onClick Insert ] [ text "Add" ]

    topicName =
      input [ onInput UpdateNewTopicName, placeholder "Topic name" ] [ ]

    topics =
      List.map viewIndexedTopic model.topics
  in
    div [] ([topicName, insert] ++ topics)

viewIndexedTopic : IndexedTopic -> Html Msg
viewIndexedTopic {id, model} =
  div []
  [ App.map (Modify id) (Topic.view model)
  , button [ onClick (RemoveTopic id) ] [ text ("Remove " ++ model.name)]
  ]

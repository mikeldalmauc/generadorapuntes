module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Time exposing (Posix)


type alias FrontendModel =
    { key : Key
    , text : String
    , selectedText : Maybe String
    , feedback : Maybe SimpleFeedback
    , comment : String
    , feedbacks : List FeedbackEntry
    }

type alias BackendModel =
    { feedbacks : List FeedbackEntry
    }

type alias FeedbackEntry =
    { selectedText : String
    , timestamp : Posix
    , simpleFeedback : SimpleFeedback
    , comment : Maybe String
    }

type SimpleFeedback
    = Like
    | Dislike
    | Neutral

type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    
    | MouseUp
    | GotSelection String
    | SetSimpleFeedback SimpleFeedback
    | UpdateComment String
    | SubmitFeedback
    | SendFeedback FeedbackEntry
    | TriggerTooltip String
    
type ToBackend
    = NoOpToBackend
    | AddFeedback FeedbackEntry
    | RequestFeedbacks
    
type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | UpdateFeedbacks (List FeedbackEntry)
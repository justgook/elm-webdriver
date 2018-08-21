module WebDriver.Internal.HttpHelper exposing (delete, stringifyError, toTask)

import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)


toTask : Http.Request a -> Task String a
toTask =
    Http.toTask >> Task.mapError stringifyError


delete : String -> Decoder a -> Http.Request a
delete url decoder =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


stringifyError : Http.Error -> String
stringifyError error =
    case error of
        Http.BadUrl url ->
            "I was expecting a valid URL, but I got the url: " ++ url

        Http.Timeout ->
            "It took too long to get a response from the server!"

        Http.NetworkError ->
            "Unable to make a connection. Is your network working?"

        Http.BadStatus response ->
            responseToEnglish response

        Http.BadPayload errorMessage response ->
            "I failed because of the following error: "
                ++ errorMessage
                ++ " and "
                ++ responseToEnglish response


responseToEnglish : Http.Response String -> String
responseToEnglish response =
    "I tried to connect to "
        ++ response.url
        ++ " but the response gave me the error code: "
        ++ String.fromInt response.status.code
        ++ " which is known as: \""
        ++ response.status.message
        ++ "\"."

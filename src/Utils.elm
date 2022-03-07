--вынес запросы в отдельный модуль ибо его нужно вызывать в todos 
--которые вызываются в мэйне и получалось зацикливание вызовово
module Utils exposing (RecordWithId, delete, mergeById, patchJson, postJson, removeById)

import Http
import Json.Decode
import Json.Encode
import Task

type alias RecordWithId a = { a | id : Int }

mergeById : List (RecordWithId a) -> RecordWithId a -> List (RecordWithId a)
mergeById existing new =
    let merger = \candidate ( ffound, els ) ->
                if new.id == candidate.id then ( True, new :: els )
                else ( ffound, candidate :: els )

        ( found, coalesced ) = List.foldl merger ( False, [] ) existing

        newListReversed =
            if found then coalesced
            else new :: coalesced
    in List.reverse newListReversed


removeById : List (RecordWithId a) -> RecordWithId a -> List (RecordWithId a)
removeById existing target =
    let filterFn = \a b -> a.id /= b.id
    in List.filter (filterFn target) existing

postJson : Json.Decode.Decoder value -> String -> Json.Encode.Value -> Platform.Task Http.Error value
postJson decoder url json =
    let body = Http.stringBody "application/json" (Json.Encode.encode 0 json)
    in Http.toTask (Http.post url body decoder)

patchJson : Json.Decode.Decoder value -> String -> Json.Encode.Value -> Platform.Task Http.Error value
patchJson decoder url json =
    let body = Http.stringBody "application/json" (Json.Encode.encode 0 json)
        request =
            Http.request
                { method = "PATCH"
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson decoder
                , timeout = Maybe.Nothing
                , withCredentials = False
                }
    in Http.toTask request

delete : a -> String -> Platform.Task Http.Error a
delete a url =
    let decoder = Json.Decode.succeed a
        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson decoder
                , timeout = Maybe.Nothing
                , withCredentials = False
                }
    in Http.toTask request
module ApiTypes exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


import Time exposing(Posix)
import Date exposing(Date)
import MyApiDecoder exposing(..)


type alias AccountId  = Int

jsonDecAccountId : Json.Decode.Decoder ( AccountId )
jsonDecAccountId =
    Json.Decode.int

jsonEncAccountId : AccountId -> Value
jsonEncAccountId  val = Json.Encode.int val



type alias ApiAccount  =
   { id: AccountId
   , name: String
   , age: (Maybe Int)
   , type_: AccountType
   }

jsonDecApiAccount : Json.Decode.Decoder ( ApiAccount )
jsonDecApiAccount =
   Json.Decode.succeed (\pid pname page ptype -> {id = pid, name = pname, age = page, type_ = ptype})
   |> required "id" (jsonDecAccountId)
   |> required "name" (Json.Decode.string)
   |> fnullable "age" (Json.Decode.int)
   |> required "type" (jsonDecAccountType)

jsonEncApiAccount : ApiAccount -> Value
jsonEncApiAccount  val =
   Json.Encode.object
   [ ("id", jsonEncAccountId val.id)
   , ("name", Json.Encode.string val.name)
   , ("age", (maybeEncode (Json.Encode.int)) val.age)
   , ("type", jsonEncAccountType val.type_)
   ]



type AccountType  =
    AccountTypeUser 
    | AccountTypeAdmin 

jsonDecAccountType : Json.Decode.Decoder ( AccountType )
jsonDecAccountType = 
    let jsonDecDictAccountType = Dict.fromList [("AccountTypeUser", AccountTypeUser), ("AccountTypeAdmin", AccountTypeAdmin)]
    in  decodeSumUnaries "AccountType" jsonDecDictAccountType

jsonEncAccountType : AccountType -> Value
jsonEncAccountType  val =
    case val of
        AccountTypeUser -> Json.Encode.string "AccountTypeUser"
        AccountTypeAdmin -> Json.Encode.string "AccountTypeAdmin"


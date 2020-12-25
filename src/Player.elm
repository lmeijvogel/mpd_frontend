module Player exposing (..)

import Url


type alias Player =
    { ip : String
    , name : String
    }


fromUrl : List Player -> Url.Url -> Maybe Player
fromUrl playerList url =
    let
        fragment =
            url.fragment |> Maybe.withDefault ""
    in
    List.filter (\el -> fragment == el.name) playerList |> List.head


findByIp : List Player -> String -> Maybe Player
findByIp players ip =
    List.filter (\p -> p.ip == ip) players |> List.head


findByNameCaseInsensitive : List Player -> String -> Maybe Player
findByNameCaseInsensitive players name =
    let
        lowerQuery =
            String.toLower name
    in
    findByFn players (\p -> String.toLower p.name == lowerQuery)


findByFn : List Player -> (Player -> Bool) -> Maybe Player
findByFn players fn =
    List.filter fn players |> List.head

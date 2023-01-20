module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Attribute, Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (on, onMouseDown, onMouseOver)
import Json.Decode as D
import Set exposing (Set)


type alias Cordinate =
    ( Int, Int )


type alias Model =
    { pressed : Maybe Cordinate
    , overCordinate : Maybe Cordinate
    , selectedSet : Set Cordinate
    , mode : SelectionMode
    }


type SelectionMode
    = Deselection
    | Selection


type Msg
    = Pressed Cordinate
    | Released
    | OverCordinate Cordinate


init : a -> ( Model, Cmd Msg )
init _ =
    ( { pressed = Nothing
      , mode = Selection
      , selectedSet = Set.empty
      , overCordinate = Nothing
      }
    , Cmd.none
    )


getCordinatesBetween : Cordinate -> Cordinate -> List Cordinate
getCordinatesBetween ( x1, y1 ) ( x2, y2 ) =
    let
        yRange =
            List.range (min y1 y2) (max y1 y2)
    in
    List.range (min x1 x2) (max x1 x2)
        |> List.concatMap
            (\x ->
                yRange
                    |> List.map (Tuple.pair x)
            )


applySelectionMode : SelectionMode -> Cordinate -> Set Cordinate -> Set Cordinate
applySelectionMode mode cordinate set =
    case mode of
        Selection ->
            set |> Set.insert cordinate

        Deselection ->
            set |> Set.remove cordinate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed cordinate ->
            let
                mode =
                    if Set.member cordinate model.selectedSet then
                        Deselection

                    else
                        Selection
            in
            ( { model
                | pressed = Just cordinate
                , mode = mode
                , selectedSet =
                    model.selectedSet |> applySelectionMode mode cordinate
              }
            , Cmd.none
            )

        OverCordinate cordinate ->
            ( { model
                | selectedSet =
                    model.pressed
                        |> Maybe.map
                            (\pressed ->
                                getCordinatesBetween pressed cordinate
                                    |> List.foldl (applySelectionMode model.mode) model.selectedSet
                            )
                        |> Maybe.withDefault model.selectedSet
              }
            , Cmd.none
            )

        Released ->
            ( { model
                | pressed = Nothing
                , mode = Selection
              }
            , Cmd.none
            )


addMouseOverIfPressed : Maybe a -> Cordinate -> List (Attribute Msg) -> List (Attribute Msg)
addMouseOverIfPressed maybePressed cordinate ls =
    maybePressed
        |> Maybe.map (\_ -> onMouseOver (OverCordinate cordinate) :: ls)
        |> Maybe.withDefault ls


makeRow : Model -> Int -> Html Msg
makeRow model rowCord =
    tr []
        (td [] []
            :: (List.range 0 6
                    |> List.map
                        (\colCord ->
                            let
                                cordinate =
                                    ( rowCord, colCord )

                                attrs =
                                    [ onMouseDown (Pressed cordinate)
                                    , classList [ ( "selected", model.selectedSet |> Set.member ( rowCord, colCord ) ) ]
                                    ]
                                        |> addMouseOverIfPressed model.pressed cordinate
                            in
                            td attrs
                                [ text (String.fromInt rowCord ++ ", " ++ String.fromInt colCord) ]
                        )
               )
        )


view : Model -> Html Msg
view model =
    div [ class "m-10" ]
        [ table [ class "user-select-none" ]
            [ thead []
                [ th [] [ text "" ]
                , th [] [ text "Sun" ]
                , th [] [ text "Mon" ]
                , th [] [ text "Tue" ]
                , th [] [ text "Wed" ]
                , th [] [ text "Thu" ]
                , th [] [ text "Fri" ]
                , th [] [ text "Sat" ]
                ]
            , tbody []
                (List.range 0 7
                    |> List.map (makeRow model)
                )
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    model.pressed
        |> Maybe.map (always <| Browser.Events.onMouseUp (D.succeed Released))
        |> Maybe.withDefault Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

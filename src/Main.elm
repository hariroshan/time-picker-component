module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onClick, onMouseDown, onMouseOver)
import Json.Decode as D
import List.Extra
import Set exposing (Set)
import Task
import Time
import TimeUtils


type alias Cordinate =
    ( Int, Int )


type alias Model =
    { pressed : Maybe Cordinate
    , overCordinate : Maybe Cordinate
    , selectedSet : Set Cordinate
    , mode : SelectionMode

    -- Time series
    , daySeries : List Time.Posix
    , timeSeries : Dict ( Int, Int ) Time.Posix
    , timeSeriesLength : Int

    -- Time
    , zone : Time.Zone
    , now : Maybe Time.Posix
    }


type SelectionMode
    = Deselection
    | Selection


type Msg
    = Pressed Cordinate
    | Released
    | OverCordinate Cordinate
    | Clicked Cordinate
    | GotTimeAndZone ( Time.Posix, Time.Zone )


init : a -> ( Model, Cmd Msg )
init _ =
    ( { pressed = Nothing
      , mode = Selection
      , selectedSet = Set.empty
      , overCordinate = Nothing
      , zone = Time.utc
      , now = Nothing
      , daySeries = []
      , timeSeries = Dict.empty
      , timeSeriesLength = 7
      }
    , Cmd.batch
        [ Task.map2 Tuple.pair Time.now Time.here
            |> Task.perform GotTimeAndZone
        ]
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


daySeriesRange : List Int
daySeriesRange =
    List.range 1 7


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeAndZone ( now, zone ) ->
            let
                cleaned =
                    now
                        |> TimeUtils.removeMillis zone
                        |> TimeUtils.removeSeconds zone
                        |> TimeUtils.removeMinutes zone
                        |> TimeUtils.removeHours zone

                daySeries =
                    daySeriesRange
                        |> List.foldr
                            (\cur acc ->
                                (cleaned |> TimeUtils.addDays cur) :: acc
                            )
                            []

                timeSeriesForDaySeries =
                    daySeries
                        |> List.map TimeUtils.generatePosixList

                lengthOfTimeSeries : Int
                lengthOfTimeSeries =
                    timeSeriesForDaySeries
                        |> List.head
                        |> Maybe.map List.length
                        |> Maybe.withDefault 7

                timeSeriesDict =
                    timeSeriesForDaySeries
                        |> List.foldl
                            (\cur ( row, acc ) ->
                                ( row + 1
                                , cur
                                    |> List.foldl
                                        (\time ( col, dict ) ->
                                            ( col + 1
                                            , dict
                                                |> Dict.insert ( col, row ) time
                                            )
                                        )
                                        ( 1, acc )
                                    |> Tuple.second
                                )
                            )
                            ( 1, model.timeSeries )
                        |> Tuple.second
            in
            ( { model
                | zone = zone
                , now = Just cleaned
                , daySeries = daySeries
                , timeSeriesLength = lengthOfTimeSeries
                , timeSeries = timeSeriesDict
              }
            , Cmd.none
            )

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

        Clicked cordinate ->
            model
                |> update (Pressed cordinate)
                |> Tuple.first
                |> update Released


addOverEventsIfPressed : Maybe a -> Cordinate -> List (Attribute Msg) -> List (Attribute Msg)
addOverEventsIfPressed maybePressed cordinate ls =
    maybePressed
        |> Maybe.map
            (\_ ->
                onMouseOver (OverCordinate cordinate)
                    :: ls
            )
        |> Maybe.withDefault ls


makeRow : Model -> Int -> List (Html Msg)
makeRow model rowCord =
    let
        timeOnRow =
            model.timeSeries
                |> Dict.get ( rowCord, 1 )
                |> Maybe.map (TimeUtils.timeFormat model.zone)
                |> Maybe.withDefault ""
    in
    div [ class "bg-white flex items-center justify-center text-sm leading-6 text-gray-500" ] [ text timeOnRow ]
        :: (daySeriesRange
                |> List.map
                    (\colCord ->
                        let
                            cordinate =
                                ( rowCord, colCord )

                            attrs =
                                [ onMouseDown (Pressed cordinate)
                                , class
                                    (if model.selectedSet |> Set.member ( rowCord, colCord ) then
                                        "brand-blue"

                                     else
                                        "bg-white"
                                    )
                                , class "w-full h-8 text-center"
                                ]
                                    |> addOverEventsIfPressed model.pressed cordinate
                        in
                        --text (String.fromInt rowCord ++ ", " ++ String.fromInt colCord)
                        button attrs []
                    )
           )


showTimeGrid : Model -> Html Msg
showTimeGrid model =
    div [ class "select-none min-w-2xl" ]
        [ div [ class "grid grid-cols-8 text-center text-xs leading-6 text-gray-500" ]
            (div [] []
                :: (model.daySeries
                        |> List.map
                            (\posix ->
                                div [ class "flex flex-col justify-center items-center" ]
                                    [ div [ class "text-lg font-medium" ]
                                        [ Time.toWeekday model.zone posix
                                            |> TimeUtils.weekdayToString
                                            |> text
                                        ]
                                    , div [ class "text-sm flex justify-center items-center space-x-2" ]
                                        [ span [] [ Time.toDay model.zone posix |> String.fromInt |> text ]
                                        , span [] [ Time.toMonth model.zone posix |> TimeUtils.monthToString |> text ]
                                        ]
                                    ]
                            )
                   )
            )
        , div [ class "mt-2 grid grid-cols-8 gap-px bg-gray-200 text-sm border border-gray-200" ]
            (List.range 1 model.timeSeriesLength
                |> List.concatMap (makeRow model)
            )
        ]


groupSelectedByDay : Time.Zone -> Dict Cordinate Time.Posix -> Set Cordinate -> Dict Int (List Time.Posix)
groupSelectedByDay zone timeSeries selectedSet =
    selectedSet
        |> Set.foldl
            (\cur acc ->
                timeSeries
                    |> Dict.get cur
                    |> Maybe.map
                        (\posix ->
                            let
                                key =
                                    Time.toWeekday zone posix
                                        |> TimeUtils.weekdayToInt
                            in
                            if Dict.member key acc then
                                acc
                                    |> Dict.update key (Maybe.map ((::) posix))

                            else
                                acc |> Dict.insert key [ posix ]
                        )
                    |> Maybe.withDefault acc
            )
            Dict.empty


sortPosixValuesInDict : Dict Int (List Time.Posix) -> Dict Int (List Time.Posix)
sortPosixValuesInDict =
    Dict.foldl
        (\key value acc ->
            acc
                |> Dict.insert key (List.sortBy Time.posixToMillis value)
        )
        Dict.empty


combineTimePosixValues : Time.Zone -> List Time.Posix -> List String
combineTimePosixValues zone ls =
    let
        ( _, result ) =
            ls
                |> List.foldl
                    (\cur ( maybePrevTime, acc ) ->
                        case maybePrevTime of
                            Nothing ->
                                ( Just cur, [ cur ] :: acc )

                            Just prevTime ->
                                if TimeUtils.isIncrementOf30Minutes prevTime cur then
                                    ( Just cur
                                    , acc
                                        |> List.head
                                        |> Maybe.map ((::) cur)
                                        |> Maybe.map (\timings -> timings :: List.drop 1 acc)
                                        |> Maybe.withDefault acc
                                    )

                                else
                                    ( Just cur, [ cur ] :: acc )
                    )
                    ( Nothing, [] )
    in
    result
        |> List.reverse
        |> List.map
            (\timings ->
                case ( List.Extra.last timings, List.head timings ) of
                    ( Just start, Just end ) ->
                        if start == end then
                            TimeUtils.timeFormat zone start

                        else
                            TimeUtils.timeFormat zone start
                                ++ "-"
                                ++ TimeUtils.timeFormat zone end

                    _ ->
                        ""
            )


groupIncrementalTimeTogether : Time.Zone -> Dict Int (List Time.Posix) -> List ( Int, String )
groupIncrementalTimeTogether zone dict =
    dict
        |> Dict.foldl
            (\key value acc ->
                ( key, combineTimePosixValues zone value |> String.join ", " )
                    :: acc
            )
            []


showSelectedTimings : Model -> Html Msg
showSelectedTimings model =
    case model.pressed of
        Nothing ->
            let
                grouped =
                    model.selectedSet
                        |> groupSelectedByDay model.zone model.timeSeries
                        |> sortPosixValuesInDict
                        |> groupIncrementalTimeTogether model.zone
                        |> List.sortBy Tuple.first
                        |> List.map (Tuple.mapFirst TimeUtils.intToWeekday)
            in
            div [ class "mt-6 space-y-2" ]
                (grouped
                    |> List.map
                        (\( weekDay, timings ) ->
                            div [ class "flex items-center space-x-2" ]
                                [ div [ class "text-lg leading-6 text-gray-500" ] [ text (TimeUtils.weekdayToString weekDay) ]
                                , div [ class "text-sm leading-6 text-gray-700" ] [ text timings ]
                                ]
                        )
                )

        Just _ ->
            text ""


view : Model -> Html Msg
view model =
    case model.now of
        Nothing ->
            text ""

        Just _ ->
            div [ class "m-10 flex justify-between" ]
                [ showTimeGrid model
                , showSelectedTimings model
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

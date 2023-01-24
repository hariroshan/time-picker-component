module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onMouseDown, onMouseOver)
import Json.Decode as D
import List.Extra
import Set exposing (Set)
import Task
import Time
import TimeUtils


{-| Cordinate of the grid
-}
type alias Cordinate =
    ( Int, Int )


type alias Model =
    { -- Used to store the grid cordinate when a grid is pressed
      pressed : Maybe Cordinate

    -- Used to store all the selected grid cordinate
    , selectedSet : Set Cordinate

    -- Used to determine the selection mode based on pressed grid cordinate
    , mode : SelectionMode

    -- Time related
    -- Used to render the day series
    , daySeries : List Time.Posix

    -- Used to store the grid cordinate to time dict.
    -- The cordinate can be used to fetch the time
    , timeSeries : Dict ( Int, Int ) Time.Posix

    -- Used to render the time grid rows
    , timeSeriesLength : Int

    -- Timezone of the user
    , zone : Time.Zone

    -- Cleaned (removed millis, seconds, minutes, hours from posix) series starting time
    , startingTime : Maybe Time.Posix
    , now : Time.Posix
    }


type SelectionMode
    = Deselection
    | Selection


type Msg
    = Pressed Cordinate
    | OverCordinate Cordinate
    | Released
    | LoadTimeSeries Bool
    | GotTimeAndZone ( Time.Posix, Time.Zone )


init : a -> ( Model, Cmd Msg )
init _ =
    ( { pressed = Nothing
      , mode = Selection
      , selectedSet = Set.empty
      , zone = Time.utc
      , startingTime = Nothing
      , daySeries = []
      , timeSeries = Dict.empty
      , timeSeriesLength = 0
      , now = Time.millisToPosix 0
      }
    , Cmd.batch
        [ Task.map2 Tuple.pair Time.now Time.here
            |> Task.perform GotTimeAndZone
        ]
    )


daySeriesLength : Int
daySeriesLength =
    7


timeIncrement : Int
timeIncrement =
    30


empty : Html msg
empty =
    text ""


daySeriesRange : List Int
daySeriesRange =
    List.range 1 daySeriesLength


{-| Gets cordinates between 2 points.
-}
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


{-| Removes or inserts cordinate based on selection mode
-}
applySelectionMode : SelectionMode -> Cordinate -> Set Cordinate -> Set Cordinate
applySelectionMode mode cordinate set =
    case mode of
        Selection ->
            set |> Set.insert cordinate

        Deselection ->
            set |> Set.remove cordinate


{-| Calculates day ranges and time ranges for each day range
-}
calculateAndSetTimeSeries : Time.Posix -> Model -> Model
calculateAndSetTimeSeries current model =
    let
        daySeries =
            -- Generates day series posix
            daySeriesRange
                |> List.foldr
                    (\cur acc ->
                        (current |> TimeUtils.addDays cur) :: acc
                    )
                    []

        timeSeriesForDaySeries =
            -- Generates time increment posix for each day
            daySeries
                |> List.map (TimeUtils.generatePosixList timeIncrement)

        lengthOfTimeSeries : Int
        lengthOfTimeSeries =
            -- Calculates length for rendering time grid
            timeSeriesForDaySeries
                |> List.head
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        timeSeriesDict =
            -- Creates a cordinate to time lookup dict
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
                    ( 1, Dict.empty )
                |> Tuple.second
    in
    { model
        | startingTime = Just current
        , daySeries = daySeries
        , timeSeriesLength = lengthOfTimeSeries
        , timeSeries = timeSeriesDict
        , selectedSet = Set.empty
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- when pressed, we'll determine the mode of selection and add the cordinate to the selection set accordingly
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
                    model.selectedSet
                        |> applySelectionMode mode cordinate
              }
            , Cmd.none
            )

        -- OverCordinate msg is called only when pressed. We will use this cordinate and calculate the region of selection
        -- Based on mode, we will either add or remove cordinates
        OverCordinate cordinate ->
            ( { model
                | selectedSet =
                    model.pressed
                        |> Maybe.map
                            (\pressed ->
                                getCordinatesBetween pressed cordinate
                                    |> List.foldl (applySelectionMode model.mode)
                                        model.selectedSet
                            )
                        |> Maybe.withDefault model.selectedSet
              }
            , Cmd.none
            )

        -- When released, we will reset back to original states
        Released ->
            ( { model
                | pressed = Nothing
                , mode = Selection
              }
            , Cmd.none
            )

        -- When user click the arrow, we will generate a new day series based on isNext parameter
        LoadTimeSeries isNext ->
            model.startingTime
                |> Maybe.map
                    (TimeUtils.addDays
                        (if not isNext then
                            -1

                         else
                            1
                        )
                    )
                |> Maybe.map
                    (\next ->
                        ( calculateAndSetTimeSeries next model
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        -- called when we have the current day and zone
        GotTimeAndZone ( now, zone ) ->
            let
                cleaned =
                    now
                        |> TimeUtils.removeMillis zone
                        |> TimeUtils.removeSeconds zone
                        |> TimeUtils.removeMinutes zone
                        |> TimeUtils.removeHours zone
            in
            ( { model
                | zone = zone
                , now = now
              }
                |> calculateAndSetTimeSeries cleaned
            , Cmd.none
            )


{-| Adds mouseover event to grid only when model.pressed is Just
-}
addOverEventIfPressed : Maybe a -> Cordinate -> List (Attribute Msg) -> List (Attribute Msg)
addOverEventIfPressed maybePressed cordinate ls =
    maybePressed
        |> Maybe.map
            (\_ ->
                onMouseOver (OverCordinate cordinate)
                    :: ls
            )
        |> Maybe.withDefault ls


{-| Generates time grid
-}
makeRow : Model -> Int -> List (Html Msg)
makeRow model rowCord =
    let
        timeOnRow =
            model.timeSeries
                |> Dict.get ( rowCord, 1 )
                |> Maybe.map (TimeUtils.timeFormat model.zone)
                |> Maybe.andThen
                    (\format ->
                        if format |> String.contains ":30" then
                            Nothing

                        else
                            Just format
                    )
                |> Maybe.withDefault ""
    in
    div [ class "bg-white flex items-center justify-center text-sm leading-6 text-gray-500", classList [("-mt-[1px]", timeOnRow == "")] ] [ text timeOnRow ]
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
                                        "bg-white hover:bg-gray-100"
                                    )
                                , class "w-full h-6 text-center"
                                ]
                                    |> addOverEventIfPressed model.pressed cordinate
                        in
                        button attrs []
                    )
           )


{-| Renders the top row with date, month and weekday
-}
showDayOfWeekRow : Model -> Html Msg
showDayOfWeekRow model =
    div [ class "relative grid grid-cols-8 text-center text-xs leading-6 text-gray-500" ]
        (div [] []
            :: (model.daySeries
                    |> List.indexedMap
                        (\i posix ->
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
                                , if i == 0 || i == 6 then
                                    button
                                        [ class "absolute transform w-10 h-10 text-lg text-blue-800"
                                        , classList
                                            [ ( "rotate-90 -translate-x-full", i == 0 )
                                            , ( "-rotate-90 translate-x-full", i == 6 )
                                            , ( "hidden", Time.posixToMillis posix < Time.posixToMillis model.now )
                                            ]
                                        , onClick
                                            (LoadTimeSeries (not <| i == 0))
                                        ]
                                        [ text (String.fromChar '▼') ]

                                  else
                                    empty
                                ]
                        )
               )
        )


{-| Renders time grid for clicking
-}
showTimeGrid : Model -> Html Msg
showTimeGrid model =
    div [ class "select-none min-w-2xl" ]
        [ showDayOfWeekRow model
        , div [ class "mt-2 grid grid-cols-8 gap-px bg-gray-200 text-sm border border-gray-200" ]
            (List.range 1 model.timeSeriesLength
                |> List.concatMap (makeRow model)
            )
        ]


{-| Groups the posix from the timeSeries dict by weekday
-}
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


{-| Sorts posix by weekday
-}
sortPosixValuesInDict : Dict Int (List Time.Posix) -> Dict Int (List Time.Posix)
sortPosixValuesInDict =
    Dict.foldl
        (\key value acc ->
            acc
                |> Dict.insert key (List.sortBy Time.posixToMillis value)
        )
        Dict.empty


{-| Groups the posix when they are of increments of timeIncrement
-}
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
                                if TimeUtils.isIncrementOfXMinutes timeIncrement prevTime cur then
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


{-| Groups the time posix values of weekday and joins them
-}
groupIncrementalTimeTogether : Time.Zone -> Dict Int (List Time.Posix) -> List ( Int, String )
groupIncrementalTimeTogether zone dict =
    dict
        |> Dict.foldl
            (\key value acc ->
                ( key, combineTimePosixValues zone value |> String.join ", " )
                    :: acc
            )
            []


{-| Renders the selected timings
-}
showSelectedTimings : Model -> Html Msg
showSelectedTimings model =
    case model.pressed of
        -- Shows when not pressed
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
            empty


view : Model -> Html Msg
view model =
    case model.startingTime of
        Nothing ->
            empty

        Just _ ->
            div [ class "m-10 flex justify-between" ]
                [ showTimeGrid model
                , showSelectedTimings model
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Listening for mouse up event on document so that when it is released outside of grid region, we can clean up
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

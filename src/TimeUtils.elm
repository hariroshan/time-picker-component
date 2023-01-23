module TimeUtils exposing
    ( addDays
    , addMinutes
    , generatePosixList
    , intToWeekday
    , isIncrementOfXMinutes
    , minusDays
    , monthToString
    , removeHours
    , removeMillis
    , removeMinutes
    , removeSeconds
    , timeFormat
    , weekdayToInt
    , weekdayToString
    )

import Time


generatePosixList : Int -> Time.Posix -> List Time.Posix
generatePosixList increment cleanedStartingPosix =
    let
        startingPosix : Time.Posix
        startingPosix =
            cleanedStartingPosix
                |> addMinutes
                    -- starting series at 8am
                    (8 * 60)
    in
    -- This can be passed as parameter too
    List.range 0 (10 * 2)
        |> List.map
            (\multiply ->
                startingPosix
                    -- Increment by increment mins
                    |> addMinutes (increment * multiply)
            )


isIncrementOfXMinutes : Int -> Time.Posix -> Time.Posix -> Bool
isIncrementOfXMinutes x source target =
    addMinutes x source == target


removeMillis : Time.Zone -> Time.Posix -> Time.Posix
removeMillis zone time =
    let
        posixInMillis : Int
        posixInMillis =
            time
                |> Time.posixToMillis

        milliSeconds : Int
        milliSeconds =
            Time.toMillis zone time
    in
    (posixInMillis
        - milliSeconds
    )
        |> Time.millisToPosix


removeSeconds : Time.Zone -> Time.Posix -> Time.Posix
removeSeconds zone time =
    let
        posixInMillis : Int
        posixInMillis =
            time
                |> Time.posixToMillis

        seconds : Int
        seconds =
            Time.toSecond zone time
    in
    (posixInMillis
        - (seconds * 1000)
    )
        |> Time.millisToPosix


removeMinutes : Time.Zone -> Time.Posix -> Time.Posix
removeMinutes zone time =
    let
        posixInMillis : Int
        posixInMillis =
            time
                |> Time.posixToMillis

        minutes : Int
        minutes =
            Time.toMinute zone time
    in
    (posixInMillis
        - (minutes * 1000 * 60)
    )
        |> Time.millisToPosix


removeHours : Time.Zone -> Time.Posix -> Time.Posix
removeHours zone time =
    let
        posixInMillis : Int
        posixInMillis =
            time
                |> Time.posixToMillis

        hours : Int
        hours =
            Time.toHour zone time
    in
    (posixInMillis
        - (hours * 1000 * 60 * 60)
    )
        |> Time.millisToPosix


addDays : Int -> Time.Posix -> Time.Posix
addDays day time =
    (Time.posixToMillis time + (86400000 * day))
        |> Time.millisToPosix


minusDays : Int -> Time.Posix -> Time.Posix
minusDays day time =
    (Time.posixToMillis time - (86400000 * day))
        |> Time.millisToPosix


addMinutes : Int -> Time.Posix -> Time.Posix
addMinutes minutes time =
    (Time.posixToMillis time + (minutes * 1000 * 60))
        |> Time.millisToPosix


weekdayToString : Time.Weekday -> String
weekdayToString week =
    case week of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


weekdayToInt : Time.Weekday -> Int
weekdayToInt week =
    case week of
        Time.Mon ->
            0

        Time.Tue ->
            1

        Time.Wed ->
            2

        Time.Thu ->
            3

        Time.Fri ->
            4

        Time.Sat ->
            5

        Time.Sun ->
            6


intToWeekday : Int -> Time.Weekday
intToWeekday weekNumber =
    case weekNumber of
        0 ->
            Time.Mon

        1 ->
            Time.Tue

        2 ->
            Time.Wed

        3 ->
            Time.Thu

        4 ->
            Time.Fri

        5 ->
            Time.Sat

        _ ->
            Time.Sun


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "Ma"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


timeFormat : Time.Zone -> Time.Posix -> String
timeFormat zone posix =
    let
        hours =
            Time.toHour zone posix

        minutes =
            String.fromInt (Time.toMinute zone posix)
                |> prependZeroIfLessThan 2

        meridiem =
            if hours > 12 then
                "pm"

            else
                "am"

        hoursInString =
            String.fromInt
                (if hours > 12 then
                    hours - 12

                 else
                    hours
                )
                |> prependZeroIfLessThan 2
    in
    hoursInString ++ ":" ++ minutes ++ " " ++ meridiem


prependZeroIfLessThan : Int -> String -> String
prependZeroIfLessThan targetLength target =
    if String.length target == targetLength then
        target

    else
        "0" ++ target

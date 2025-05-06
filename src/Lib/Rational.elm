module Lib.Rational exposing
    ( Rational
    , add
    , div
    , fromInt
    , mul
    , new
    , sub
    , toDecimalString
    , toString
    , zero
    )

import Dict exposing (Dict)


type Rational
    = Rational Int Int


zero : Rational
zero =
    Rational 0 1


fromInt : Int -> Rational
fromInt n =
    Rational n 1


new : Int -> Int -> Maybe Rational
new numer denom =
    if denom == 0 then
        Nothing

    else
        Just (makeRational numer denom)


makeRational : Int -> Int -> Rational
makeRational numer denom =
    let
        divisor =
            gcd numer denom

        g =
            if denom < 0 then
                -divisor

            else
                divisor

        n =
            numer // g

        d =
            denom // g
    in
    Rational n d


gcd : Int -> Int -> Int
gcd a b =
    gcdHelper (abs a) (abs b)


gcdHelper : Int -> Int -> Int
gcdHelper a b =
    if b == 0 then
        a

    else
        gcdHelper b (modBy b a)


add : Rational -> Rational -> Rational
add (Rational n1 d1) (Rational n2 d2) =
    makeRational (n1 * d2 + n2 * d1) (d1 * d2)


sub : Rational -> Rational -> Rational
sub (Rational n1 d1) (Rational n2 d2) =
    makeRational (n1 * d2 - n2 * d1) (d1 * d2)


mul : Rational -> Rational -> Rational
mul (Rational n1 d1) (Rational n2 d2) =
    makeRational (n1 * n2) (d1 * d2)


div : Rational -> Rational -> Rational
div (Rational n1 d1) (Rational n2 d2) =
    if n2 == 0 then
        zero

    else
        makeRational (n1 * d2) (d1 * n2)


toString : Rational -> String
toString (Rational n d) =
    if d == 1 then
        String.fromInt n

    else
        String.fromInt n ++ "/" ++ String.fromInt d


toDecimalString : Rational -> String
toDecimalString (Rational n d) =
    if d == 1 then
        String.fromInt n

    else
        let
            sign =
                if n < 0 then
                    "-"

                else
                    ""

            m =
                abs n

            quotient =
                m // d

            remainder =
                modBy d m
        in
        sign ++ String.fromInt quotient ++ "." ++ decimalRep remainder d


decimalRep : Int -> Int -> String
decimalRep n d =
    decimalRepHelper n d [] Dict.empty


decimalRepHelper : Int -> Int -> List ( Int, Int ) -> Dict ( Int, Int ) ( Int, Int ) -> String
decimalRepHelper n d terms memo =
    case Dict.get ( n, d ) memo of
        Just ( q, r ) ->
            displayRepeating ( q, r ) terms ")"

        Nothing ->
            let
                n10 =
                    n * 10

                q =
                    n10 // d

                r =
                    modBy d n10
            in
            if r == 0 then
                displayTerminating (( q, r ) :: terms) ""

            else
                decimalRepHelper
                    r
                    d
                    (( q, r ) :: terms)
                    (Dict.insert ( n, d ) ( q, r ) memo)


displayTerminating : List ( Int, Int ) -> String -> String
displayTerminating terms output =
    case terms of
        [] ->
            output

        ( q, _ ) :: rest ->
            displayTerminating rest (String.fromInt q ++ output)


displayRepeating : ( Int, Int ) -> List ( Int, Int ) -> String -> String
displayRepeating marker terms output =
    case terms of
        [] ->
            output

        ( q, r ) :: rest ->
            let
                s =
                    if ( q, r ) == marker then
                        "(" ++ String.fromInt q

                    else
                        String.fromInt q
            in
            displayRepeating marker rest (s ++ output)

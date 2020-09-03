module Polynomials exposing (..)


type alias Polynomial number =
    List number


type alias Ratfun number =
    ( Polynomial number, Polynomial number )



-- empty list is treated as 0 polynomial
-- add simplify function to get rid of trailing zeros
-- Create


fromList : List number -> Polynomial number
fromList coefs =
    if List.isEmpty coefs then
        [ 0 ]

    else
        coefs


const : number -> Polynomial number
const x =
    List.singleton x



-- Basic Operations
-- add, subtract, divide, multiply, evaluate


negative : Polynomial number -> Polynomial number
negative p =
    if List.isEmpty p then
        [ 0 ]

    else
        List.map ((*) -1) p


plus : Polynomial number -> Polynomial number -> Polynomial number
plus p q =
    let
        n =
            List.length p

        m =
            List.length q
    in
    if n <= m then
        List.map2 (+) (List.repeat (m - n) 0 ++ p) q

    else
        List.map2 (+) p (List.repeat (m - n) 0 ++ q)


minus : Polynomial number -> Polynomial number -> Polynomial number
minus p q =
    plus p (negative q)


timesConst : number -> Polynomial number -> Polynomial number
timesConst c p =
    if List.isEmpty p then
        [ 0 ]

    else
        List.map ((*) c) p



-- Change this again when we have rational functions defined


shift : Int -> Polynomial number -> Maybe (Polynomial number)
shift n p =
    if n < 0 then
        Nothing

    else
        Just (p ++ List.repeat n 0)


times : Polynomial number -> Polynomial number -> Polynomial number
times p q =
    let
        n =
            List.length p
    in
    case p of
        [] ->
            [ 0 ]

        h :: ta ->
            let
                mult =
                    timesConst h q

                mlead =
                    shift n mult
            in
            case mlead of
                Nothing ->
                    [ 0 ]

                Just lead ->
                    plus lead (times ta q)



-- Calculus
-- derive, integrate
-- Quality of Life
-- To string, get coefficients

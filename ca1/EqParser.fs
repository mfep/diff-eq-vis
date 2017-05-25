module EqParser

open System

type private Token =
| ID of char
| NUM of float
| HAT
| PLUS
| MINUS
| STAR

let private regex s = new Text.RegularExpressions.Regex(s)
let private tokenR = regex @"^((?<token>(\d+\.?\d*|\w|\^|\+|-))\s*)+$"
let private tokenize (s : string) =
    if not (tokenR.IsMatch(s)) then failwith "Invalid input string" else
        [ for x in tokenR.Match(s).Groups.["token"].Captures do
            yield match x.Value with
                    | "^" -> HAT
                    | "+" -> PLUS
                    | "-" -> MINUS
                    | "*" -> STAR
                    | s when Char.IsNumber(s, 0) -> NUM (float s)
                    | "x" -> ID 'x'
                    | "y" -> ID 'y'
                    | _ -> failwith "Unexpected case in tokenize!"
        ]

let private preprocessTokens tokens =
    match List.head tokens with
    | PLUS | MINUS -> tokens
    | _ -> PLUS :: tokens

type private Term = float * (char * float) list
type Polinomial = Term list
type private TokenStream = Token list

let private tryToken (src : TokenStream) =
    match src with
    | tok :: rest -> Some(tok, rest)
    | _ -> None

let private parseExp src =
    match tryToken src with
    | Some (HAT, src) ->
        match tryToken src with
        | Some (NUM x, src) -> x, src
        | _ -> failwith "expected number after '^'"
    | _ -> 1.0, src

let private parseIdExp src =
    match tryToken src with
    | Some (ID id, src) ->
        let exponent, src = parseExp src
        Some(id, exponent), src
    | _ -> None, src

let rec private parseIds ids src =
    match parseIdExp src with
    | Some idtup, src -> parseIds (idtup :: ids) src
    | None, src -> List.rev ids, src

let private signMul = function
| PLUS -> 1.0
| MINUS -> -1.0
| _ -> failwith "not a sign"

let private parseTerm src : (Term * Token list) option =
    match tryToken src with
    | Some (sign, src) ->
        match tryToken src with
        | Some (NUM x, src) ->
            let ids, src = parseIds [] src
            Some ((signMul sign * x, ids), src)
        | Some _ ->
            let ids, src = parseIds [] src
            Some ((signMul sign, ids), src)
        | _ -> failwith "unexpected end of term"
    | None -> None

let rec private parsePolinomial poly src : Polinomial =
    match parseTerm src with
    | Some (term, src) -> parsePolinomial (term :: poly) src
    | None -> List.rev poly

let private calcTerm (x, y) (coeff, vars)  =
    let rec loop res vars =
        match vars with
        | ('x', exponent) :: tl -> loop (res * Math.Pow(x, exponent)) tl
        | ('y', exponent) :: tl -> loop (res * Math.Pow(y, exponent)) tl
        | [] -> res
        | _ -> failwith "unexpected id"
    coeff * loop 1.0 vars

let rec private calcPolinomial sum point (poly : Polinomial) =
    match poly with
    | term :: tl -> calcPolinomial (sum + calcTerm point term) point tl
    | [] -> sum

let parse str =
    str |> tokenize |> preprocessTokens |> parsePolinomial []

let calculate point polinomial =
    calcPolinomial 0.0 point polinomial

let defaultPoly =
    parse "0"

module EqParser

type private Token =
| ID of string
| INT of int
| HAT
| PLUS
| MINUS

let private regex s = new System.Text.RegularExpressions.Regex(s)
let private tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*$"
let private tokenize (s : string) =
    [ for x in tokenR.Match(s).Groups.["token"].Captures do
        let token =
            match x.Value with
            | "^" -> HAT
            | "-" -> MINUS
            | "+" -> PLUS
            | s when System.Char.IsDigit s.[0] -> INT (int s)
            | s -> ID s
        yield token
    ]

type Term =
| Term of int * string * int
| Const of int

type Polinomial = Term list
type private TokenStream = Token list

let private tryToken (src : TokenStream) =
    match src with
    | tok :: rest -> Some(tok, rest)
    | _ -> None

let private parseIndex src =
    match tryToken src with
    | Some (HAT, src) ->
        match tryToken src with
        | Some (INT num2, src) -> num2, src
        | _ -> failwith "expected integer after '^'"
    | _ -> 1, src

let private parseSign src =
    match tryToken src with
    | Some (PLUS, src) -> 1, src
    | Some (MINUS, src) -> -1, src
    | _ -> failwith "missing sign"

let private parseTerm src =
    let sign, src = parseSign src
    match tryToken src with
    | Some (INT num, src) ->
        match tryToken src with
        | Some (ID id, src) ->
            let idx, src = parseIndex src
            Term (num * sign, id, idx), src
        | _ -> Const (num * sign), src
    | Some (ID id, src) ->
        let idx, src = parseIndex src
        Term (sign, id, idx), src
    | _ -> failwith "end of token stream in term"

let private parsePolinomial src =
    let rec loop src terms =
        printfn "%A" src
        match src with
        | [] -> terms
        | src ->
            let t, s = parseTerm src
            loop s (t :: terms)
    loop src [] |> List.rev

let private preprocessTokens tokens =
    match List.head tokens with
    | PLUS | MINUS -> tokens
    | _ -> PLUS :: tokens

let parse input =
    input |> tokenize |> preprocessTokens |> parsePolinomial

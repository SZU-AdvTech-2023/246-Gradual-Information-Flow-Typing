open FSharp.Text.Lexing

let parse str =
    let lexbuf = LexBuffer<char>.FromString str
    let res = Parser.program Lexer.token lexbuf
    res

let printTokens str =
    let lexbuf = LexBuffer<char>.FromString str
    let mutable br = true
    while br do
        match Lexer.token lexbuf with
        | Parser.EOF -> br <- false
        | tk -> printfn "%A" tk

let test str =
    printfn "======================="
    //printTokens str

    let term = parse str
    try
        let (ty, et) = Typing.typing [] term
        printfn "Type:%s" (Syntax.labeledTypeToString ty)
        Evaluation.evaluatePrint et
    with
    | Typing.TypingError str ->
        printfn "Error:%s" str

test """
let age : Int = 42 in
let salary : Int = 58000 in
let intToString : Int -> String = \x : Int. #string x in
let print : String -> Unit = \x : String. #print x in
print (intToString salary)
"""

test """
let age : Int = 42 in
let salary : Int = 58000^H in
let intToString : Int -> String = \x : Int. #string x in
let print : String -> Unit = \x : String. #print (x => String^L) in
print (intToString salary)
"""

test """
let age : Int^L = 42 in
let salary : Int^H = 58000 ! Int^H in
let intToString : Int -> String = \x : Int. #string x in
let print : String^H -> Unit^H = \s : String^H. let s : String^L = s => String^L in #print s in
print (intToString salary)
"""

test """
let age : Int^L = 42 in
let salary : Int^H = 58000 ! Int^H in
let intToString : Int -> String = \x : Int. #string x in
let intToStringL : Int^L -> String^L = intToString => (Int^L -> String^L) in
let print : String^L -> Unit^L = \x : String. #print x in
print (intToStringL salary)
"""

test """
let fun : (Unit^L -> Int^L) -> Int^L = \f : (Unit^L -> Int^L). f () in
(fun ! (Unit^L -> Int^H) -> Int^L) (\u : Unit^L. 1^H)
"""

test """
let fun : (Unit^L -> Int^L) -> Int^L = \f : (Unit^L -> Int^L). f () in
(fun => (Unit^L -> Int^H) -> Int^L) (\u : Unit^L. 1^H)
"""

test """
let fun : (Int^H -> Int^H) = \x : Int^H. x in
(fun ! (Int^L -> Int^H)) 1^L
"""

test """
let salary : Int = 12445^H in
let leakValue : Int = salary + 144 in
#print (#string leakValue)
"""

test """
let salary : Int^H = 12445^H in
let leakValue : Int^L = salary + 144^L in
#print (#string leakValue)
"""

test """
let salary : Int^H = 12445^H in
let leakFunction : Int^H -> String^L = \x : Int^H. (#string x) + "leaking" in
#print (leakFunction salary)
"""

test """
let salary : Int = 12445^H in
let leakFunction : Int -> String = \x : Int. (#string x) + "leaking" in
#print ((leakFunction salary) => String^L)
"""

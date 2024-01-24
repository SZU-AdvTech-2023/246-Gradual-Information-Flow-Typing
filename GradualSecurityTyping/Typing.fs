module Typing

open Syntax

type Environment = (string * LabeledType) list

exception TypingError of string

let findType x (env: Environment) =
    let (_, t) = List.find (fun (y, _) -> x = y) env
    t

let typeOfConstant = function
    | ConstantInt _ -> BaseTInt
    | ConstantBool _ -> BaseTBool
    | ConstantString _ -> BaseTString
    | ConstantUnit -> BaseTUnit

let rec (<!) typeA typeB =
    match typeA, typeB with
    | LabeledType (RawTBase b1, l), LabeledType (RawTBase b2, k) -> b1 = b2 && l <=* k
    | LabeledType (RawTArrow (typeA, typeB), l), LabeledType (RawTArrow (typeA', typeB'), k) ->
        l <=* k && typeA' <! typeA && typeB <! typeB'
    | _ -> raise (TypingError "types can not be compared")

let rec (<!+) typeA typeB =
    let s = sprintf "%s <!+ %s" (Syntax.labeledTypeToString typeA) (Syntax.labeledTypeToString typeB)
    match typeA, typeB with
    | LabeledType (RawTBase b1, l), LabeledType (RawTBase b2, k) -> b1 = b2 && l <=* k
    | LabeledType (RawTArrow (typeA, typeB), l), LabeledType (RawTArrow (typeA', typeB'), k) ->
        l <=* k && typeA' <!- typeA && typeB <!+ typeB'
    | _ -> raise (TypingError "types can not be compared")
and (<!-) typeA typeB =
    let s = sprintf "%s <!- %s" (Syntax.labeledTypeToString typeA) (Syntax.labeledTypeToString typeB)
    match typeA, typeB with
    | LabeledType (RawTBase b1, l), LabeledType (RawTBase b2, k) -> b1 = b2 && k <=* l
    | LabeledType (RawTArrow (typeA, typeB), l), LabeledType (RawTArrow (typeA', typeB'), k) ->
        l <=* k && typeA' <!+ typeA && typeB <!- typeB'
    | _ -> raise (TypingError "types can not be compared")

let union l1 l2 =
    match l1, l2 with
    | _, H -> H
    | H, _ -> H
    | G, _ -> G
    | _, G -> G
    | L, L -> L

let unionMany = function
    | l :: ls -> List.fold union l ls
    | [] -> L


type STLCType =
    | STLCTBase of BaseType
    | STLCTArrow of STLCType * STLCType

let rec convertToSimplyTypedLambda = function
    | LabeledType (RawTBase BaseTInt, _) -> STLCTBase BaseTInt
    | LabeledType (RawTBase BaseTBool, _) -> STLCTBase BaseTBool
    | LabeledType (RawTBase BaseTString, _) -> STLCTBase BaseTString
    | LabeledType (RawTBase BaseTUnit, _) -> STLCTBase BaseTUnit
    | LabeledType (RawTArrow (typeA, typeB), _) ->
        STLCTArrow (convertToSimplyTypedLambda typeA, convertToSimplyTypedLambda typeB)
    

let typingOperator operator rawTypes =
    match operator, rawTypes with
    | Add, [RawTBase BaseTInt; RawTBase BaseTInt] -> RawTBase BaseTInt
    | Add, [RawTBase BaseTString; RawTBase BaseTString] -> RawTBase BaseTString
    | ToStr, [RawTBase BaseTInt] | ToStr, [RawTBase BaseTBool] -> RawTBase BaseTString
    | Print, [RawTBase BaseTString] -> RawTBase BaseTUnit
    | _ -> raise (TypingError "mismatch parameters of operator")


let rec typing (env: Environment) (term: Term) : LabeledType * EvalTerm =
    match term with
    | TermValue (LabeledValue (RawVConstant c, l)) ->
        LabeledType (RawTBase (typeOfConstant c), l), ETermValue (LabeledValue (RawVConstant c, l))
    | TermVariable x ->
        findType x env, ETermVariable x
    | TermValue (LabeledValue (RawVLambda (x, typeA, body), l)) ->
        let typeB, body' = typing ((x, typeA) :: env) body
        LabeledType (RawTArrow (typeA, typeB), l), ETermValue (LabeledValue (RawVLambda (x, typeA, body'), l))
    | TermApply (termT, termS) ->
        match typing env termT with
        | LabeledType (RawTArrow (typeA, LabeledType (b, k)), l), termT' ->
            let typeA', termS' = typing env termS
            if typeA' <! typeA then
                LabeledType (b, union k l), ETermApply (termT', termS')
            else
                raise (TypingError "mismatch argument")
        | _ ->
            raise (TypingError "only lambda can be apply")
    | TermClassification (t, typeB) ->
        let typeA, t' = typing env t
        if typeA <!+ typeB then
            typeB, ETermClassification (t', typeA, typeB) //此处通过typing补充缺失的typeA
        else
            raise (TypingError "mismatch classify")
    | TermCast (t, position, typeB) ->
        let typeA, t' = typing env t
        if convertToSimplyTypedLambda typeA = convertToSimplyTypedLambda typeB then
            typeB, ETermCast (t', typeA, position, typeB) //此处通过typing补充缺失的typeA
        else
            raise (TypingError "mismatch cast")
    | TermOperation (operator, parameters) ->
        let parameterTypes, parameters' = List.unzip [for p in parameters do typing env p]
        let rawTypes = [for (LabeledType (raw, _)) in parameterTypes do raw]
        let levels = [for (LabeledType (_, k)) in parameterTypes do k]
        let raw = typingOperator operator rawTypes
        LabeledType (raw, unionMany levels), ETermOperation (operator, parameters')
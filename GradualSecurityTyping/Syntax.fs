module Syntax

// G表示GradualSecurityLevel，这是原论文没有的
type SecurityLevel = H | L | G

// Level之间的偏序关系
let (<=*) l1 l2 =
    match l1, l2 with
    | _, G -> true
    | G, _ -> true
    | L, H -> true
    | L, L -> true
    | H, H -> true
    | H, L -> false

type BaseType =
    | BaseTInt
    | BaseTBool
    | BaseTString
    | BaseTUnit

type RawType =
    | RawTBase of BaseType
    | RawTArrow of LabeledType * LabeledType
and LabeledType = LabeledType of RawType * SecurityLevel

type Operator =
    | Add
    | ToStr
    | Print

// Neg参数表示Blame的正负
type Position = { Neg: bool; Line: int; Column: int }

type Constant =
    | ConstantInt of int
    | ConstantBool of bool
    | ConstantString of string
    | ConstantUnit

type RawValue<'Term> =
    | RawVConstant of Constant
    | RawVLambda of string * LabeledType * 'Term

type LabeledValue<'Term> = LabeledValue of RawValue<'Term> * SecurityLevel

// 为了方便用户编程 将原paper的Classification 和 Cast 简化
type Term =
    | TermValue of LabeledValue<Term>
    | TermVariable of string
    | TermApply of Term * Term
    | TermOperation of Operator * Term list
    | TermClassification of Term * LabeledType
    | TermCast of Term * Position * LabeledType

// EvalTerm填充了Term的简化掉的部分 以匹配Evaluation规则
type EvalTerm =
    | ETermValue of LabeledValue<EvalTerm>
    | ETermVariable of string
    | ETermApply of EvalTerm * EvalTerm
    | ETermOperation of Operator * EvalTerm list
    | ETermClassification of EvalTerm * LabeledType * LabeledType
    | ETermCast of EvalTerm * LabeledType * Position * LabeledType


(* Pretty ToString *)

let operatorToString = function
    | Add -> "+"
    | ToStr -> "#string"
    | Print -> "#print"

let levelToString = function
    | G -> ""
    | L -> "L"
    | H -> "H"

let baseTypeToString = function
    | BaseTBool -> "Bool"
    | BaseTInt -> "Int"
    | BaseTString -> "String"
    | BaseTUnit -> "Unit"

let stringToBaseType = function
    | "Bool" -> BaseTBool
    | "Int" -> BaseTInt
    | "String" -> BaseTString
    | "Unit" -> BaseTUnit
    | s -> failwithf "no such type: %s" s

let rec labeledTypeToString (LabeledType (rawType, k)) =
    match k with
    | G -> sprintf "%s" (rawTypeToString rawType)
    | _ -> sprintf "%s^%s" (rawTypeToString rawType) (levelToString k)
and rawTypeToString = function
    | RawTBase baseType -> baseTypeToString baseType
    | RawTArrow (labeledType1, labeledType2) ->
        sprintf "(%s->%s)" (labeledTypeToString labeledType1) (labeledTypeToString labeledType2)

let constantToString = function
    | ConstantBool b -> string b
    | ConstantInt i -> string i
    | ConstantString s -> $"\"{s}\""
    | ConstantUnit -> "()"

let rec labeledValueToString termToString (LabeledValue (rawValue, k)) =
    match k with
    | G -> sprintf "%s" (rawValueToString termToString rawValue)
    | _ -> sprintf "%s^%s" (rawValueToString termToString rawValue) (levelToString k)
and rawValueToString termToString = function
    | RawVConstant c -> constantToString c
    | RawVLambda (x, typeX, term) -> sprintf "(\%s:%s.%s)" x (labeledTypeToString typeX) (termToString term)

let rec termToString = function
    | TermValue v -> labeledValueToString termToString v
    | TermVariable x -> x
    | TermApply (term1, term2) -> sprintf "%s %s" (termToString term1) (termToString term2)
    | TermOperation (operator, parameters) ->
        sprintf "%s(%s)"
            (operatorToString operator)
            (System.String.Join(",", [for p in parameters do termToString p]))
    | TermClassification (term, typeT) ->
        sprintf "%s!%s" (termToString term) (labeledTypeToString typeT)
    | TermCast (term, position, typeT) ->
        sprintf "(%s)=>%s" (termToString term) (labeledTypeToString typeT)

let rec evalTermToString = function
    | ETermValue v -> labeledValueToString evalTermToString v
    | ETermVariable x -> x
    | ETermApply (term1, term2) -> sprintf "%s %s" (evalTermToString term1) (evalTermToString term2)
    | ETermOperation (operator, parameters) ->
        sprintf "%s(%s)"
            (operatorToString operator)
            (System.String.Join(",", [for p in parameters do evalTermToString p]))
    | ETermClassification (term, _, typeT) ->
        sprintf "%s!%s" (evalTermToString term) (labeledTypeToString typeT)
    | ETermCast (term, _, position, typeT) ->
        sprintf "(%s)=>%s" (evalTermToString term) (labeledTypeToString typeT)
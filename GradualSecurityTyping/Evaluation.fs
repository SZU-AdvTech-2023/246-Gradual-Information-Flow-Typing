module Evaluation

open Syntax

//表示出现type error，例如将Int作为函数调用
exception Abort of EvalTerm

//表示出现不合法的information flow
exception Blame of Position

let rec substitute (term: EvalTerm) (x: string) (value: LabeledValue<EvalTerm>) =
    match term with
    | ETermValue (LabeledValue (RawVLambda (y, ty, body), k)) ->
        ETermValue (LabeledValue (RawVLambda (y, ty, substitute body x value), k))
    | ETermValue v -> ETermValue v
    | ETermVariable y when x = y -> ETermValue value
    | ETermVariable _ -> term
    | ETermApply (termT, termS) -> ETermApply (substitute termT x value, substitute termS x value)
    | ETermOperation (operator, parameters) ->
        ETermOperation (operator, [for t in parameters do substitute t x value])
    | ETermClassification (t, typeA, typeB) -> ETermClassification (substitute t x value, typeA, typeB)
    | ETermCast (t, typeA, position, typeB) -> ETermCast (substitute t x value, typeA, position, typeB)

let union l1 l2 =
    match l1, l2 with
    | L, L -> L
    | H, H -> H
    | _, H -> H
    | H, _ -> H
    | L, G -> L
    | G, L -> L
    // G标签不能出现在value上，运行时若出现 union G G 说明必然有一个G标签在value上
    | G, G -> failwith "gradual label should not appear in value!"

let unionMany (levels: SecurityLevel list) =
    match levels with
    | l :: ls -> List.fold union l ls
    | [] -> L

// 对运算符求值
let operate (operator: Operator) (parameters: RawValue<EvalTerm> list) =
    match operator, parameters with
    | Add, [RawVConstant (ConstantInt i1); RawVConstant (ConstantInt i2)] ->
        Some (RawVConstant (ConstantInt (i1 + i2)))
    | Add, [RawVConstant (ConstantString s1); RawVConstant (ConstantString s2)] ->
        Some (RawVConstant (ConstantString (s1 + s2)))
    | ToStr, [RawVConstant (ConstantInt i)] ->
        Some (RawVConstant (ConstantString (string i)))
    | ToStr, [RawVConstant (ConstantBool b)] ->
        Some (RawVConstant (ConstantString (if b then "true" else "false")))
    | Print, [RawVConstant (ConstantString s)] ->
        printfn "Print:%s" s;
        Some (RawVConstant ConstantUnit)
    | _ ->
        None

// 用于blame取反
let negPosition (p: Position) = { p with Neg = not p.Neg }

let rec evaluate (term: EvalTerm) : LabeledValue<EvalTerm> =
    printfn "Eval:%s" (Syntax.evalTermToString term)
    match term with
    | ETermValue value -> value
    | ETermApply (termT, termS) ->
        match evaluate termT with
        | LabeledValue (RawVLambda (x, typeA, t), l) ->
            let value = evaluate termS
            evaluate (substitute t x value)
        | _ ->
            raise (Abort term)
            
    | ETermOperation (operator, parameters) ->
        let values = [for p in parameters do evaluate p]
        let rawValues = [for (LabeledValue (raw, _)) in values do raw]
        let levels = [for (LabeledValue (_, k)) in values do k]
        let rawValue =
            match operate operator rawValues with
            | Some r -> r
            | None -> raise (Abort term)
        let level = unionMany levels
        LabeledValue (rawValue, level)
    | ETermCast (
        t,
        LabeledType (RawTArrow (typeA, typeB), k),
        position,
        LabeledType (RawTArrow (typeA', typeB'), l)
        ) ->
            let (LabeledValue (r, m) as rm) = evaluate t
            if m <=* l then
                let t = LabeledValue (
                        RawVLambda (
                            "x'", typeA', ETermCast (
                                ETermApply (
                                    ETermValue rm,
                                    ETermCast (
                                        ETermVariable "x'",
                                        typeA',
                                        negPosition position,
                                        typeA
                                    )
                                ),
                                typeB,
                                position,
                                typeB'
                            )
                        ),
                        L // Bottom
                    )
                t
            else
                raise (Blame position)
    | ETermCast (t, LabeledType (RawTBase b1, k), position, LabeledType (RawTBase b2, l)) when b1 = b2 ->
        let (LabeledValue (r, m)) = evaluate t
        if m <=* l then
            LabeledValue (r, m)
        else
            raise (Blame position)
    | ETermClassification (t, LabeledType (RawTBase b1, k), LabeledType (RawTBase b2, l)) ->
        let (LabeledValue (r, m)) = evaluate t
        LabeledValue (r, union m l)
    | ETermClassification (
        t,
        LabeledType (RawTArrow (typeA, typeB), k),
        LabeledType (RawTArrow (typeA', typeB'), l)
        ) ->
            let (LabeledValue (r, m) as rm) = evaluate t
            LabeledValue (
                RawVLambda (
                    "x'", typeA', ETermClassification (
                        ETermApply (
                            ETermValue rm,
                            ETermClassification (
                                ETermVariable "x'",
                                typeA',
                                typeA
                            )
                        ),
                        typeB,
                        typeB'
                    )
                ),
                union m l
            )
    | _ -> raise (Abort term)

let evaluatePrint (term: EvalTerm) =
    try
        printfn "Succeed:%s" (labeledValueToString evalTermToString (evaluate term))
    with
    | Abort t -> printfn "Abort:%s" (evalTermToString t)
    | Blame p -> printfn "Blame:%s at %i" (if p.Neg then "neg" else "positive") p.Column
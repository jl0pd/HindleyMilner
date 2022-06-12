open System

type SyntaxNode =
    | Lambda of variable:string * body:SyntaxNode
    | Ident of string
    | Apply of fn:SyntaxNode * arg:SyntaxNode
    | Let of variable:string * definition:SyntaxNode * body:SyntaxNode
    | LetRec of variable:string * definition:SyntaxNode * body:SyntaxNode
    override ast.ToString() =
        let nakedString ast =
            match ast with
            | Ident name -> name
            | Lambda (v, b) -> "\\" + v + " -> " + b.ToString()
            | Apply (fn, arg) -> fn.ToString() + " " + arg.ToString()
            | Let (v, d, b) -> "let " + v + " = " + d.ToString() + " in " + b.ToString()
            | LetRec (v, d, b) -> "letrec " + v + " = " + d.ToString() + " in " + b.ToString()
        match ast with
        | Ident _ -> nakedString ast
        | _ -> "(" + nakedString ast + ")"

type TypeError(msg) = inherit Exception(msg)
type ParseError(msg) = inherit Exception(msg)

module TypeSystem =
    open System.Collections.Immutable
    open System.Collections.Generic

    [<AbstractClass>]
    type CType() = class end

    type CVariable (id: int, name:string) =
        inherit CType()
        member val Id = id
        member val Name = name
        member val Instance : CType option = None with get, set
        override v.ToString() =
            match v.Instance with
            | Some i -> i.ToString()
            | None -> v.Name

    type COper (name: string, args: CType[]) =
        inherit CType()
        member val Name = name
        member val Args = args
        override o.ToString() =
            if args.Length = 0 then
                name
            else if args.Length = 2 then
                "(" + args[0].ToString() + " " + name + " " + args[1].ToString() + ")"
            else
                name + " " + String.Join(" ", args)

    let newFunction src dst = COper("->", [|src; dst|])
    let Integer = COper("int", [||])
    let Bool = COper("bool", [||])

    let mutable _nextVariableName = 1
    let getNextVariableName () =
        let result = _nextVariableName
        _nextVariableName <- int _nextVariableName + 1
        sprintf "a%d" result

    let mutable _nextVariableId = 0
    let newVariable ()  =
        let result = _nextVariableId
        _nextVariableId <- _nextVariableId + 1
        CVariable(result, getNextVariableName())

    type Env = Map<string, CType>
    type Nongen = System.Collections.Immutable.ImmutableHashSet<CVariable>

    let rec prune (t: CType) =
        match t with
        | :? CVariable as v when v.Instance.IsSome ->
            let inst = prune (v.Instance.Value)
            v.Instance <- Some inst
            inst
        | _ -> t

    let rec occursIn (v: CVariable) (types: CType seq) =
        Seq.exists (occursInType v) types

    and occursInType (v: CVariable) (ty: CType) =
        match prune ty with
        | a when a = v -> true
        | :? COper as o -> occursIn v o.Args
        | _ -> false

    let fresh (ty: CType) (nongen: Nongen) =
        let mapping = Dictionary<CVariable, CVariable>()
        let rec loop (tp: CType) =
            match prune tp with
            | :? CVariable as v ->
                if occursIn v (nongen |> Seq.cast<CType>) then
                    tp
                else
                    match mapping.TryGetValue v with
                    | (true, r) -> r
                    | (false, _) ->
                        let res = newVariable()
                        mapping[v] <- res
                        res
            | :? COper as o ->
                COper(o.Name, o.Args |> Array.map loop)
            | _ -> failwith "not supported type"
        loop ty

    let inline referenceEquals a b = LanguagePrimitives.PhysicalEquality a b

    let rec unify (t1: CType) (t2: CType) =
        let ty1 = prune t1
        let ty2 = prune t2
        match ty1, ty2 with
        | (:? CVariable as a, b) ->
            if not <| referenceEquals b a then
                if occursInType a b then
                    raise (TypeError "Recursive unification")
                a.Instance <- Some b
        | (:? COper as a, (:? CVariable as b)) -> unify b a
        | (:? COper as a, (:? COper as b)) ->
            if a.Name <> b.Name || a.Args.Length <> b.Args.Length then
                raise (TypeError "Type mismatch")
            Array.iter2 unify a.Args b.Args
        | _ -> ()

    let isIntLiteral (str: string) = fst (Int32.TryParse str)

    let getType name (env: Env) (nongen: Nongen) : CType =
        if env.ContainsKey name then
            fresh env[name] nongen
        else if isIntLiteral name then
            Integer
        else
            raise (ParseError ("Undefined symbol " + name))

    let rec analyseCore (ast: SyntaxNode) (env: Env) (nongen: Nongen) : CType =
        match ast with
        | Ident name -> getType name env nongen
        | Apply (fn, arg) ->
            let funType = analyseCore fn env nongen
            let argType = analyseCore arg env nongen
            let resultType = newVariable()
            unify (newFunction argType resultType) funType
            resultType
        | Lambda (arg, body) ->
            let argType = newVariable()
            let resultType = analyseCore body (Map.add arg argType env) (nongen.Add argType)
            newFunction argType resultType
        | Let (v, defn, body) ->
            let defnType = analyseCore defn env nongen
            let newEnv = Map.add v defnType env
            analyseCore body newEnv nongen
        | LetRec (v, defn, body) ->
            let newType = newVariable ()
            let newEnv = Map.add v (newType :> CType) env
            let defnType = analyseCore defn newEnv (nongen.Add newType)
            unify newType defnType
            analyseCore body newEnv nongen

    let analyse (ast: SyntaxNode) (env: Env) : CType = analyseCore ast env ImmutableHashSet.Empty

open TypeSystem
let main () =
    let var1 = newVariable()
    let var2 = newVariable()
    let pairType = COper ("*", [|var1; var2|])

    let obj = COper("obj", [||])

    let var3 = newVariable()
    let myEnv = Map.ofList<_,CType> [
        "pair", newFunction var1 (newFunction var2 pairType)
        "true", Bool
        "box", newFunction (newVariable()) obj
        "|>", newFunction var1 (newFunction (newFunction var1 var2) var2) // 'a -> ('a -> 'b) -> 'b
        "cond", newFunction Bool (newFunction var3 (newFunction var3 var3))
        "zero", newFunction Integer Bool
        "pred", newFunction Integer Integer
        "times", newFunction Integer (newFunction Integer Integer)
    ]

    let pair =
        Apply(
            Apply(
                Ident "pair",
                Apply (Ident "f", Ident "4")
            ),
            Apply (Ident "f", Ident "true")
        )

    let examples = [|
        // factorial
        LetRec("factorial", // letrec factorial =
            Lambda("n",    // fn n =>
                Apply(
                    Apply(   // cond (zero n) 1
                        Apply(Ident("cond"),     // cond (zero n)
                            Apply(Ident("zero"), Ident("n"))),
                        Ident("1")),
                    Apply(    // times n
                        Apply(Ident("times"), Ident("n")),
                        Apply(Ident("factorial"),
                            Apply(Ident("pred"), Ident("n")))
                    )
                )
            ),      // in
            Apply(Ident("factorial"), Ident("5"))
        )

        // Should fail:
        // fn x => (pair(x(3) (x(true)))
        Lambda("x",
            Apply(
                Apply(Ident("pair"),
                    Apply(Ident("x"), Ident("3"))),
                Apply(Ident("x"), Ident("true"))))

        // pair(f(3), f(true))
        Apply(
            Apply(Ident("pair"), Apply(Ident("f"), Ident("4"))),
            Apply(Ident("f"), Ident("true")))


        // letrec f = (fn x => x) in ((pair (f 4)) (f true))
        Let("f", Lambda("x", Ident("x")), pair)

        // fn f => f f (fail)
        Lambda("f", Apply(Ident("f"), Ident("f")))

        // let g = fn f => 5 in g g
        Let("g",
            Lambda("f", Ident("5")),
            Apply(Ident("g"), Ident("g")))

        // example that demonstrates generic and non-generic variables:
        // fn g => let f = fn x => g in pair (f 3, f true)
        Lambda("g",
                Let("f",
                    Lambda("x", Ident("g")),
                    Apply(
                        Apply(Ident("pair"),
                                Apply(Ident("f"), Ident("3"))
                        ),
                        Apply(Ident("f"), Ident("true")))))

        // Function composition
        // fn f (fn g (fn arg (f g arg)))
        Lambda("f", Lambda("g", Lambda("arg", Apply(Ident("g"), Apply(Ident("f"), Ident("arg"))))))

        // (int -> 'a) -> 'a
        Apply(Ident "|>", Ident "1")

        // Piping into 'box' will produce 'obj'
        Apply(
            Apply(Ident "|>", Ident "1"),
            Ident "box"
        )
    |]

    let tryExp env ast =
        printf "%O : " ast
        try
            let t = analyse ast env
            printfn "%O" t
        with
        | (:? TypeError | :? ParseError) as e -> printfn "%s" e.Message

    Array.iter (tryExp myEnv) examples

main ()

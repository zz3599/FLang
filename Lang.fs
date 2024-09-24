module Lang

type VarName = string

module VarName =
    let make (name: string) = name

[<RequireQualifiedAccess>]
type BType =
    | Int of int
    | Unit

type ArithmeticFn =
    | Add
    | Sub
    | Mul
    | Div

module ArithmeticFn = 
    let apply (fn: ArithmeticFn) (a: int) (b: int) : int = 
        match fn with
        | Add -> a + b
        | Sub -> a - b
        | Mul -> a * b
        | Div -> a / b

type UnaryArithmeticFn = | Neg

type ComparisonFn =
    | Less
    | Equal
    | Greater

module ComparisonFn =
    let apply(fn: ComparisonFn) (a: int) (b: int) : int = 
        match fn with
        | Less -> if a < b then 1 else 0
        | Equal -> if a = b then 1 else 0
        | Greater -> if a < b then 1 else 0

type BuiltinFn =
    | Arithmetic of fn: ArithmeticFn * opA: Expr * opB: Expr
    | Comparison of fn: ComparisonFn * lhs: Expr * rhs: Expr

and Expr =
    | Lit of BType
    | Var of VarName
    // abstraction
    | Abs of var: VarName * body: Expr
    // application of a function to an argument
    | App of expr: Expr * arg: Expr
    | Cond of pred: Expr * trueBranch: Expr * falseBranch: Expr
    //| Bind of recursive: bool * var: VarName * body: Expr * expr: Expr
    | Builtin of BuiltinFn

type EvalError = 
    | Wrongtype of Expr * expectedType: string
exception EvalException of EvalError

module Expr = 
    let asInt = function
        | Lit(BType.Int n) -> n
        | other -> raise (EvalException (Wrongtype(other, "int")))
    let asAbs = function
        | Abs(var, body) -> (var, body)
        | other -> raise (EvalException (Wrongtype(other, "abs")))
    let rec substitute (replaceWhat: VarName) (replaceFor: Expr) (expr: Expr) : Expr =
        let substFn = substitute replaceWhat replaceFor
        match expr with
        | Lit _ -> expr
        | Builtin(Arithmetic(fn, opA, opB)) -> 
            Builtin(Arithmetic(fn, substFn opA, substFn opB))
        | Builtin(Comparison(fn, opA, opB)) -> 
            Builtin(Comparison(fn, substFn opA, substFn opB))
        | Cond(pred, trueBranch, falseBranch) -> 
            Cond(substFn pred, substFn trueBranch, substFn falseBranch)
        | App(expr, arg) -> 
            App(substFn expr, substFn arg)
        | Abs(boundName, body) -> 
            if boundName = replaceWhat then replaceFor else Abs(boundName, substFn body)
        | Var boundName ->
            if boundName = replaceWhat then replaceFor else expr      

let rec eval (expr: Expr) : Expr = 
    match expr with 
    | Lit _ -> expr
    | Builtin (Arithmetic( fn, opA, opB)) -> 
        let valA = eval opA |> Expr.asInt
        let valB = eval opB |> Expr.asInt
        Lit (BType.Int(ArithmeticFn.apply fn valA valB))
    | Builtin (Comparison(fn, lhs, rhs)) -> 
        let valA = eval lhs |> Expr.asInt
        let valB = eval rhs |> Expr.asInt
        Lit (BType.Int(ComparisonFn.apply fn valA valB))     
    | Cond(pred, trueBranch, falseBranch) -> 
        let predVal = eval pred |> Expr.asInt
        if predVal <> 0 then eval trueBranch else eval falseBranch
    | Abs(var, body) -> expr
    | App(expr, arg) -> 
        let lambdaVar, lambdaBody = eval expr |> Expr.asAbs 
        Expr.substitute lambdaVar arg lambdaBody |> eval
    | Var name -> expr

module Example = 
    let lit (n: int) = Lit(BType.Int n)
    let incrFn = 
        Abs(
            var = "x", 
            body = Builtin(Arithmetic(Add, Var("x"), lit 1))
        )
    let incrApp (n: int) = App(expr = incrFn, arg = lit n)

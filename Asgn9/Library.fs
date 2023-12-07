module Asgn9

open System

type ExprC = 
    | NumC of n : float
    | IdC of s : string
    | StrC of s : string
    | IfC of cond : ExprC * t : ExprC * e : ExprC
    | AppC of f : ExprC * args : ExprC list
    | LamC of parameters : string list * body : ExprC

and Value =
    | NumV of value : float
    | BoolV of value : bool
    | StrV of value : string
    | ClosV of parameters : string list * body : ExprC * env : Env
    | UnOpV of action : (Value -> Value)
    | BinOpV of action : (Value -> Value -> Value)

and Binding = 
    struct
        val sym : string
        val value : Value
        new (s: string, v: Value) = { sym = s; value = v }
    end

and Env = Binding list

let plus (x : Value) (y : Value) : Value =
    match x, y with
    | NumV a, NumV b -> NumV (a + b)
    | _ -> failwith "Arguments must be NumC values"

let sub (x : Value) (y : Value) : Value =
    match x, y with
    | NumV a, NumV b -> NumV (a - b)
    | _ -> failwith "Arguments must be NumC values"

let mult (x : Value) (y : Value) : Value =
    match x, y with
    | NumV a, NumV b -> NumV (a * b)
    | _ -> failwith "Arguments must be NumC values"

let div (x : Value) (y : Value) : Value =
    match x, y with
    | NumV a, NumV b -> NumV (a / b)
    | _ -> failwith "Arguments must be NumC values"

let lte (x : Value) (y : Value) : Value =
    match x, y with
    | NumV a, NumV b -> BoolV(a <= b)
    | _ -> failwith "Arguments must be NumC values"

let equal (x : Value) (y : Value) : Value =
    match x, y with
    | NumV a, NumV b -> BoolV(a = b)
    | StrV a, StrV b -> BoolV(a = b)
    | BoolV a, BoolV b -> BoolV(a = b)
    | _ -> failwith "Arguments must be matching types"

let topEnv : Env = [
    new Binding("+", BinOpV plus);
    new Binding("-", BinOpV sub);
    new Binding("*", BinOpV mult);
    new Binding("/", BinOpV div);
    new Binding("<=", BinOpV lte);
    new Binding("equal?", BinOpV equal);
    new Binding("true", BoolV true);
    new Binding("false", BoolV false);]

// Uses a symbol name to look up a value in an environment.
let rec lookup (sym : string) (env : Env) : Value =
    match env with
    | [] -> failwith "Symbol not found"
    | {sym = s; value = v} :: rest -> if s = sym then v else lookup sym rest

// Calls the given function with the given arguments and environment.
let rec call (f : Value) (args : Value list) (env : Env) : Value =
    match f with
    | UnOpV f -> f (args |> List.head)
    | BinOpV f -> f (args |> List.head) (args |> List.tail |> List.head)
    | ClosV (parameters, body, env) ->
            // zip parameters and values to get a list of Binding structures
            let bindings = List.zip parameters args |> List.map (fun (s, v) -> new Binding(s, v))
            // interp body with new env
            interp body (bindings @ env)
    | _ -> failwith "Function is not a callable value"

// Interprets an ExprC into a Value.
and interp (e : ExprC) (env : Env) : Value =
    match e with
    | NumC n -> NumV n
    | StrC s -> StrV s
    | IdC s -> lookup s env
    | IfC (cond, t, e) ->
        match (interp cond env) with
        | BoolV true -> interp t env
        | BoolV false -> interp e env
        | _ -> failwith "If condition is not a boolean"
    | AppC (f, args) -> call (interp f env) (List.map (fun arg -> interp arg env) args) env
    | LamC (parameters, body) -> ClosV (parameters, body, env)


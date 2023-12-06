module Asgn9

open System


type ExprC = 
    | NumC of n: float
    | IdC of s: string
    | StrC of s: string 
    | IfC of cond : ExprC * t : ExprC * e : ExprC

type Value =
    | NumV of value : float
    | BoolV of value : bool
    | StrV of value : string
//    | ClosV of params : string list * body : ExprC * env : Env

type PrimOpV =
    | UnOpV of action : (Value -> Value)
    | BinOpV of action : (Value -> Value -> Value)

//type Env = Binding list 

//type Binding = 
 //    | sym  of value : string
 //    | binding of value : Value

// 
// let plus (x : Value) (y : Value) : Value =
//   match x, y with
//  | NumC a, NumC b -> NumV (a + b)
//  | _ -> failwith "Arguments must be NumC values"

// let sub (x : Value) (y : Value) : Value =
//   match x, y with
//  | NumC a, NumC b -> NumV (a - b)
//  | _ -> failwith "Arguments must be NumC values"

// let mult (x : Value) (y : Value) : Value =
//   match x, y with
//  | NumC a, NumC b -> NumV (a * b)
//  | _ -> failwith "Arguments must be NumC values"

// let div (x : Value) (y : Value) : Value =
//   match x, y with
//  | NumC a, NumC b -> NumV (a / b)
//  | _ -> failwith "Arguments must be NumC values"

//let topEnv : Env =
//    let initialBindings = [
//      Binding (+) 
//      Binding (*)
//      Binding (-) 
//      Binding (/) 
//      Binding (true BoolV true)
//      Binding (false BoolV false)
//   ]
//   initialBindings


let extractNumVs (values: Value list) =
    match values with
    | [NumV x; NumV y] -> [x; y]
    | _ -> failwith "Values are not NumVs"

// Uses a symbol 

// Interprets an ExprC into a Value.
let rec interp (e : ExprC) : Value =
    match e with
    | NumC n -> NumV n
    | StrC s -> StrV s
    | IdC s -> failwith "Not implemented"
    | IfC (cond, t, e) ->
        match (interp cond) with
        | BoolV true -> interp t
        | BoolV false -> interp e
        | _ -> failwith "If condition is not a boolean"

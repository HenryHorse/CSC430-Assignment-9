module Asgn9.Tests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.IsTrue(true)



// Takes an ExprC from the Asgn9 module and checks to see if calling interp with that ExprC returns the expected NumV
[<Test>]
let InterpTest1 () =
    match interp (NumC 1) topEnv with
    | NumV n -> Assert.AreEqual(1, n)
    | _ -> Assert.Fail("interp did not return a NumV")
[<Test>]
let InterpTest2 () =
    match interp (StrC "test") topEnv with
    | StrV s -> Assert.AreEqual("test", s)
    | _ -> Assert.Fail("interp did not return a StrV")
[<Test>]
let InterpTest3 () =
    match interp (IdC "true") topEnv with
    | BoolV v -> Assert.AreEqual(v, true)
    | _ -> Assert.Fail("interp did not return a BoolV")
[<Test>]
let InterpTest4() =
    match interp (IfC((IdC "true"), (NumC 1), (NumC 2))) topEnv with
    | NumV n -> Assert.AreEqual(1, n)
    | _ -> Assert.Fail("interp did not return a NumV")

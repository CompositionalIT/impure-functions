#load @".paket/load/netcoreapp3.1/main.group.fsx"

let mutable environment = 0
let mutable sideEffect = 0

let usesMutableVar (x, _) =
    x + environment
let usesRandom (x, _) =
    let z = System.Random().Next()
    x + z
let hasSideEffect (x, _) =
    sideEffect <- sideEffect + 1
    x
let usesDependency (x, y) =
    y x
let pureFn (x, _) =
    x + 10

let isNotAffectedByImpureInputs f x =
    let y x = (System.Random().Next() + x)
    let a = f (x, y)
    let b = f (x, y)
    a = b

let isNotAffectedByEnvironmentChanges f x =
    let a = f (x, id)
    environment <- environment + 1
    let b = f (x, id)
    a = b

let isNotSideEffectful f x =
    let currentEnvironment = sideEffect
    f (x, id) |> ignore
    f (x, id) |> ignore
    currentEnvironment = sideEffect

let alwaysGivesTheSameOutput f x =
    let a = f (x, id)
    let b = f (x, id)
    a = b

open FsCheck

let isPure f x =
    (alwaysGivesTheSameOutput f x          |@ "Doesn't always give the same output") .&.
    (isNotAffectedByEnvironmentChanges f x |@ "Is affected by environment") .&.
    (isNotAffectedByImpureInputs f x       |@ "Is affected by impure inputs") .&.
    (isNotSideEffectful f x                |@ "Has a side effect")

Check.Quick (isPure usesMutableVar)
Check.Quick (isPure usesRandom)
Check.Quick (isPure usesDependency)
Check.Quick (isPure hasSideEffect)
Check.Quick (isPure pureFn)
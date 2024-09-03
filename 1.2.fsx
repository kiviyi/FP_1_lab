let eps = 0.000001

// functions
let f1 x : float = 3. * x - 14. + exp x - exp(-x)

let f2 x : float = sqrt (1. - x) - System.Math.Tan x

let f3 x : float = x + cos(x ** 0.52 + 2.)

// differentials
let df1dx x : float = 3. + exp x + exp(-x)

let df2dx x : float = -1. / (2. * sqrt (1. - x)) - 1. / (cos x) ** 2.

let df3dx x : float = 1. - 0.52 * (x ** -0.48) * sin(x ** 0.52 + 2.)

// numeric methods
let rec dichotomy f (a: float) (b: float) =
    let average = (a + b) / 2.
    if abs(f average) < eps then average
    else if f a < f b then 
        if (f average) < 0. then dichotomy f average b else dichotomy f a average
    else 
        if (f average) > 0. then dichotomy f average b else dichotomy f a average

let iterations phi (a: float) (b: float) =
    let rec iterations' =
        function
        | xk when abs(xk - (phi xk)) < eps -> xk
        | xk -> iterations' (phi xk)
    iterations' ((a + b) / 2.)

let newton f dfdx (a: float) (b: float) =
    let phi x : float = x - (f x) / (dfdx x)
    iterations phi a b

// Phi functions for iterations method
let phi1 x = x - f1 x / df1dx x
let phi2 x = x - f2 x / df2dx x
let phi3 x = x - f3 x / df3dx x

// Find roots for given intervals using appropriate methods
let main =
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 1. 3.0) (iterations phi1 1. 3.0) (newton f1 df1dx 1. 3.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 0. 1.0) (iterations phi2 0. 1.0) (newton f2 df2dx 0. 1.0)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 0.5 1.0) (iterations phi3 0.5 1.0) (newton f3 df3dx 0.5 1.)

main
let f x = log (2.0 + x)

let a = -1.0
let b = 1.0
let eps = 0.000001

// Define a function to compute f using naive Taylor series method
let taylor_naive x =
    let rec taylor_naive' n acc term =
        if abs term < eps then acc, n
        else
            let term = term * (-1.0) * x / (float n * 2.0)
            taylor_naive' (n + 1) (acc + term) term
    taylor_naive' 1 (log 2.0) (x / 2.0)

// Define a function to do the same in a more efficient way
let taylor x =
    let rec taylor' n acc term =
        if abs term < eps then acc, n
        else
            let term = term * x / (float n * 2.0)
            taylor' (n + 1) (acc + term) term
    taylor' 1 (log 2.0) (x / 2.0)

let n = 10
let main =
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let x1, n1 = taylor_naive x
        let x2, n2 = taylor x
        printfn "%5.2f    %10.6f  %10.6f %d  %10.6f %d" x (f x) x1 n1 x2 n2

// make sure to improve this table to include the required number of iterations
// for each of the methods
main

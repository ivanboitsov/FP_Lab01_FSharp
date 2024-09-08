module equations

// Метод деления пополам (дихотомия)
let rec dichotomy f (a:float) (b:float) =
    let eps = 0.000001
    let xn = (a + b) / 2.
    let fa = f a
    let fb = f b

    if abs(f xn) < eps then
        xn
    elif fa < fb then
        if (f xn) < 0. then dichotomy f xn b else dichotomy f a xn
    else
        if (f xn) > 0. then dichotomy f xn b else dichotomy f a xn

// Метод итерации
let rec iterations phi x0 = 
    let eps = 0.1

    if abs(x0 - (phi x0)) < eps then
        x0
    else
        let next = phi x0
        iterations phi next

// Метод Ньютона
let rec newthon f f' x0 = 
    let phi x: float = x - (f x ) / (f' x)
    // Используя метод итерации
    iterations phi x0


// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
// Оригинальные уравнения
let f1 x: float = 3. * x - 4. * System.Math.Log x - 5. // #23

let f2 x: float = 
    System.Math.Cos (2./x) - 2. * System.Math.Sin (1./x) + (1./x) // #24

let f3 x: float =
    System.Math.Sqrt (1. - 0.4 * (x ** 2.))  - System.Math.Asin x // #25

// Первая производная
let f1' x: float = 3. - 4./x // #23

let f2' x: float = 
    ((2. * System.Math.Cos(1./x) + 2. * System.Math.Sin(2./x))/(x**2.)) - (1./(x**2)) // #24

let f3' x: float = 
    (-2.*x)/(5. * System.Math.Sqrt(1. - ((2.*(x**2.))/5.))) - (1. / (System.Math.Sqrt(1. - (x ** 2.)))) // #25

// Выражение x из первоначального уравнения
let phi1 x : float =  (4. * System.Math.Log x + 5.) / 3.

let phi2 x : float = 1./(2.*sin(1./x) - cos(2./x))

let phi3 x : float = System.Math.Sqrt((1. - (System.Math.Asin x)**2.) / 0.4)

let main = 
    // printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 2. 4.) (iterations phi1 3.) (newthon f1 f1' 3.)
    // printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 1. 2.) (iterations phi2 1.5) (newthon f2 f2' 1.5)
    // printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 0. 1.) (iterations phi3 0.5) (newthon f3 f3' 0.5)


     printfn "%10.5f" (iterations phi1 3.)
     // printfn "%10.5f" (iterations phi2 1.5)
     printfn "%10.5f" (iterations phi3 0.5)
main
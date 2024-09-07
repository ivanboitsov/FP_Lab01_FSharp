module Taylor

// Заданные изначально значения
let a = 0.0
let b = 0.5
let n = 10


// Функция вычисления с помощью встроенных функций F#
let f x : float = float(System.Math.Atan x)

// Наивный способ вычисления ряда Тейлора, где каждый член вычисляется по формуле
let rec taylorDumb (x: float) n (acc: float) =
    let epc = 0.00001

    if abs (f x - acc) < epc then
        acc, n
    else
        let newAcc = acc + ((-1.) ** float(n)) * (x ** (2. * float n + 1.)) / (2. * float n + 1.)
        taylorDumb x (n + 1) newAcc

// Умный способ вычисления ряда Тейлора, где каждый член вычисляется через предыдущий
let rec taylorSmart (x: float) n (prev: float) (acc: float) =
    // Определение эпсилона в зависимости от величины x
    let epc = 
        if abs x < 0.1 then 0.000001
        elif abs x < 0.2 then 0.0001
        elif abs x < 0.3 then 0.001
        else 0.01

    if abs (f x - acc) <= epc then
        acc, n
    else
        let newPrev = -prev * (x ** 2.0) / float (2 * n + 1)
        let newAcc = acc + newPrev
        taylorSmart x (n + 1) newPrev newAcc

let main = 
    // Красивый вывод таблички
    printfn "|--------------------------------------------------------------------|"
    printfn "| %5s | %10s | %10s | %5s |  %10s | %5s |" "x" "Builtin" "Smart Taylor" "#terms" "Dumb Taylor" "#terms"
    printfn "|--------------------------------------------------------------------|"
    for i = 0 to n do
        // Вычисление точки x
        let x = a + (float i) / (float n) * (b - a)
        // Вычисление функции наивным методом
        let dumbResult, _ =  taylorDumb x 0 0.
        // Вычисление функции умным методом
        let smartResult, _ =  taylorSmart x 1 x x
        // Вывод 
        printfn "| %5.2f | %10.6f | %12.6f | %6d | %12.6f | %6d |" x  (f x) dumbResult i smartResult i
        printfn "|--------------------------------------------------------------------|"

// Запуск программы
main
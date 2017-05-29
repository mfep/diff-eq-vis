module DeSolver

type stepfun = float -> (float * float -> float * float) -> float * float -> float * float

let naivestep : stepfun = fun step f (x, y) ->
    let dx, dy = f(x ,y)
    x + dx * step, y + dy * step

let rk4step : stepfun = fun step f (x, y) ->
    let k1x, k1y = f (x, y)
    let k2x, k2y = f (x + step / 2.0 * k1x, y + step / 2.0 * k1y)
    let k3x, k3y = f (x + step / 2.0 * k2x, y + step / 2.0 * k2y)
    let k4x, k4y = f (x + step * k3x, y + step * k3y)
    x + step / 6.0 * (k1x + k2x + k3x + k4x), y + step / 6.0 * (k1y + k2y + k3y + k4y)

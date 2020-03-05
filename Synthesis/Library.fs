module Synthesis

let abelar n =
    (n > 12) && (n < 3097) && (n % 12 = 0)

let area b h =
    match b < 0.0 ||     h < 0.0 with 
    | true -> failwith "The values are negative"
    | false -> b/2.0 * h 

let zollo x =
    match x < 0 with 
    | true -> x * (-1)
    |false -> x * 2 

let min x y =
    match x < y with 
    |true -> x
    |false -> y

let max z x =
    match x > z with
    |true -> x
    |false -> z 

let ofTime h m s = (h * 3600) + (m * 60) + s                                                                                                                                     


let toTime s = 
    match s < 0 with 
    | true -> (0,0,0)
    | false -> 
    let h = s / 3600
    let m = (s % 3600) / 60 
    let snd =  (s % 3600) % 60 
    (h,m,snd)
    

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"
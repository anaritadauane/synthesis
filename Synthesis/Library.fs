module Synthesis

let abelar n =
    (n > 12) && (n < 3097) && (n % 12 = 0)

let area b h =
    match b < 0.0 || h < 0.0 with 
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
    

let digits d =
        let rec count n =
            match n < 10 && n > -10 with 
            | false -> count(n/10)  + 1
            | true -> 1
        count d
    

let minmax mn =
              match mn with 
              |(a, b, c , d) -> min (min a b) (min c d), max (max a b) (max c d)
              |_ -> failwith "Wau"

let isLeap year = 
                  match year >= 1582 with 
                  | true -> 
                        match year % 4 = 0 && year % 100 <> 0 || year % 400 = 0 with 
                        | false -> false | true -> true
                  | false -> failwith "The year is not a leap year"

let month y = 
              match y < 1 ||  y > 12 with 
              | false -> 
                        match y  with 
                        | 1 -> ("January", 31) | 2 -> ("February", 28)| 3 -> ("March", 31) | 4 -> ("April", 30) 
                        | 5 -> ("May", 31) | 6 -> ("June", 30) | 7 -> ("July", 31) | 8 -> ("August", 31) 
                        | 9 -> ("September", 30)| 10 -> ("October", 31) | 11 -> ("November", 30) | 12 -> ("December", 31)
              | true -> failwith "The year does not exit"

    

let toBinary tb = 
               match tb < 0 with 
               | true -> failwith "A negative integer has been supplied"
               | false -> 
                         match tb = 0 with 
                         | true -> "0"
                         | false ->
                               let rec count n a= 
                                 match n = 0 with 
                                 | true -> a
                                 | false -> count(n /2) (string(n%2) + a)
                               count  tb  ""

let bizFuzz y = 
              let rec count n  =  
                  match y > 1 with 
                  | false -> 0
                  | true ->
                          
                        match n % 3 = 0 with 
                        | true -> 1 + count(n - 1) 
                        | false -> count (n - 1)

              let rec count5 n  =  
                  match y > 1 with 
                  | false -> 0
                  | true ->
                          
                        match n % 5 = 0 with 
                        | true -> count(n - 1) + 1
                        | false -> 0

              let rec count35 n  =  
                  match y > 1 with 
                  | false -> 0
                  | true ->
                          
                        match n % 3 = 0 && n % 5 = 0 with 
                        | true -> count(n - 1) + 1
                        | false -> 0

       
              (count y  , count5 y, count35 y )   
              



let monthDay _ _ =
    failwith "Not implemented"

let sqrt n =
   let rec calculate guess i =
    match i with
    | 10 -> guess
    | _ ->
         let g = (guess + n/guess) / 2.0
         calculate g (i+1)
   match n <= 0.0 with
   | true -> failwith "Impossibru!"
   | _ ->
         calculate (n/2.0) 0



let coord a =
  failwith "Not implemented"

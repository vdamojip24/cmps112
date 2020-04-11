(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])


    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))


    let trimzeros list =
         let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                let cdr' = trimzeros' cdr
                in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
        in trimzeros' list
   
    let rec cmp' list1 list2 = match(list1, list2) with
        | [], []          -> 0     
        | list1, []         -> 1
        | [], list2         -> -1
        | [_], [_]          -> 

        (if (car list2) > (car list1)
                then -1
        else if (car list1) > (car list2)             
                then 1
        else 0)
        
        | list1, list2      -> 

        (if (car list2) > (car list1)
                then -1
        else if (car list1) > (car list2)             
                then 1
        else cmp' (cdr list1) (cdr list2))

    let rec cmp list1 list2 =
        
        let reverseList1 = reverse (trimzeros list1) in     
        let reverseList2 = reverse (trimzeros list2) in 
        if List.length reverseList1 > List.length reverseList2
        then 1 
        else if List.length reverseList1 < List.length reverseList2
        then -1
        else
        cmp' reverseList1 reverseList2
           


    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0 -> list1
        | list1, [], carry -> sub' list1 [- carry] 0
        | [], _::_, _ -> raise (Invalid_argument "sub'")
        | car1::cdr1, car2::cdr2, carry ->
            let diff = car1 - car2 + carry + 10
            in diff mod 10 :: sub' cdr1 cdr2 (diff / 10 - 1)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let compVal = cmp value1 value2 in 
        if compVal = 0
            then zero
        else if neg1 != neg2 
            then Bigint (neg1, add' value1 value2 0)
        else if neg1 = neg2 
            then (if cmp value1 value2 = 1
                then Bigint (neg1, trimzeros (sub' value1 value2 0))
                else if cmp value1 value2 = -1      
                    then (if neg1 = Pos
                        then Bigint (Neg, 
                            trimzeros(sub' value2 value1 0))
                        else Bigint (Pos, 
                            trimzeros(sub' value2 value1 0)))
                else zero)
        else zero
    

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
            then Bigint (neg1, add' value1 value2 0)
        else if neg1 != neg2 
            then (if cmp value1 value2 = 1
                then Bigint (neg1, trimzeros(sub' value1 value2 0))
                else if cmp value1 value2 = -1
                then Bigint (neg2, trimzeros(sub' value2 value1 0))
                else zero)
        else zero

    let double x  = add' x x 0 

    let rec mul' (multiplier, powerof2, multiplicand') =
        if cmp powerof2 multiplier = 1
        then multiplier, [0]
        else let remainder, product =
             mul' (multiplier, double powerof2, double multiplicand')
            in  if cmp remainder powerof2 = -1 
                then remainder, product
                else 
                trimzeros(sub' remainder powerof2 0), 
                add' product multiplicand' 0 

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, product = mul' (value1, [1], value2) in 
        if neg1 != neg2
        then Bigint (Neg, product )
        else
        Bigint (Pos, product)

    (*let rec sub' num1 num2 carry = match (num1, num2, carry) with
        | list1, [], 0 -> list1
        | list1, [], carry -> sub' list1 [- carry] 0
        | [], _::_, _ -> raise (Invalid_argument "sub'")
        | h1::t1, h2::t2, carry ->
        let diff = h1 - h2 + carry + 10
        in diff mod 10 :: sub' t1 t2 (diff / 10 - 1)*)

    let rec divrem' (dividend, powerof2, divisor') =
        if cmp divisor' dividend = 1
        then [0], dividend
        else let quotient, remainder =
                divrem' (dividend, double powerof2, double divisor')
            in  if cmp remainder divisor' = -1
                 then quotient, remainder
                 else 
                 add' quotient powerof2 0, sub' remainder divisor' 0

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, _ = divrem (value1, value2) in
        if neg1 != neg2 
        then Bigint (Neg, trimzeros(quotient ))
        (*then (if cmp [0] quotient = 0 
            then Bigint (Pos, quotient)
            else 
            Bigint (Neg, quotient )) *)
        else
        Bigint (Pos, trimzeros(quotient))

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, remainder = divrem (value1, value2) in
        if neg1 = Neg 
        then Bigint (Neg, trimzeros(remainder))
        (*then (if cmp [0] remainder = 0
            then Bigint (Pos, remainder)
            else 
            Bigint (Neg, remainder)) *)
        else
        Bigint (Pos, trimzeros(remainder))  
    
    
    (*let even number = (cmp (snd divrem' (number, [1], [2])) [0] = 0)*)
    let even number = cmp (snd (divrem' (number, [1], [2]))) [0] = 0



    let rec pow' (base, expt, result) = match expt with 
        | [0] -> result
        | []  -> result
        | expt when even expt -> pow' (snd(mul' (base, [1], base)) , 
            fst (divrem'(expt, [1], [2])), result)
        | expt -> pow' (base, sub' expt [1] 0, 
            snd ((mul' (base, [1], result))))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg
        then if cmp value1 [1] = 0
            then (if (neg1 = Pos || even value2)
                then Bigint (Pos, [1])
                else 
                Bigint (Neg, [1]))      
            else zero  
        else if (neg1 = Pos || even value2)
            then Bigint (Pos, pow' (value1, value2, [1]))
        else Bigint(Neg, pow' (value1, value2, [1]) )
        

end

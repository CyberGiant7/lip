let rec alt_even a = match a with
    0 -> true 
  | _ -> if (a > 0 &&  a < 9) then a mod 2 = 0
      else if (a mod 2 = 0) && (a/10) mod 2 != 0 then alt_even (a/100) else false
;;


alt_even 101010;;
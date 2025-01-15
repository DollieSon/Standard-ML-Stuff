(* 
val incremented = ref 0
val target = 10
val u = while (!x) <> 10 do (
    incremeted := !x * 
)
 *)

(* val factorial : int -> int =
    fn x => 
        val target = ref 1;
        val result = ref 1;
        val _ = while (!target) <> 0 do (
            result := !result * !target
            target := !target - 1
        )
        result

val _ = print (Int.toString (factorial(3)) ^ "\n"); *)
val factorial : int -> int = 
    fn x =>
    let 
        val target = ref x
        val result = ref 1
    in
        (while (!target) > 1 do (
            result := !result * !target;
            target := !target - 1
        );
        !result
        )
    end;

fun fibonacci n =
    let
        val a = ref 0  
        val b = ref 1  
        val i = ref 0  
    in
        while !i < n do (
            let
                val temp = !a + !b
            in
                a := !b;
                b := temp;
                i := !i + 1
            end
        );
        !a  
    end;

(* Test the function *)
val _ = print ("Fibonacci(10): " ^ Int.toString (fibonacci 10) ^ "\n");
val _ = print ("Result: " ^ Int.toString (factorial(10)) ^ "\n");
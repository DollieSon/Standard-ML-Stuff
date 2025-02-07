print("Hello World\n");
(*Sleeping Function*)
fun sleep seconds =
    let
        val start = Time.now ()
        val target = Time.+ (start, Time.fromSeconds seconds)
        fun loop () =
            if Time.< (Time.now (), target) then loop () else ()
    in
        loop ()
    end;

(*
clearing the screen
OS.Process.system "cls";
*)

(*printing rows*)

fun printRow arr row cols =
    let
        fun loop col =
            if col < cols then
                (
                if Array2.sub (arr, row, col) = 1 then
                     print "# "
                 else 
                     print "  ";
                 (* if (Array2.sub (arr,row col) = 1) then
                 (print ("# ");)
                 else 
                 (print ("  ");) *)
                 (* print (Int.toString (Array2.sub (arr, row, col)) ^ " ");  *)
                 loop (col + 1))
            else
                print "\n"
    in
        loop 0
    end;
(*printing cols / entire matrix*)
fun printMatrix arr =
    let
        val (rows, cols) = Array2.dimensions arr
        fun loop row =
            if row < rows then
                (printRow arr row cols; loop (row + 1))
            else
                ()
    in
        loop 0
    end;
(*
fun printMatrix arr =
    let
        val (rows, cols) = Array2.dimensions arr
        fun loop row =
            if row < rows then
                (printRow arr row cols;
                 loop (row + 1)
                 )
            else
                ()
    in
        loop 0
    end;
*)

(*counting Neighbors of cells*)
(*
fun countNeighbors arr row col = 
    let
        val (rows, cols) = Array2.dimensions arr
        val neighbors = ref 0
        fun isValid (r, c) = r >= 0 andalso r < rows andalso c >= 0 andalso c < cols
        fun countR r c =
            if isValid (r, c) then
                if Array2.sub (arr, r, c) = 1 then
                    neighbors := !neighbors + 1
                else
                    ();
            else
                ()
        in
            List.app (fn (dr, dc) => countR (row + dr, col + dc))
                [(~1, ~1), (~1, 0), (~1, 1), (0, ~1), (0, 1), (1, ~1), (1, 0), (1, 1)];
            !neighbors
    end;
*)
fun countNeighbors (arr, row, col) =
    let
        val (rows, cols) = Array2.dimensions arr
        
        (* Helper to check if coordinates are within bounds *)
        fun isValid (r, c) = 
            r >= 0 andalso r < rows andalso 
            c >= 0 andalso c < cols
        
        (* List of relative coordinates for all 8 neighbors *)
        val directions = [
            (~1, ~1), (~1, 0), (~1, 1),
            (0, ~1),           (0, 1),
            (1, ~1),  (1, 0),  (1, 1)
        ]
        
        (* Count a single neighbor if it exists and equals 1 *)
        fun checkNeighbor (dr, dc) =
            let 
                val newRow = row + dr
                val newCol = col + dc
            in
                if isValid (newRow, newCol) andalso 
                   Array2.sub(arr, newRow, newCol) = 1 
                then 1 
                else 0
            end
            
        (* Sum up all valid neighbors *)
        fun sumNeighbors [] = 0
          | sumNeighbors ((dr, dc)::rest) = 
            checkNeighbor(dr, dc) + sumNeighbors rest
    in
        sumNeighbors directions
    end;
(*
fun countNeighbors arr row col =
    let
        val (rows, cols) = Array2.dimensions arr
        val neighbors = ref 0
        fun countR r c =
            if r < rows then
                if c < cols then
                    if Array2.sub (arr, r, c) = 1 then
                        neighbors:= !neighbors + 1
                    else
                        ();
                    countR r (c + 1)
                else
                    countR (r + 1) 0
            else
                !neighbors
    in
        countR (row - 1) (col - 1)
    end;
*)
(*generate next frame*)
fun nextFrame arr1 arr2 =
    let
        val (rows, cols) = Array2.dimensions arr1
        (* Iterates over each cell in the array and updates arr2 *)
        fun frameR row col =
            if row < rows then
                if col < cols then
                    let
                        val neighbors = countNeighbors (arr1, row, col)
                    in
                        (* Rule for Conway's Game of Life *)
                        if Array2.sub (arr1, row, col) = 1 then
                            if neighbors < 2 orelse neighbors > 3 then
                                Array2.update (arr2, row, col, 0)
                            else
                                Array2.update (arr2, row, col, 1)
                        else
                            if neighbors = 3 then
                                Array2.update (arr2, row, col, 1)
                            else
                                Array2.update (arr2, row, col, 0);
                        (* Move to the next column *)
                        frameR row (col + 1)
                    end
                else
                    (* Move to the next row, reset column *)
                    frameR (row + 1) 0
            else
                ()
    in
        frameR 0 0
    end;



(*Set-up board*)
structure Array2 = Array2

val on_board1 = ref true;
(*Simulate Conway's Game Of Life*)
(*
fun simulate b1 b2 epoch =
    let
        fun loop i =
            if i < epoch then
                (printMatrix b1;
                 sleep 2;
                 OS.Process.system "cls";
                 nextFrame b1 b2;
                 if !on_board1 then
                     (Array2.update (b1, 25, 25, 1);
                      on_board1 := false)
                 else
                     (Array2.update (b2, 25, 25, 1);
                      on_board1 := true);
                 loop (i + 1))
            else
                ()
    in
        loop 0
    end;
*)

fun simulate b1 b2 epoch =
    let
        fun loop i =
            if i < epoch then
                (
                (* printMatrix b1; *)
                 (* print "\n\n"; *)
                 (* printMatrix b2; *)
                sleep 1;
                OS.Process.system "cls";
                 nextFrame b1 b2;
                 if !on_board1 then
                    (printMatrix b1;
                     nextFrame b1 b2;
                     (* b1 = b2; *)
                     on_board1 := false)
                 else
                    (printMatrix b2;
                     nextFrame b2 b1;
                     (* b2 = b1; *)
                     on_board1 := true);
                 loop (i + 1))
            else
                ()
    in
        loop 0
    end;


(*Test the function*)

val board1 = Array2.array(21,21,0);
val board2 = Array2.array(21,21,0);
Array2.update (board1, 11, 11, 1);
Array2.update (board1, 12, 11, 1);
Array2.update (board1, 10, 11, 1);
Array2.update (board1, 11, 10, 1);
Array2.update (board1, 12, 12, 1);
(* Array2.update (board1, 24, 25, 1); *)
(* Array2.update (board1, 25, 24, 1); *)
(* Array2.update (board1, 25, 26, 1); *)
(* Array2.update (board1, 26, 25, 1); *)

simulate board1 board2 50;
(*Program is now Skibidead*)


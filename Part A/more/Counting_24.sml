(*
算24点是4个数通过四则运算、括号得到24的一种数学游戏。
给定4个数打印所有可能的计算方法，并且返回方案数量。
同时，规定不能改变四个整数的顺序。
由于不知道sml中将整数强制转化为有理数的函数，故编写时用有理数代替整数。
*)

val Sum_of_Number = 24.0(*final results*)
val Precision = 0.000001(*arithmetic precision*)
exception WRONG


fun cal(x:real, y:real, mark:string) = (* x and y complete the corresponding calculation *)
    case mark of
	"+" => x+y
	| "-" => x-y
	| "*" => x*y
    | "/" => x/y
	| _ => raise WRONG

	
(*5 different processes for calculating the results of 4 digits with 4 kinds of operations*)
fun calculate1(numbers:real*real*real*real, marks:string*string*string) =
    let
    	val flag = cal(cal(cal(#1 numbers, #2 numbers, #1 marks), #3 numbers, #2 marks), #4 numbers, #3 marks)
	in
	    if(Real.abs(flag - Sum_of_Number) < Precision)
		then (print ("(("^Real.toString(#1 numbers)^(#1 marks)^Real.toString(#2 numbers)^")"^(#2 marks)^Real.toString(#3 numbers)^")"^(#3 marks)^Real.toString(#4 numbers)^"=24\n");1)
		else 0
	end

fun calculate2(numbers:real*real*real*real, marks:string*string*string) =
    let 
        val flag = cal(cal(#1 numbers, cal(#2 numbers, #3 numbers, #2 marks), #1 marks), #4 numbers, #3 marks)
	in
	    if(Real.abs(flag - Sum_of_Number) < Precision)
		then (print ("("^Real.toString(#1 numbers)^(#1 marks)^"("^Real.toString(#2 numbers)^(#2 marks)^Real.toString(#3 numbers)^"))"^(#3 marks)^Real.toString(#4 numbers)^"=24\n");1)
		else 0
	end
	
fun calculate3(numbers:real*real*real*real, marks:string*string*string) =
    let 
        val flag = cal(#1 numbers, cal(#2 numbers, cal(#3 numbers, #4 numbers, #3 marks), #2 marks), #1 marks)
	in
	    if(Real.abs(flag - Sum_of_Number) < Precision)
		then (print (Real.toString(#1 numbers)^(#1 marks)^"("^Real.toString(#2 numbers)^(#2 marks)^"("^Real.toString(#3 numbers)^(#3 marks)^Real.toString(#4 numbers)^"))"^"=24\n");1)
		else 0
	end
	
fun calculate4(numbers:real*real*real*real, marks:string*string*string) =
    let 
        val flag = cal(#1 numbers, cal(cal(#2 numbers, #3 numbers ,#2 marks), #4 numbers, #3 marks), #1 marks)
	in
	    if(Real.abs(flag - Sum_of_Number) < Precision)
		then (print (Real.toString(#1 numbers)^(#1 marks)^"(("^Real.toString(#2 numbers)^(#2 marks)^Real.toString(#3 numbers)^")"^(#3 marks)^Real.toString(#4 numbers)^")"^"=24\n");1)
		else 0
	end
	
fun calculate5(numbers:real*real*real*real, marks:string*string*string) =
    let 
        val flag = cal(cal(#1 numbers, #2 numbers, #1 marks), cal(#3 numbers, #4 numbers, #3 marks), #2 marks)
	in
	    if(Real.abs(flag - Sum_of_Number) < Precision)
		then (print ("("^Real.toString(#1 numbers)^(#1 marks)^Real.toString(#2 numbers)^")"^(#2 marks)^"("^Real.toString(#3 numbers)^(#3 marks)^Real.toString(#4 numbers)^")"^"=24\n");1)
		else 0
	end

	
(*Hash decoding to get the corresponding operation symbol*)
fun int_to_marks(n) =
    let
	   val a = n mod 4
	   val b = (n div 4) mod 4
	   val c = (n div 16) mod 4
       fun Tomark(k) =
	       case k of
		   0 => "+"
		   | 1 => "-"
		   | 2 => "*"
		   | 3 => "/"
		   | _ => raise WRONG
	in
      (Tomark(a), Tomark(b), Tomark(c))
	end


	
fun Search(nums:real*real*real*real) =
    let fun loop(numbers, n, acc) =(*acc records number of methods ,n is using hash decoding*)
	        if(n =64) then (print ("There are "^Int.toString(acc)^" ways to get 24!\n\n"); acc) 
            else 
	           (let val marks = int_to_marks(n)
	           in
	                loop(numbers, n+1, acc+calculate1(nums, marks)+calculate2(nums, marks)+calculate3(nums, marks)+calculate4(nums, marks)+calculate5(nums, marks))
	           end)
    in
	   loop(nums,0,0)
    end

	
	
	
val test_0 = Search(1.0,2.0,3.0,4.0)
val test_1 = Search(2.0,3.0,4.0,5.0)
val test_2 = Search(4.0,3.0,2.0,1.0)
val test_3 = Search(5.0,1.0,2.0,3.0)
val test_4 = Search(7.0,2.0,5.0,8.0)
val test_5 = Search(6.0,7.0,8.0,9.0)
val test_6 = Search(7.0,3.0,5.0,6.0)
val test_7 = Search(8.0,9.0,3.0,2.0)
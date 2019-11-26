														(*Karatsuba algorithm*)
													(*programmed by Akshay Gupta*)
															(*2019mcs2556*)
			(*The code is optimized to the point where you can find factorial of 10000 in approx 3 to 5 minutes in 5th Gen Intel i5 processor with clock speed of 2.2GHz*)

exception Invalid_Input_exception of string;

(*no reference functions*)

(*Gives the length of the list*)
fun ak_length([]) = 0
|	ak_length(x) = 1 + ak_length(tl(x));


(*This will reverse the list of any type*)
fun ak_reverse(nil, ak_final) = ak_final
|	ak_reverse(ak_n, ak_final) = ak_reverse(tl(ak_n),hd(ak_n)::ak_final);


(*Pad zeroes to the right of the list*)
fun ak_paddingRight(nil, 0) = nil
|	ak_paddingRight(nil,n) = 0::ak_paddingRight(nil,n-1)
|	ak_paddingRight(a::x, n) = a::ak_paddingRight(x,n);


(*Pad zeroes to the left of the list*)
fun ak_paddingLeft(nil,0) = nil
|	ak_paddingLeft(x,0) = x
|	ak_paddingLeft(x, n) = 0::ak_paddingLeft(x,n-1);


(*This will convert the integer list to the String*)
fun toStringi([]) = ""
|   toStringi(ak_head::ak_tail) = let
									val jodo = if(ak_head mod 10 = ak_head) then "000"^(Int.toString(ak_head))
											   else if(ak_head mod 100 = ak_head) then "00"^(Int.toString(ak_head))
											   else if(ak_head mod 1000 = ak_head) then "0"^(Int.toString(ak_head))
											   else Int.toString(ak_head)
								 in
									jodo^toStringi(ak_tail)
								 end;
								 

								 

(*This function removes the leading zeros from the character list*)
fun ak_removeLeadingZeroes nil = [#"0"]
|	ak_removeLeadingZeroes n = if(hd(n) = #"0") then ak_removeLeadingZeroes(tl(n))
							   else n;


(*This function removes the leading zeros from the integer list*)
fun ak_removeLeadZeroes nil = [0]
|	ak_removeLeadZeroes n = if(hd(n) = 0) then ak_removeLeadZeroes(tl(n))
							   else n;


(*This will convert the String list to String*)
fun toStringc([]) = ""
|   toStringc([ak_a]) = ak_a
|   toStringc(ak_head::ak_tail) = ak_head^toStringc(ak_tail);


(*1 reference functions*)

(*Break the list into two parts where ak_max will be the size of first part*)
fun ak_break(x, y, ak_max) = if null(x) then ak_break(hd(y)::x,tl(y),ak_max-1) else if((ak_max) > 0) then ak_break(hd(y)::x,tl(y),ak_max-1)
							 else (ak_reverse(x,[]),y);


(*This function actually pass the string to another function which in turn changes the list to the string and then this function returns the string without any unnecessary
zeroes in the start of the string*) 
fun toString(x) = 	let
						val s = toStringi(x)
					in
						implode(ak_removeLeadingZeroes(explode(s)))
					end;


(*Addition code*)

(*adds any two list of integers*)
fun ak_addition (nil,nil,c) = [c]
|	ak_addition (a::x, nil, c) = let
								val ak_sum = (a+c) mod 10000
								val ak_carry = (a+c) div 10000
							  in
								ak_sum::ak_addition(x,nil,ak_carry)
							  end
|	ak_addition (nil, b::y, c) = let
								val ak_sum = (b+c) mod 10000
								val ak_carry = (b+c) div 10000
							  in
								ak_sum::ak_addition(nil,y,ak_carry)
							  end
|	ak_addition (a::x, b::y, c) = let
									val ak_sum = (a+b+c) mod 10000
									val ak_carry = (a+b+c) div 10000
								  in
									ak_sum::ak_addition(x,y,ak_carry)
								  end;

(*passes the number to add according to the demand*)
fun ak_add (x,y) = ak_removeLeadZeroes(ak_reverse(ak_addition(ak_reverse(x,[]),ak_reverse(y,[]),0),[]));


(*Subtraction code*)
(*first we will start from ak_grSub and then compare and then pass the two number lists to the ak_subtraction. And also remember
that x will always be greater than y when we pass it to the ak_subtraction function*)


(*getting the number ready for any operation by padding bits according to the requirement*)
(*it makes the length of x, y equal*)
fun ak_pad(x, y) = let
					val (akXLen, akYLen) = (ak_length(x),ak_length(y))
				   in
					(if(akXLen>akYLen) then x else ak_paddingLeft(x,akYLen-akXLen), if(akXLen>akYLen) then ak_paddingLeft(y,akXLen-akYLen) else y)
				   end;


(*Subtracts the two list of integers based on the assumption that two lists are of same length and x is greater than y*)
fun ak_subtraction(nil, nil, 0) = [0]
|	ak_subtraction(a::x, nil, borrow) = let
											val (ak_difference,ak_borrow) = if(a >= borrow) then (a-borrow,0) else (a+10000-borrow,1)
										in
											ak_difference::ak_subtraction(x,nil,ak_borrow)
										end
|	ak_subtraction (a::x, b::y, borrow) = let
											val (ak_difference,ak_borrow) = if(a >= b+borrow) then (a-b-borrow,0) else (a+10000-b-borrow,1)
										  in
											ak_difference::ak_subtraction(x,y,ak_borrow)
										  end;


(*Compare the two numbers and returns 0 when both are equal, 1 when x > y else -1*)
fun ak_comparision(nil, nil) = 0 
|	ak_comparision(x,nil) = 1
|	ak_comparision(nil, x) = ~1
|	ak_comparision(a::x,b::y) = if a > b  then 1
								else if a < b then ~1
								else ak_comparision(x,y);


(*Getting the numbers ready for the subtraction and gives the result in mod value*)
fun ak_grSub (x, y) = let
						val (ak_first,ak_second) = ak_pad(x, y)
						val ak_compare = ak_comparision(ak_first, ak_second)
						val ak_firstFinal = if(ak_compare = 1) then ak_first else ak_second
						val ak_secondFinal = if(ak_compare = 1) then ak_second else ak_first
					  in
						if(ak_compare = 0) then [0]
						else ak_removeLeadZeroes(ak_reverse(ak_subtraction(ak_reverse(ak_firstFinal,[]),ak_reverse(ak_secondFinal,[]),0),[]))
					  end;


(*Karatsuba algorithm*)

(*returns the sign of x-y the two numbers*)
fun ak_sgn(nil, nil) = 1 
|	ak_sgn(x,nil) = 1
|	ak_sgn(nil, x) = ~1
|	ak_sgn(a::x,b::y) = if a > b  then 1
								else if a < b then ~1
								else ak_sgn(x,y);


(*Check whether the whole list is zero or not*)
fun ak_checkZeroes(nil, nil) = true
|	ak_checkZeroes(a::x, nil) = if(a = 0) then ak_checkZeroes(x, nil) else false
|	ak_checkZeroes(nil, a::y) = if(a = 0) then ak_checkZeroes(nil, y) else false
|	ak_checkZeroes(a::x, b::y) = if(a = 0 andalso b = 0) then ak_checkZeroes(x , y) else if (a = 0) then ak_checkZeroes(x, nil) else if(b = 0) then ak_checkZeroes(nil, y) else false;


(*this function will apply the karatsuba algorithm and returns the result of multiplication*)
fun ak_kar x y = if(ak_length(x) = 1) then if (hd(x)*hd(y)) < 10000 then [hd(x)*hd(y)] else ((hd(x)*hd(y)) div 10000)::((hd(x)*hd(y)) mod 10000)::[]
				 else if ak_checkZeroes(x, y) then [0] 
				 else
				 let
					val ak_max1 = ak_length(x)
					val ak_max = (ak_max1 + 1) div 2
					val (x1, x0) = ak_break([],x, ak_max1 div 2)
					val (y1, y0) = ak_break([],y, ak_max1 div 2)
					val z0 = ak_kar x0 y0
					val z2 = ak_kar x1 y1
					val z10i = ak_grSub(x0,x1)
					val z11i = ak_grSub(y1,y0) 
						val (z10,z11) = ak_pad(z10i, z11i)
					val z1f = ak_kar z10 z11
					val (x0f,x1f) = ak_pad(x0,x1)
					val (y0f,y1f) = ak_pad(y0,y1)
					val z1i = (ak_sgn(x0f,x1f)*ak_sgn(y1f,y0f))
					val z1c = ak_add(z2, z0)
					val z1 = if z1i < 0 then ak_grSub(z1c,z1f)
							 else ak_add(z1f, z1c)
				 in
					ak_add(ak_add(z0,ak_paddingRight(z2,ak_max*2)),ak_paddingRight(z1,ak_max))
				 end;

(*gets the numbers ready to apply the karatsuba algorithm*)
fun karatsuba x y = let
						val (ak_first,ak_second) = ak_pad(x, y)
					in
						ak_removeLeadZeroes(ak_kar ak_first ak_second)
					end;


(*Factorial code*)

(*Pairs the list of 4 strings into one single string and do it recursively for the whole list*)
fun ak_pair(nil) = nil
|	ak_pair(x) = hd(x)^hd(tl(x))^hd(tl(tl(x)))^hd(tl(tl(tl(x))))::ak_pair(tl(tl(tl(tl(x)))));


(*Split the String to the list of integers of base 10^4*)
fun fromString(x) =  let 
					   val ak_len = ak_length(explode(x))
					   val ak_x = if x = ""  then ""
					   else if ak_len mod 4 = 0 then  x
					   else if ak_len mod 4 = 1 then "000"^x
					   else if ak_len mod 4 = 2 then "00"^x
					   else "0"^x
					   val y = map Char.toCString(explode(ak_x))
				   in
					   map valOf(map Int.fromString(ak_pair(y)))
				   end;


(*do the factorial of the given number where the number should be in the list format*)
fun actualF(x) = if(ak_length(x) = 1 andalso hd(x) <= 0) then [1] 
				 else let 
					    val f = ak_removeLeadZeroes(ak_grSub(x,[1]))
						val c = actualF(f)
					  in karatsuba x c 
					  end;


(*This will check whether the input is valid of not*)
fun check(nil,n) = 1
|	check(n,m) = if(hd(n) < #"0" orelse hd(n)> #"9")then raise Invalid_Input_exception(m) else check(tl(n),m);

(*get the number ready to do the factorial of it and then pass it to actualF which in return gives*)
fun factorial(n) =	
					let
						val dummy = if(null(explode(n))) then raise Invalid_Input_exception(n) else check(explode(n),n)
						val z = ak_removeLeadingZeroes(explode(n))
						val StringList = map Char.toCString(z)(*This will give the list of string which will contain every character eg "a" "1" "$" etc.*)
						val actualNo = fromString(toStringc(StringList))
					in 
						toString(actualF(actualNo))
					end
					handle Invalid_Input_exception(n) => if n<> "" then (print("Invalid_Input_exception of "^n^"\n");"") else  (print("Invalid_Input_exception of Empty String\n");"")
 

@.str = private constant [4 x i8] c%d0A00

declare i32 @printf(i8*, ...)

define i32 @printInt(i32 %x) {
  %t0 = getelementptr [4 x i8]* @.str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %s)
  ret i32 %x
}


define i32 @zero(i32 x){
	ret i32 0
}


define i32 @suc(i32 x){
	%tmp105 = add i32 %x, 1
	ret i32 %tmp105
}


define i32 @pred(i32 x){
	%tmp101 = eq %x, 0
	br i1 tmp101, label if_branch103, label else_branch104
if_branch103:
	ret i32 %x
else_branch104:
	%tmp102 = sub i32 %x, 1
	ret i32 %tmp102
}


define i32 @add(i32 x,i32 y){
	%tmp95 = eq %x, 0
	br i1 tmp95, label if_branch99, label else_branch100
if_branch99:
	ret i32 %y
else_branch100:
	%tmp96 = sub i32 %x, 1
	%tmp97 = call i32 @add (i32 %tmp96,i32 %y)
	%tmp98 = call i32 @suc (i32 %tmp97)
	ret i32 %tmp98
}


define i32 @mult(i32 x,i32 y){
	%tmp89 = eq %x, 0
	br i1 tmp89, label if_branch93, label else_branch94
if_branch93:
	ret i32 0
else_branch94:
	%tmp90 = sub i32 %x, 1
	%tmp91 = call i32 @mult (i32 %tmp90,i32 %y)
	%tmp92 = call i32 @add (i32 %y,i32 %tmp91)
	ret i32 %tmp92
}


define i32 @pow(i32 x,i32 y){
	%tmp83 = eq %y, 0
	br i1 tmp83, label if_branch87, label else_branch88
if_branch87:
	ret i32 1
else_branch88:
	%tmp84 = sub i32 %y, 1
	%tmp85 = call i32 @pow (i32 %x,i32 %tmp84)
	%tmp86 = call i32 @mult (i32 %x,i32 %tmp85)
	ret i32 %tmp86
}


define i32 @fib(i32 n){
	%tmp72 = eq %n, 0
	br i1 tmp72, label if_branch79, label else_branch80
if_branch79:
	ret i32 0
else_branch80:
	%tmp73 = eq %n, 1
	br i1 tmp73, label if_branch81, label else_branch82
if_branch81:
	ret i32 1
else_branch82:
	%tmp75 = sub i32 %n, 1
	%tmp76 = call i32 @fib (i32 %tmp75)
	%tmp77 = sub i32 %n, 2
	%tmp78 = call i32 @fib (i32 %tmp77)
	%tmp74 = add i32 %tmp76, %tmp78
	ret i32 %tmp74
}


define i32 @fact(i32 n){
	%tmp66 = eq %n, 0
	br i1 tmp66, label if_branch70, label else_branch71
if_branch70:
	ret i32 1
else_branch71:
	%tmp68 = sub i32 %n, 1
	%tmp69 = call i32 @fact (i32 %tmp68)
	%tmp67 = mul i32 %n, %tmp69
	ret i32 %tmp67
}


define i32 @ack(i32 m,i32 n){
	%tmp53 = eq %m, 0
	br i1 tmp53, label if_branch62, label else_branch63
if_branch62:
	%tmp61 = add i32 %n, 1
	ret i32 %tmp61
else_branch63:
	%tmp54 = eq %n, 0
	br i1 tmp54, label if_branch64, label else_branch65
if_branch64:
	%tmp59 = sub i32 %m, 1
	%tmp60 = call i32 @ack (i32 %tmp59,i32 1)
	ret i32 %tmp60
else_branch65:
	%tmp55 = sub i32 %m, 1
	%tmp56 = sub i32 %n, 1
	%tmp57 = call i32 @ack (i32 %m,i32 %tmp56)
	%tmp58 = call i32 @ack (i32 %tmp55,i32 %tmp57)
	ret i32 %tmp58
}


define i32 @stack_test(i32 x){
	%tmp52 = add i32 4, 5
	%tmp51 = add i32 3, %tmp52
	%tmp50 = add i32 2, %tmp51
	%tmp49 = add i32 1, %tmp50
	%tmp48 = add i32 %x, %tmp49
	ret i32 %tmp48
}


define i32 @div(i32 x,i32 y){
	%tmp47 = udiv i32 %x, %y
	ret i32 %tmp47
}


define i32 @rem(i32 x,i32 y){
	%tmp46 = urem i32 %x, %y
	ret i32 %tmp46
}


define i32 @gcd(i32 a,i32 b){
	%tmp41 = eq %b, 0
	br i1 tmp41, label if_branch44, label else_branch45
if_branch44:
	ret i32 %a
else_branch45:
	%tmp42 = urem i32 %a, %b
	%tmp43 = call i32 @gcd (i32 %b,i32 %tmp42)
	ret i32 %tmp43
}


define i32 @is_prime_aux(i32 n,i32 i){
	%tmp32 = urem i32 %n, %i
	%tmp31 = eq %tmp32, 0
	br i1 tmp31, label if_branch37, label else_branch38
if_branch37:
	ret i32 0
else_branch38:
	%tmp34 = mul i32 %i, %i
	%tmp33 = sle %tmp34, %n
	br i1 tmp33, label if_branch39, label else_branch40
if_branch39:
	%tmp35 = add i32 %i, 1
	%tmp36 = call i32 @is_prime_aux (i32 %n,i32 %tmp35)
	ret i32 %tmp36
else_branch40:
	ret i32 1
}


define i32 @is_prime(i32 n){
	%tmp27 = eq %n, 2
	br i1 tmp27, label if_branch29, label else_branch30
if_branch29:
	ret i32 1
else_branch30:
	%tmp28 = call i32 @is_prime_aux (i32 %n,i32 2)
	ret i32 %tmp28
}


define i32 @primes(i32 n){
	%tmp15 = eq %n, 0
	br i1 tmp15, label if_branch23, label else_branch24
if_branch23:
	ret i32 0
else_branch24:
	%tmp17 = call i32 @is_prime (i32 %n)
	%tmp16 = eq %tmp17, 1
	br i1 tmp16, label if_branch25, label else_branch26
if_branch25:
	%tmp20 = call i32 @printInt(i32 %n)
	%tmp21 = sub i32 %n, 1
	%tmp22 = call i32 @primes (i32 %tmp21)
	ret i32 %tmp22
else_branch26:
	%tmp18 = sub i32 %n, 1
	%tmp19 = call i32 @primes (i32 %tmp18)
	ret i32 %tmp19
}


define i32 @is_collatz(i32 n){
	%tmp2 = eq %n, 1
	br i1 tmp2, label if_branch11, label else_branch12
if_branch11:
	ret i32 1
else_branch12:
	%tmp4 = urem i32 %n, 2
	%tmp3 = eq %tmp4, 0
	br i1 tmp3, label if_branch13, label else_branch14
if_branch13:
	%tmp9 = udiv i32 %n, 2
	%tmp10 = call i32 @is_collatz (i32 %tmp9)
	ret i32 %tmp10
else_branch14:
	%tmp6 = mul i32 3, %n
	%tmp5 = add i32 %tmp6, 1
	%tmp7 = call i32 @is_collatz (i32 %tmp5)
	%tmp8 = add i32 1, 1
	ret i32 %tmp8
}


define i32 @main()
	%tmp1 = call i32 @primes (i32 1000)
	ret i32 0
}


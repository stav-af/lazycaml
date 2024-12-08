let prelude = {|

@.str = private constant [4 x i8] c"%d\0A\00"
@.strl = private constant [5 x i8] c"%ld\0A\00"  ; "%ld\n\0"

declare i32 @printf(i8*, ...)
define i32 @printInt(i32 %x) {
entry:
  %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
  ret i32 %x
}
define i64 @printLInt(i64 %x) {
entry:
    %t0 = getelementptr [5 x i8], [5 x i8]* @.strl, i64 0, i64 0
    call i32 (i8*, ...) @printf(i8* %t0, i64 %x)
    ret i64 %x
}
%Token = type { i8*, i64, i64 }

%CacheEntry = type { i64, i64 }

@GLOBAL_CACHE_SIZE = constant i64 2048
@global_cache = global [2048 x %CacheEntry] zeroinitializer, align 8
@GLOBAL_PRIME = constant i64 1315423911

declare i8* @malloc(i64)
declare void @free(i8*)

define i64 @compute_hash(i8* %fptr, i64 %arg_count, i64* %values) {
entry:
  %hash_init = ptrtoint i8* %fptr to i64
  br label %loop_cond

loop_cond:
  %i = phi i64 [0, %entry], [%i_next, %loop_body]
  %accum = phi i64 [%hash_init, %entry], [%hash_next, %loop_body]
  %cond = icmp slt i64 %i, %arg_count
  br i1 %cond, label %loop_body, label %loop_end

loop_body:
  %c_ptr = getelementptr i64, i64* %values, i64 %i
  %curr = load i64, i64* %c_ptr
  %tmp22 = mul i64 %curr, 1315423911
  %hash_next = xor i64 %accum, %tmp22
  %i_next = add i64 %i, 1
  br label %loop_cond

loop_end:
  ret i64 %accum
}

define i64 @eq_func(i64* %args) {
entry:
  %arg0_ptr = getelementptr i64, i64* %args, i64 0
  %arg0 = load i64, i64* %arg0_ptr

  %arg1_ptr = getelementptr i64, i64* %args, i64 1
  %arg1 = load i64, i64* %arg1_ptr

  %is_equal = icmp eq i64 %arg0, %arg1
  %result = zext i1 %is_equal to i64

  ret i64 %result
}

define i64 @add_func(i64* %args) {
entry:
  %arg0_ptr = getelementptr i64, i64* %args, i64 0
  %arg0 = load i64, i64* %arg0_ptr
  %arg1_ptr = getelementptr i64, i64* %args, i64 1
  %arg1 = load i64, i64* %arg1_ptr
  %sum64 = add i64 %arg0, %arg1
  ret i64 %sum64
}

define i64 @mul_func(i64* %args) {
entry:
  %arg0_ptr = getelementptr i64, i64* %args, i64 0
  %arg0 = load i64, i64* %arg0_ptr
  %arg1_ptr = getelementptr i64, i64* %args, i64 1
  %arg1 = load i64, i64* %arg1_ptr
  %mul64 = mul i64 %arg0, %arg1
  ret i64 %mul64
}

define i64 @sub_func(i64* %args) {
entry:
  %arg0_ptr = getelementptr i64, i64* %args, i64 0
  %arg0 = load i64, i64* %arg0_ptr
  %arg1_ptr = getelementptr i64, i64* %args, i64 1
  %arg1 = load i64, i64* %arg1_ptr
  %diff64 = sub i64 %arg0, %arg1
  ret i64 %diff64
}

define i64 @div_func(i64* %args) {
entry:
  %arg0_ptr = getelementptr i64, i64* %args, i64 0
  %arg0 = load i64, i64* %arg0_ptr

  %arg1_ptr = getelementptr i64, i64* %args, i64 1
  %arg1 = load i64, i64* %arg1_ptr

  %div64 = udiv i64 %arg0, %arg1

  ret i64 %div64
}

define i64 @mod_func(i64* %args) {
entry:
  %arg0_ptr = getelementptr i64, i64* %args, i64 0
  %arg0 = load i64, i64* %arg0_ptr

  %arg1_ptr = getelementptr i64, i64* %args, i64 1
  %arg1 = load i64, i64* %arg1_ptr

  %mod64 = urem i64 %arg0, %arg1

  ret i64 %mod64
}

define %Token* @engine_lazy(i8* %fptr, i64 %arg_count, i64 %args_or_value) {
entry:
  %token_size_ptr = getelementptr %Token, %Token* null, i32 1
  %token_size = ptrtoint %Token* %token_size_ptr to i64

  %token_mem = call i8* @malloc(i64 %token_size)
  %token = bitcast i8* %token_mem to %Token*

  %fptr_slot = getelementptr %Token, %Token* %token, i32 0, i32 0
  store i8* %fptr, i8** %fptr_slot

  %arg_count_slot = getelementptr %Token, %Token* %token, i32 0, i32 1
  store i64 %arg_count, i64* %arg_count_slot

  %args_slot = getelementptr %Token, %Token* %token, i32 0, i32 2
  store i64 %args_or_value, i64* %args_slot

  ret %Token* %token
}

define i64 @engine_strict(%Token* %token) {
entry:
  %fptr_slot = getelementptr %Token, %Token* %token, i32 0, i32 0
  %fptr = load i8*, i8** %fptr_slot

  %is_terminal = icmp eq i8* %fptr, null
  br i1 %is_terminal, label %terminal, label %non_terminal

terminal:
  %val_slot = getelementptr %Token, %Token* %token, i32 0, i32 2
  %val64 = load i64, i64* %val_slot
  ret i64 %val64

non_terminal:
  %arg_count_slot = getelementptr %Token, %Token* %token, i32 0, i32 1
  %arg_count = load i64, i64* %arg_count_slot

  %args_slot_nt = getelementptr %Token, %Token* %token, i32 0, i32 2
  %args_int = load i64, i64* %args_slot_nt
  %args_ptr_nt = inttoptr i64 %args_int to %Token**

  %arg_values_mem = alloca i64, i64 %arg_count
  br label %loop_cond

loop_cond:
  %i = phi i64 [0, %non_terminal], [%i_next, %loop_body]
  %cond = icmp slt i64 %i, %arg_count
  br i1 %cond, label %loop_body, label %loop_end

loop_body:
  %arg_token_ptr = getelementptr %Token*, %Token** %args_ptr_nt, i64 %i
  %arg_token = load %Token*, %Token** %arg_token_ptr
  %arg_val = call i64 @engine_strict(%Token* %arg_token)

  %dest_ptr = getelementptr i64, i64* %arg_values_mem, i64 %i
  store i64 %arg_val, i64* %dest_ptr
  %i_next = add i64 %i, 1
  br label %loop_cond

loop_end:
  %fptr_loaded = load i8*, i8** %fptr_slot
  %hash = call i64 @compute_hash(i8* %fptr_loaded, i64 %arg_count, i64* %arg_values_mem)

  %cache_size = load i64, i64* @GLOBAL_CACHE_SIZE
  %slot = urem i64 %hash, %cache_size

  %cache_ptr = getelementptr [2048 x %CacheEntry], [2048 x %CacheEntry]* @global_cache, i64 0, i64 %slot
  %stored_hash_ptr = getelementptr %CacheEntry, %CacheEntry* %cache_ptr, i32 0, i32 0
  %stored_val_ptr  = getelementptr %CacheEntry, %CacheEntry* %cache_ptr, i32 0, i32 1

  %stored_hash = load i64, i64* %stored_hash_ptr
  %hash_match = icmp eq i64 %stored_hash, %hash
  br i1 %hash_match, label %cache_hit, label %cache_miss

cache_hit:
  %cached_val = load i64, i64* %stored_val_ptr
  ret i64 %cached_val

cache_miss:
  %fptr_call = bitcast i8* %fptr_loaded to i64 (i64*)*
  %call_args = bitcast i64* %arg_values_mem to i64*
  %res = call i64 %fptr_call(i64* %call_args)
  store i64 %hash, i64* %stored_hash_ptr
  store i64 %res, i64* %stored_val_ptr

  ret i64 %res
}

define %Token* @token_add(%Token* %t1, %Token* %t2) {
entry:
  %size = mul i64 2, 8
  %arg_mem = call i8* @malloc(i64 %size)
  %arg_tokens = bitcast i8* %arg_mem to %Token**
  
  %arg0 = getelementptr %Token*, %Token** %arg_tokens, i64 0
  store %Token* %t1, %Token** %arg0
  %arg1 = getelementptr %Token*, %Token** %arg_tokens, i64 1
  store %Token* %t2, %Token** %arg1

  %arg_int = ptrtoint %Token** %arg_tokens to i64

  %add_fptr = bitcast i64 (i64*)* @add_func to i8*
  %res_token = call %Token* @engine_lazy(i8* %add_fptr, i64 2, i64 %arg_int)
  ret %Token* %res_token
}

define %Token* @token_mul(%Token* %t1, %Token* %t2) {
entry:
  %size = mul i64 2, 8
  %arg_mem = call i8* @malloc(i64 %size)
  %arg_tokens = bitcast i8* %arg_mem to %Token**
  
  %arg0 = getelementptr %Token*, %Token** %arg_tokens, i64 0
  store %Token* %t1, %Token** %arg0
  %arg1 = getelementptr %Token*, %Token** %arg_tokens, i64 1
  store %Token* %t2, %Token** %arg1

  %arg_int = ptrtoint %Token** %arg_tokens to i64
  %mul_fptr = bitcast i64 (i64*)* @mul_func to i8*
  %res_token = call %Token* @engine_lazy(i8* %mul_fptr, i64 2, i64 %arg_int)
  ret %Token* %res_token
}

define %Token* @token_eq(%Token* %t1, %Token* %t2) {
entry:
  %size = mul i64 2, 8
  %arg_mem = call i8* @malloc(i64 %size)
  %arg_tokens = bitcast i8* %arg_mem to %Token**

  %arg0 = getelementptr %Token*, %Token** %arg_tokens, i64 0
  store %Token* %t1, %Token** %arg0

  %arg1 = getelementptr %Token*, %Token** %arg_tokens, i64 1
  store %Token* %t2, %Token** %arg1
  %arg_int = ptrtoint %Token** %arg_tokens to i64

  %eq_fptr = bitcast i64 (i64*)* @eq_func to i8*
  %res_token = call %Token* @engine_lazy(i8* %eq_fptr, i64 2, i64 %arg_int)

  ret %Token* %res_token
}

define %Token* @token_sub(%Token* %t1, %Token* %t2) {
entry:
  %size = mul i64 2, 8
  %arg_mem = call i8* @malloc(i64 %size)
  %arg_tokens = bitcast i8* %arg_mem to %Token**

  %arg0 = getelementptr %Token*, %Token** %arg_tokens, i64 0
  store %Token* %t1, %Token** %arg0

  %arg1 = getelementptr %Token*, %Token** %arg_tokens, i64 1
  store %Token* %t2, %Token** %arg1

  %arg_int = ptrtoint %Token** %arg_tokens to i64
  %sub_fptr = bitcast i64 (i64*)* @sub_func to i8*

  %res_token = call %Token* @engine_lazy(i8* %sub_fptr, i64 2, i64 %arg_int)

  ret %Token* %res_token
}

define %Token* @token_div(%Token* %t1, %Token* %t2) {
entry:
  %size = mul i64 2, 8
  %arg_mem = call i8* @malloc(i64 %size)
  %arg_tokens = bitcast i8* %arg_mem to %Token**

  %arg0 = getelementptr %Token*, %Token** %arg_tokens, i64 0
  store %Token* %t1, %Token** %arg0

  %arg1 = getelementptr %Token*, %Token** %arg_tokens, i64 1
  store %Token* %t2, %Token** %arg1
  %arg_int = ptrtoint %Token** %arg_tokens to i64
  %div_fptr = bitcast i64 (i64*)* @div_func to i8*

  %res_token = call %Token* @engine_lazy(i8* %div_fptr, i64 2, i64 %arg_int)

  ret %Token* %res_token
}

define %Token* @token_mod(%Token* %t1, %Token* %t2) {
entry:
  %size = mul i64 2, 8
  %arg_mem = call i8* @malloc(i64 %size)
  %arg_tokens = bitcast i8* %arg_mem to %Token**

  %arg0 = getelementptr %Token*, %Token** %arg_tokens, i64 0
  store %Token* %t1, %Token** %arg0

  %arg1 = getelementptr %Token*, %Token** %arg_tokens, i64 1
  store %Token* %t2, %Token** %arg1

  %arg_int = ptrtoint %Token** %arg_tokens to i64
  %mod_fptr = bitcast i64 (i64*)* @mod_func to i8*

  %res_token = call %Token* @engine_lazy(i8* %mod_fptr, i64 2, i64 %arg_int)

  ret %Token* %res_token
}

|}
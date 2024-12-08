let _defs: string = 
{|
def fib(n) = write(n); if n == 0 then 0 
             else if n == 1 then 1 
             else fib(n - 1) + fib(n - 2);


def fact(n) = if n == 0 then 1
              else n * fact(n - 1);

(write(fib(92));write(fib(fact(3))))
|}























let _fact: string = 
{|
def fact(n) =
  (if n == 0 then 1 else n * fact(n - 1));

def facT(n, acc) =
  if n == 0 then acc else facT(n - 1, n * acc);

def facTi(n) = facT(n, 1);

//fact(10)
//facTi(10)

write(fact(6)); facTi(6)

// a simple factorial program
// (including a tail recursive version)
|}

let _add: string = 
{|
def add(x, y) =
  if x == 0 then y else 1 + (add(x - 1, y));

def addT(x, y) =
  if x == 0 then y else addT(x - 1, y + 1);

(write(add(1000, 1000)); addT(100000,100000))
|}
<!-- ctrl+shift+v to see preview,  ctrl+shift+i to arrange tables in VS Code-->

# TinyCLisp
hand-made small subset of Common Lisp processor written in Python3


## Usage
To run an interpreter, type
> python3 TinyLisp.py [--debug] [--prompt "prompt message here"]

To execute your common lisp all in once, type
> python3 TinyLisp.py [--file "your subset of Common Lisp file here"]

To see help, type
> python3 TinyLisp.py -h

<br>

## Features

### 1. Lisp-2 system
```lisp
(let ((x 3)) (let* ((x 1) (y (+ x 2))) (+ x y)))
; => 4

(let ((f-2 20)) (defun f-2 (x) (+ x 2)) f-2)
; => 20

(let ((f-3 20)) (defun f-3 (x) (+ x 3)) (f-3 3))
; => 6
```

```lisp
(flet ((foo (x) (+ x 2)) (bar (x) (+ x 5))) (let ((foo 100)) (bar (foo 3))))
; => 10

(labels ((foo (x) (+ x 2)) (bar (x) (+ (foo x) 2))) (bar 3))
; => 7
```

### 2. Scope chain with dynamic scope and lexical scope
(ordinary lexical scope)
```lisp
(setq lexical-var 1) 
(defun access-lexical () lexical-var) 
(let ((lexical-var 100)) (access-lexical))
; => 1
```

(special variable has dynamic scope!)
```lisp
(defparameter *global-var* 1)
(defun access-dynamic () *global-var*) 
(let ((*global-var* 100)) (access-dynamic))
; => 100
```

<br>

## Currently supported functions

The built-in variable that is supported is:
- `pi`

Currently supported built-in special form, symbol, and macros are as below:
- `quote (')` , ```quasiquote (`)``` , `unquote (,)` , `unquote-splicing (,@)` , `funcquote (#')` ,
- `t`, `nil`, `otherwise`, `return`, `return-from`,
- `if`, `progn`, `prog1`, `when`, `unless`, `cond`, `case`, `block`, `loop`, `dotimes`, `dolist`,
- `lambda`, `defun`, `setq`, `defvar`, `defparameter`, `let`, `let*`, `flet`, `labels`
  
Currently supported built-in functions are as follows:
- `acos`, `acosh`, `asin`, `asinh`, `atan`, `atanh`, `cos`, `cosh`, `sin`, `sinh`, `tan`, `tanh`, `exp`, `expt`, `log`, `gcd`, `mod`, `rem`, `imagpart`, `realpart`, `isqrt`, `sqrt`, `1+`, `1-`, 
- `+`, `-`, `*`, `/`, `>`, `<`, `>=`, `<=`, `=`, `eq`, `equal`, `not`,
- `length`, `cons`, `append`, `list`, `car`, `first`, `cdr`, `rest`, `last`, `caar`, `cadr`, `cdar`, `cddr`,
- `consp`, `listp`, `null`, `symbolp`, `numberp`, `oddp`, `evenp`, 
- `float`, `complex`, `print`, `apply`, `funcall`
  
<br>


## Data Type
```
data -- list
     |- atom -- symbol
             |- string
             |- number -- integer
                       |- float
                       |- ratio
                       |- complex
```

| input   | data type inside the interpreter | super class          |
| :------ | :------------------------------- | :------------------- |
| list    | list (processed recursively)     |                      |
| symbol  | LispSymbol                       | str                  |
| string  | LispStr                          | str                  |
| integer | LispInt                          | int, LispNumber      |
| float   | LispFloat                        | float, LispNumber    |
| ratio   | LispRatio                        | Fraction, LispNumber |
| complex | LispComplex                      | complex, LispNumber  |

When tokenizing and parsing, the input s-expression is converted based on the table above. When evaluating, the special symol is converted to `LispSpecial` and this works as kaind of mark that shows whether the variable we refers to from a certain scope is special variable or ordinary variable. So as to see how the tokenizer and parser and evaluator works, use *--debug* option.

<br>

## (Limitations)
- you cannot use complex literal such as `#c(1, 2)`
- 'non-nil terminated cons' is not supported...! :[
- user-defined macro system has not yet been implemented.


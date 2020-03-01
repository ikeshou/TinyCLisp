#coding: UTF-8
"""
Unit Test for TinyLisp.py
"""

import unittest
from lisp_tokenizer import LispTokenizer
from lisp_parser import LispParser
from  lisp_evaluator import LispEvaluator
from TinyLisp import arrange
from fractions import Fraction


class TestInterpreter(unittest.TestCase):
    """test class for the TinyLisp interpreter"""
    def setUp(self):
        self.t = LispTokenizer()
        self.p = LispParser()
        self.e = LispEvaluator()
        self.all_in_one = lambda s_exp: arrange(self.e.evaluate(self.p.parse(self.t.tokenize(s_exp))))

    # expected, actual
    def test_atom(self):
        print("test >>> atom")
        self.assertEqual('a', self.all_in_one("'a"))
        self.assertEqual('"string with space"', self.all_in_one('"string with space"'))
        self.assertEqual('"string with \'(quote)"', self.all_in_one('"string with \'(quote)"'))
        self.assertEqual('1', self.all_in_one('1'))
        self.assertEqual('1.0', self.all_in_one('1.0'))
        self.assertEqual('1/2', self.all_in_one('1/2'))
        self.assertEqual('(1+2j)', self.all_in_one('(complex 1 2)'))    # complex is also tested
        self.assertEqual('t', self.all_in_one('t'))
        self.assertEqual('nil', self.all_in_one('nil'))
    

    def test_builtin_variable(self):
        print("test >>> built-in variable")
        self.assertAlmostEqual('3.141592653589793', self.all_in_one('pi'))
        
    
    def test_quote_family(self):
        print("test >>> quote family")
        self.assertEqual('(list 1 2 3)', self.all_in_one("'(list 1 2 3)"))
        self.assertEqual('(list 1 2 (3 4))', self.all_in_one("`(list 1 2 ,(list 3 4))"))
        self.assertEqual('(list 1 2 4 8 16 num)', self.all_in_one("(let ((L '(4 8)) (num 32)) `(list 1 2 ,@L 16 num))"))    # cons, append, let are also tested
        self.assertEqual('(list 1 2 4 8 16 32)', self.all_in_one("(let ((L '(4 8)) (num 32)) `(list 1 2 ,@L 16 ,num))"))    # cons, append, let are also tested
        self.assertEqual('30', self.all_in_one("(flet ((f (x) (+ x 10))) (funcall #'f 20))"))   # +, flet, funcall are also tested
        

    def test_builtin_functions(self):    
        print("test >>> built-in functions")
        # 算術
        self.assertAlmostEqual(eval('1.5707964'), eval(self.all_in_one('(acos 0)')), places=6)
        self.assertAlmostEqual(eval('1.316958'), eval(self.all_in_one('(acosh 2)')), places=6)
        self.assertAlmostEqual(eval('1.5707964'), eval(self.all_in_one('(asin 1)')), places=6)
        self.assertAlmostEqual(eval('1.4436355'), eval(self.all_in_one('(asinh 2)')), places=6)
        self.assertAlmostEqual(eval('0.7853982'), eval(self.all_in_one('(atan 1)')), places=6)
        self.assertAlmostEqual(eval('0.54930615'), eval(self.all_in_one('(atanh 0.5)')), places=6)
        self.assertAlmostEqual(eval('-1.0'), eval(self.all_in_one('(cos pi)')), places=6)
        self.assertAlmostEqual(eval('1.5430807'), eval(self.all_in_one('(cosh 1)')), places=6)
        self.assertAlmostEqual(eval('0.0'), eval(self.all_in_one('(sin pi)')), places=6)
        self.assertAlmostEqual(eval('1.1752012'), eval(self.all_in_one('(sinh 1)')), places=6)
        self.assertAlmostEqual(eval('0.0'), eval(self.all_in_one('(tan pi)')), places=6)
        self.assertAlmostEqual(eval('0.7615942'), eval(self.all_in_one('(tanh 1)')), places=6)
        self.assertAlmostEqual(eval('7.389056'), eval(self.all_in_one('(exp 2)')), places=6)
        self.assertAlmostEqual(eval('8.0'), eval(self.all_in_one('(expt 2 3)')), places=6)
        self.assertAlmostEqual(eval('3.0'), eval(self.all_in_one('(log 8 2)')), places=6)
        self.assertEqual('7', self.all_in_one('(gcd 21 35)'))
        self.assertEqual('1', self.all_in_one('(mod 10 3)'))
        self.assertEqual('2', self.all_in_one('(mod -10 3)'))
        self.assertEqual('1', self.all_in_one('(rem 10 3)'))
        self.assertEqual('-1', self.all_in_one('(rem -10 3)'))
        self.assertEqual('10.0', self.all_in_one('(realpart (complex 10 20))'))
        self.assertEqual('20.0', self.all_in_one('(imagpart (complex 10 20))'))
        self.assertEqual('3', self.all_in_one('(isqrt 10)'))
        self.assertEqual('5.0', self.all_in_one('(sqrt 25)'))
        self.assertEqual('2', self.all_in_one('(1+ 1)'))
        self.assertEqual('0', self.all_in_one('(1- 1)'))
        # 演算子
        self.assertEqual('18', self.all_in_one('(+ 9 9)'))
        self.assertEqual('27', self.all_in_one('(+ 9 9 9)'))
        self.assertEqual('0', self.all_in_one('(- 9 9)'))
        self.assertEqual('81', self.all_in_one('(* 9 9)'))
        self.assertEqual('729', self.all_in_one('(* 9 9 9)'))
        self.assertEqual('1.0', self.all_in_one('(/ 9 9)'))
        self.assertEqual('t', self.all_in_one('(> 3 2)'))
        self.assertEqual('nil', self.all_in_one('(< 3 2)'))
        self.assertEqual('t', self.all_in_one('(<= 3 3)'))
        self.assertEqual('t', self.all_in_one('(>= 3 3)'))
        self.assertEqual('nil', self.all_in_one('(= 3 2)'))
        self.assertEqual('t', self.all_in_one("(eq 'a 'a)"))
        self.assertEqual('nil', self.all_in_one("(eq '(1 2) '(1 2))"))
        self.assertEqual('t', self.all_in_one("(equal '(1 2) '(1 2))"))
        self.assertEqual('nil', self.all_in_one("(not '(1 2 3))"))
        self.assertEqual('t', self.all_in_one("(not '())"))
        self.assertEqual('t', self.all_in_one("(not nil)"))
        self.assertEqual('nil', self.all_in_one("(not t)"))
        # リスト
        self.assertEqual('5', self.all_in_one("(length '(1 2 3 (4 4) 5))"))
        self.assertEqual('(1 2)', self.all_in_one("(cons 1 '(2))"))
        self.assertEqual('(1)', self.all_in_one("(cons 1 nil)"))
        self.assertEqual('(1 2)', self.all_in_one("(append '(1) '(2))"))
        self.assertEqual('(1)', self.all_in_one("(append '(1) nil)"))
        self.assertEqual('(1 1.0 "hoge")', self.all_in_one('(list 1 1.0 "hoge")'))
        self.assertEqual('1', self.all_in_one("(car '(1 2 3 (4 4) 5))"))
        self.assertEqual('1', self.all_in_one("(first '(1 2 3 (4 4) 5))"))
        self.assertEqual('(2 3 (4 4) 5)', self.all_in_one("(cdr '(1 2 3 (4 4) 5))"))
        self.assertEqual('(2 3 (4 4) 5)', self.all_in_one("(rest '(1 2 3 (4 4) 5))"))
        self.assertEqual('(5)', self.all_in_one("(last '(1 2 3 (4 4) 5))"))
        self.assertEqual('1', self.all_in_one("(caar '((1 2) 3 (4 4) 5))"))
        self.assertEqual('3', self.all_in_one("(cadr '((1 2) 3 (4 4) 5))"))
        self.assertEqual('(2)', self.all_in_one("(cdar '((1 2) 3 (4 4) 5))"))
        self.assertEqual('((4 4) 5)', self.all_in_one("(cddr '((1 2) 3 (4 4) 5))"))
        # 述語
        self.assertEqual('t', self.all_in_one("(consp '(1 2 3))"))
        self.assertEqual('nil', self.all_in_one("(consp 3)"))
        self.assertEqual('nil', self.all_in_one("(consp nil)"))
        self.assertEqual('nil', self.all_in_one("(consp '())"))
        self.assertEqual('t', self.all_in_one("(listp '(1 2 3))"))
        self.assertEqual('nil', self.all_in_one("(listp 3)"))
        self.assertEqual('t', self.all_in_one("(listp nil)"))
        self.assertEqual('t', self.all_in_one("(listp '())"))
        self.assertEqual('nil', self.all_in_one("(null '(1 2 3))"))
        self.assertEqual('nil', self.all_in_one("(null 3)"))
        self.assertEqual('t', self.all_in_one("(null '())"))
        self.assertEqual('t', self.all_in_one("(null nil)"))
        self.assertEqual('nil', self.all_in_one('(symbolp "text")'))
        self.assertEqual('t', self.all_in_one("(symbolp 'a)"))
        self.assertEqual('t', self.all_in_one('(symbolp nil)'))
        self.assertEqual('t', self.all_in_one("(symbolp '())"))
        self.assertEqual('nil', self.all_in_one('(numberp nil)'))
        self.assertEqual('t', self.all_in_one('(numberp 1)'))
        self.assertEqual('t', self.all_in_one('(numberp 1.0)'))
        self.assertEqual('t', self.all_in_one('(numberp 1/2)'))
        self.assertEqual('t', self.all_in_one('(oddp 1)'))
        self.assertEqual('t', self.all_in_one('(evenp 2)'))
        # その他
        self.assertEqual('1.0', self.all_in_one('(float 1)'))
        self.assertEqual('(1+2j)', self.all_in_one('(complex 1 (1+ 1))'))
        self.assertEqual('"(This is a print function test.)"', self.all_in_one('(print "(This is a print function test.)")'))
        self.assertEqual('(1 2)', self.all_in_one("(apply #'cons '(1 (2)))"))
        self.assertEqual('10', self.all_in_one("(funcall #'+ 1 2 3 4)"))


    def test_special_form(self):
        print("test >>> special forms")
        # if, nil
        self.assertEqual('hoge', self.all_in_one("(if (= 1 1) 'hoge 'piyo)"))
        self.assertEqual('piyo', self.all_in_one("(if '() 'hoge 'piyo)"))
        self.assertEqual('piyo', self.all_in_one("(if nil 'hoge 'piyo)"))
        self.assertEqual('-1', self.all_in_one("(if (= 1 2) (+ 1 2) (- 1 2))"))
        self.assertEqual('3', self.all_in_one("(if (= 1 1) (+ 1 2))"))
        self.assertEqual('nil', self.all_in_one("(if (= 1 2) (+ 1 2))"))
        # progn
        self.assertEqual('0.5', self.all_in_one("(progn (+ 1 2) (- 1 2) (* 1 2) (/ 1 2))"))
        # prog1
        self.assertEqual('3', self.all_in_one("(prog1 (+ 1 2) (- 1 2) (* 1 2) (/ 1 2))"))
        # when
        self.assertEqual('0.5', self.all_in_one("(when (listp '(1 2 3)) (+ 1 2) (- 1 2) (* 1 2) (/ 1 2))"))
        self.assertEqual('nil', self.all_in_one("(when (numberp '(1 2 3)) (+ 1 2) (- 1 2) (* 1 2) (/ 1 2))"))
        # unless
        self.assertEqual('nil', self.all_in_one("(unless (listp '(1 2 3)) (+ 1 2) (- 1 2) (* 1 2) (/ 1 2))"))
        self.assertEqual('0.5', self.all_in_one("(unless (numberp '(1 2 3)) (+ 1 2) (- 1 2) (* 1 2) (/ 1 2))"))
        # cond, t
        self.assertEqual('cow', self.all_in_one("(let ((sound 'moo)) (cond ((eq sound 'moo) 'cow) ((eq sound 'bow) 'dog) (t 'mysterious-animal)))"))
        self.assertEqual('mysterious-animal', self.all_in_one("(let ((sound 'whoooo)) (cond ((eq sound 'moo) 'cow) ((eq sound 'bow) 'dog) (t 'mysterious-animal)))"))
        # case, otherwise
        self.assertEqual('cow', self.all_in_one("(let ((sound 'moo)) (case sound ('moo 'cow) ('bow 'dog) (otherwise 'mysterious-animal)))"))
        self.assertEqual('mysterious-animal', self.all_in_one("(let ((sound 'whoooo)) (case sound ('moo 'cow) ('bow 'dog) (otherwise 'mysterious-animal)))"))
        # block, return, return-from
        self.assertEqual('3', self.all_in_one("(block name (+ 1 2))"))
        self.assertEqual('nil', self.all_in_one("(block name (+ 1 2) (- 1 2) (return-from name) (* 1 2))"))
        self.assertEqual('returned!', self.all_in_one("(block name (+ 1 2) (- 1 2) (return-from name 'returned!) (* 1 2))"))
        self.assertEqual('nil', self.all_in_one("(block nil (+ 1 2) (- 1 2) (return) (* 1 2))"))
        self.assertEqual('returned!', self.all_in_one("(block nil (+ 1 2) (- 1 2) (return 'returned!) (* 1 2))"))
        # loop
        self.assertEqual('6', self.all_in_one("(let ((counter 0)) (loop (setq counter (1+ counter)) (if (> counter 5) (return counter))))"))
        # dotimes, return
        self.assertEqual('120', self.all_in_one('(let ((x 1)) (dotimes (i 5) (setq x (* x (1+ i)))) x)'))
        self.assertEqual('24', self.all_in_one('(let ((x 1)) (dotimes (i 5) (setq x (* x (1+ i))) (if (= i 3) (return))) x)'))
        # dolist, return
        self.assertEqual('120', self.all_in_one("(let ((x 1)) (dolist (elm '(1 2 3 4 5)) (setq x (* x elm))) x)"))
        self.assertEqual('24', self.all_in_one("(let ((x 1)) (dolist (elm '(1 2 3 4 5)) (setq x (* x elm)) (if (= elm 4) (return))) x)"))
        # lambda
        self.assertEqual('35', self.all_in_one("(apply #'(lambda (x y z) (* (+ x y) z)) '(3 4 5))"))
        # defun, return-from
        self.assertEqual('f-1', self.all_in_one('(defun f-1 (x) (1+ x))'))
        self.assertEqual('3', self.all_in_one('(f-1 2)'))
        self.assertEqual('20', self.all_in_one('(let ((f-2 20)) (defun f-2 (x) (+ x 2)) f-2)'))    # LISP-2
        self.assertEqual('6', self.all_in_one('(let ((f-3 20)) (defun f-3 (x) (+ x 3)) (f-3 3))'))    # LISP-2
        self.assertEqual('6', self.all_in_one('(f-3 3)'))    # registered with global table
        self.assertEqual('reached-10', self.all_in_one("(progn (defun foo (num) (dotimes (i num) (if (= i 10) (return-from foo 'reached-10)))) (foo 20))"))
        # setq
        self.assertEqual('3', self.all_in_one('(let ((x 2)) (let ((y 3)) (setq x 3)) x)'))    # 一番目のローカルスコープの x が書き換わる
        self.assertEqual('100', self.all_in_one('(let ((x 100)) (let ((x 2)) (let ((y 3)) (setq x 3))) x)'))    # 二番目のローカルスコープの x が書き換わる
        # defvar
        self.assertEqual('not-useful', self.all_in_one('(defvar not-useful)'))
        self.assertEqual('*x*', self.all_in_one('(defvar *x* 10)'))
        self.assertEqual('10', self.all_in_one('*x*'))
        self.assertEqual('*x*', self.all_in_one('(defvar *x* 20)'))
        self.assertEqual('10', self.all_in_one('*x*'))    # cannot overwrite!
        # defparameter
        self.assertEqual('*y*', self.all_in_one('(defparameter *y* 10)'))
        self.assertEqual('10', self.all_in_one('*y*'))
        self.assertEqual('*y*', self.all_in_one('(defparameter *y* 20)'))
        self.assertEqual('20', self.all_in_one('*y*'))    # easily overwitten!
        # let
        self.assertEqual('6', self.all_in_one('(let ((x 3)) (let ((x 1) (y (+ x 2))) (+ x y)))'))
        # let*
        self.assertEqual('4', self.all_in_one('(let ((x 3)) (let* ((x 1) (y (+ x 2))) (+ x y)))'))
        # flet
        self.assertEqual('10', self.all_in_one('(flet ((foo (x) (+ x 2)) (bar (x) (+ x 5))) (let ((foo 100)) (bar (foo 3))))'))
        # labels
        self.assertEqual('7', self.all_in_one('(labels ((foo (x) (+ x 2)) (bar (x) (+ (foo x) 2))) (bar 3))'))


    def test_dynamic_lexical(self):
        print("test >>> dynamic scope and lexical scope")
        # dynamic scope
        self.assertEqual('100', self.all_in_one('(progn (defparameter *global-var* 1) (defun access-dynamic () *global-var*) (let ((*global-var* 100)) (access-dynamic)))'))
        # lexical scope
        self.assertEqual('1', self.all_in_one('(progn (setq lexical-var 1) (defun access-lexical () lexical-var) (let ((lexical-var 100)) (access-lexical)))'))


    def test_recursive(self):
        print("test >>> recursive function")
        self.assertEqual('55', self.all_in_one('(progn (defun fibo (n) (if (<= n 2) 1 (+ (fibo (- n 1)) (fibo (- n 2))))) (fibo 10))'))
    
        
    def test_mutual_recursive(self):
        self.assertEqual('mutual-1', self.all_in_one("(defun mutual-1 (num) (if (>= 0 num) (cons num '(in-mutual-1)) (mutual-2 (- num 1))))"))
        self.assertEqual('mutual-2', self.all_in_one("(defun mutual-2 (num) (if (>= 0 num) (cons num '(in-mutual-2)) (mutual-1 (- num 2))))"))
        self.assertEqual('(0 in-mutual-2)', self.all_in_one('(mutual-1 10)'))
        self.assertEqual('(-1 in-mutual-1)', self.all_in_one('(mutual-1 5)'))


if __name__ == "__main__":
    unittest.main()
# coding: UTF-8
"""
1. Common Lisp 組み込みシンボルたち (の一部) を登録したシンボルテーブル

data -- list
     |- atom -- symbol
             |- string
             |- number -- integer
                       |- float
                       |- ratio
                       |- complex
上記 atom のうち、symbol は LispSymbol に、string は LispStr に、 integer は LispInt に、float は LispFloat に、ratio は LispRatio に、complex は LispComplex にそれぞれ変換される
なお、complex リテラルはサポートされていない。(#c(1, 2) など)

2. 組み込みの変数環境、関数環境 (抜粋)

サポートされる組み込み変数:
    pi
サポートされる組み込み特殊形式、シンボル:
    quote('), quasiquote(`), unquote(,), unquote-splicing(,@), funcquote(#'),
    t, nil, otherwise, return, return-from,
    if, progn, prog1, when, unless, cond, case, block, loop, dotimes, dolist,
    lambda, defun, setq, defvar, defparameter, let, let*, flet, labels
サポートされる組み込み関数:
    acos, acosh, asin, asinh, atan, atanh, cos, cosh, sin, sinh, tan, tanh, exp, expt, log, gcd, mod, rem, imagpart, realpart, isqrt, sqrt, 1+, 1-,
    +, -, *, /, >, <, >=, <=, =, eq, equal, not,
    length, cons, append, list, car, first, cdr, rest, last, caar, cadr, cdar, cddr,
    consp, listp, null, symbolp, numberp, oddp, evenp, float, complex, print, apply, funcall
"""

import math
import operator as op
from fractions import Fraction
from functools import reduce


# ===========
# トークン用
# ===========

class LispStr(str):
    """string literal"""
    pass


class LispNumber:
    """abstract class for number"""
    pass


class LispInt(int, LispNumber):
    """integer literal"""
    pass


class LispFloat(float, LispNumber):
    """float literal"""
    pass


class LispRatio(Fraction, LispNumber):
    """rational literal"""
    pass

class LispComplex(complex, LispNumber):
    """complex literal"""
    pass


class LispSymbol(str):
    """any other token"""
    pass


class LispSpecial:
    """special class for special variable"""
    def __init__(self, val):
        self.special_value = val
    def __str__(self):
        return str(self.special_value)


SYMBOL_TABLE = {}

def symbolize(s):
    """
    LispSymbol のインスタンスを作成し、SYMBOL_TABLE に登録する。
    (シンボルはリテラルと異なり固有であるため、 LispSymbol のコンストラクタをいちいち呼ぶのではなく登録したものを再利用するようにしている)
    (これによりシンボルの同一性チェック eq が可能になる)
    Args:
        s (str)
    Returns:
        LispSymbol object
    """
    if s not in SYMBOL_TABLE:
        SYMBOL_TABLE[s] = LispSymbol(s)
    return SYMBOL_TABLE[s]




# ---- 気が済むまで以下に追加 ----
TOK_QUOTE = symbolize("quote")
TOK_QUASIQUOTE = symbolize("quasiquote")
TOK_UNQUOTE = symbolize("unquote")
TOK_UNQUOTE_SPLICING = symbolize("unquote-splicing")
TOK_FUNCQUOTE = symbolize("funcquote")

TOK_T = symbolize("t")
TOK_NIL = symbolize("nil")

TOK_OTHERWISE = symbolize("otherwise")
TOK_RETURN = symbolize("return")
TOK_RETURN_FROM = symbolize("return-from")

TOK_IF = symbolize("if")
TOK_PROGN = symbolize("progn")
TOK_PROG1 = symbolize("prog1")
TOK_WHEN = symbolize("when")
TOK_UNLESS = symbolize("unless")
TOK_COND = symbolize("cond")
TOK_CASE = symbolize("case")
TOK_BLOCK = symbolize("block")
TOK_LOOP = symbolize("loop")
TOK_DOTIMES = symbolize("dotimes")
TOK_DOLIST = symbolize("dolist")

TOK_LAMBDA = symbolize("lambda")
TOK_DEFUN = symbolize("defun")
TOK_SETQ = symbolize("setq")
TOK_DEFVAR = symbolize("defvar")
TOK_DEFPARAMETER = symbolize("defparameter")
TOK_LET = symbolize("let")
TOK_LETSTAR = symbolize("let*")
TOK_FLET = symbolize("flet")
TOK_LABELS = symbolize("labels")




# ====================
# 変数環境、関数環境用
# ====================

# ---- 気が済むまで以下に追加 ----
GLOBAL_VAR_DICT = {"pi":math.pi}

# defun された関数や無名関数に関して。
# 第一引数として実行時のスコープを渡すことでその環境をもとにclosure内部でスペシャル変数の実行時評価をできるようにしている。
# 関数適用の際 defun された関数なのかビルトインなのかの判定をするのが面倒なので、一律に第一引数として実行時スコープを渡しても動作するようにしておく。
GLOBAL_FUNC_DICT ={}


GLOBAL_FUNC_DICT.update({
    # math のそれと同名の数学関数
    'acos': lambda dyna, *args: math.acos(*args),
    'acosh': lambda dyna, *args: math.acosh(*args),
    'asin': lambda dyna, *args: math.asin(*args),
    'asinh': lambda dyna, *args: math.asinh(*args),
    'atan': lambda dyna, *args: math.atan(*args),
    'atanh': lambda dyna, *args: math.atanh(*args),
    'cos': lambda dyna, *args: math.cos(*args),
    'cosh': lambda dyna, *args: math.cosh(*args),
    'exp': lambda dyna, *args: math.exp(*args),
    'gcd': lambda dyna, *args: math.gcd(*args),
    'log': lambda dyna, *args: math.log(*args),
    'sin': lambda dyna, *args: math.sin(*args),
    'sinh': lambda dyna, *args: math.sinh(*args),
    'sqrt': lambda dyna, *args: math.sqrt(*args),
    'tan': lambda dyna, *args: math.tan(*args),
    'tanh': lambda dyna, *args: math.tanh(*args),
    # その他数学関数
    '1+': lambda dyna, x: x + 1,
    '1-': lambda dyna, x: x - 1,
    'expt': lambda dyna, *args: math.pow(*args),
    'isqrt': lambda dyna, *args: math.floor(math.sqrt(*args)),
    'realpart':lambda dyna, x: x.real,
    'imagpart': lambda dyna, x: x.imag,
    'mod': lambda dyna, x, y: x % y,    # 最小非負剰余
    'rem': lambda dyna, x, y: x % y if x*y >=0 else (x % y) - y,
    # operator 系
    'not':lambda dyna, *args: op.not_(*args),
    '+':lambda dyna, *args: reduce(op.add, args),
    '-':lambda dyna, *args: op.sub(*args),
    '*':lambda dyna, *args: reduce(op.mul, args),
    '/':lambda dyna, *args: op.truediv(*args),
    '>':lambda dyna, *args: op.gt(*args),
    '<':lambda dyna, *args: op.lt(*args),
    '>=':lambda dyna, *args: op.ge(*args),
    '<=':lambda dyna, *args: op.le(*args),
    '=':lambda dyna, *args: op.eq(*args),
    'eq':lambda dyna, x, y: x is y,
    'equal':lambda dyna, *args: op.eq(*args),
    # リスト用
    'length':lambda dyna, x: len(x),
    'cons':lambda dyna, x, y: lisp_cons(x, y),    # dot cons is not supported
    'append': lambda dyna, x, y: lisp_append(x, y),    # dot cons is not supported
    'list':lambda dyna, *x: list(x),
    'car':lambda dyna, x: x[0],
    'first': lambda dyna, x: x[0],
    'cdr': lambda dyna, x: x[1:],
    'rest': lambda dyna, x: x[1:],
    'last': lambda dyna, x: x[-1:],
    'caar': lambda dyna, x: x[0][0],
    'cadr': lambda dyna, x: x[1:][0],
    'cdar': lambda dyna, x: x[0][1:],
    'cddr': lambda dyna, x: x[1:][1:],
    # 述語
    'consp': lambda dyna, x: isinstance(x, list) and x != [],
    'listp': lambda dyna, x: isinstance(x, list) or not x,
    'null': lambda dyna, x: x == [] or not x,
    'symbolp': lambda dyna, x: isinstance(x, LispSymbol) or x == [] or not x,
    'numberp': lambda dyna, x: isinstance(x, LispNumber),
    'oddp': lambda dyna, x: x % 2 == 1,
    'evenp': lambda dyna, x: x % 2 == 0,
    # その他
    'float': lambda dyna, x: LispFloat(x),
    'complex': lambda dyna, x, y: LispComplex(x, y),
    'print': lambda dyna, x: lisp_print(x),
    'apply': lambda dyna, procedure, lst: procedure(dyna, *lst),    # dyna を忘れずに
    'funcall': lambda dyna, procedure, *arg: procedure(dyna, *arg)    # dyna を忘れずに
})


def lisp_cons(x, y):
    if not (isinstance(y, list) or not y):
        raise TypeError("lisp_cons(): got {}".format(x, y))
    else:
        if not y:    # nil が False へと変換されてしまっている
            y = []
        return [x] + y


def lisp_append(x, y):
    if isinstance(x, list) and (isinstance(y, list) or not y):
        if not y:    # nil が False へと変換されてしまっている
            y = []
        return op.add(x, y)
    else:
        raise TypeError("lisp_append(): got {}".format(x, y))


def lisp_print(x):
    print(x)
    return x
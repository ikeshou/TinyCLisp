# coding: UTF-8
"""
Evaluator for S-expressions
その名の通り評価を行う。ほったらかしにしていた構文エラーもここでしっかり ParseError として検出する。
#TODO: macro の処理

>>> t = LispTokenizer()
>>> p = LispParser()
>>> e = LispEvaluator()
>>> all_in_one = lambda s: e.evaluate(p.parse(t.tokenize(s)))
>>> all_in_one('"This is an atom"')
'"This is an atom"'
>>> all_in_one('100')
100
>>> all_in_one('(print "\\'(\\' is a parenthesis.")')
"'(' is a parenthesis."
>>> all_in_one('(+ 1 (* 2 3))')
7
>>> all_in_one("(length '(1 2 3 4 5))")
5
>>> all_in_one("(defun f (x) (* x x))")
'f'
>>> all_in_one("(flet ((f (x) (* x x))) (f 5))")
25
"""

from lisp_built_in import *
from lisp_tokenizer import LispTokenizer
from lisp_parser import LispParser, ParseError


def shallow_for_atom_copy(s_exp):
    """
    eval_1 メソッド中の TOK_LOOP, TOK_DOTIMES, TOK_DOLIST について。同一のS式を複数回評価に回す際 pop の副作用が残っていないようにしたい。
    リスト構造を deep copy していくが、s_exp と同じ構造のオブジェクトを作り上げる。
    atom 段階になったら shallow copy を行うことで TOK... の同一参照を壊さないようにする。
    """
    if not isinstance(s_exp, list):
        return s_exp
    else:
        return [shallow_for_atom_copy(elm) for elm in s_exp]


def make_local_func_body(evaluator, func_body, variables, env, func_env):
    """
    eval_1 メソッド中の TOK_FLET, TOK_LABELS について。各ローカル関数に注目するためのループでクロージャーを作るとループ内の一時変数が上書きされる影響でクロージャーが壊れる。
    この外部関数を用いて適切に関数を作成する。
    """
    def local_func(dyna, *args):
        return evaluator.eval_1(shallow_for_atom_copy(func_body), Env(parameters=variables, args=args, outer=env), func_env, dyna)
    return local_func


class LoopInterrupted(Exception):
    """
    ループ中での return を補足する専用のエラー
    err.args[0]: block name (LispSymbol)
    err.args[1]: return value (Any)
    """
    pass


class Env(dict):
    """
    環境 (スコープチェーン): {'var':val} の dict で、自身は親環境 (outer) を持つ。
    シンボル用と関数用で別々のインスタンスを作成する必要がある。(LISP-2)
    """
    def __init__(self, parameters=(), args=(), outer=None):
        super(Env, self).__init__(parameters=(), args=(), outer=None)
        self.outer = outer
        # １つのバインド
        if isinstance(parameters, LispSymbol):
            try:
                self.update({parameters: list(args)})
            except TypeError:
                self.update({parameters: args})    # dotimesとかでsymbolのiを仮のカウンターの数にバインドしたい。iterableでない。
        # 複数のバインド
        else:
            if len(parameters) != len(args):
                raise TypeError("the length of parameters and the one of args are different. parameters:{} args:{}".format(parameters, args))
            self.update(zip(parameters, args))

    def find(self, var):
        """var が現れる一番内側のEnvを見つける。"""
        if var in self:
            return self
        elif self.outer is None:
            raise LookupError("could not find {}".format(var))
        else:
            return self.outer.find(var)



class LispEvaluator:
    def __init__(self):
        self.global_env = Env()
        self.global_env.update(GLOBAL_VAR_DICT)
        self.global_func_env = Env()
        self.global_func_env.update(GLOBAL_FUNC_DICT)


    def preprocess(self, ast):
        """
        後のevalで処理しやすいよう以下の前処理をする。
        - 複数の実行式をprognでまとめてしまう。[ (lambda (x y ..) (exp1) (exp2)...))とか ]
        - quasiquoteが出てきたら中身をextend()で中身をバラす。後のフェーズではquotesのうち、quoteのみを処理すれば良いようにする。
        Args:
            ast (Union(list, LispSymbol, LispStr, LispInt, LispFloat))
        Returns:
            Union(list, LispSymbol, LispStr, LispInt, LispFloat)
        """
        if not (isinstance(ast, list) and ast != []):    # nil の処理もここで行わないと次のx[0]参照でバグる
            return ast
        elif ast[0] == TOK_LAMBDA:
            return self._lambda_wrap(ast)
        elif ast[0] == TOK_DEFUN:
            return self._defun_wrap(ast)
        elif ast[0] == TOK_FLET or ast[0] == TOK_LABELS:
            return self._local_func_wrap(ast)
        elif ast[0] == TOK_QUASIQUOTE:
            res = ast[1]
            return self._quasiquote_extend(res)    # [TOK_QUASIQUOTE, [...]] の [...] に対して処理を行う
        else:
            return list(map(self.preprocess, ast))
    

    def _lambda_wrap(self, ast):
        """
        preprocess() で呼ばれる内部関数
        >>> e = LispEvaluator()
        >>> e._lambda_wrap((['lambda', ['x'], ['+', 'x', 'y'], ['-', 'x', 'y']]))
        ['lambda', ['x'], ['progn', ['+', 'x', 'y'], ['-', 'x', 'y']]]
        """
        token, variables = ast[:2]
        body = [TOK_NIL] if len(ast) == 2 else ast[2:]
        exp = body[0] if len(body) == 1 else [TOK_PROGN] + body
        return [token, variables, self.preprocess(exp)]
    
    def _defun_wrap(self, ast):
        """
        preprocess() で呼ばれる内部関数
        >>> e = LispEvaluator()
        >>> e._defun_wrap((['defun', 'f', ['x'], ['+', 'x', 'y'], ['-', 'x', 'y']]))
        ['defun', 'f', ['x'], ['progn', ['+', 'x', 'y'], ['-', 'x', 'y']]]
        """
        token, func_name, variables = ast[:3]
        body = [TOK_NIL] if len(ast) == 3 else ast[3:]
        exp = body[0] if len(body) == 1 else [TOK_PROGN] + body
        return [token, func_name, variables, self.preprocess(exp)]
    
    def _local_func_wrap(self, ast):
        """
        preprocess() で呼ばれる内部関数
        >>> e = LispEvaluator()
        >>> e._local_func_wrap(['flet', [['f', ['x'], ['+', 'x', 'y'], ['-', 'x', 'y']]], ['f', '2', '3']])
        ['flet', [['f', ['x'], ['progn', ['+', 'x', 'y'], ['-', 'x', 'y']]]], ['f', '2', '3']]
        >>> e._local_func_wrap(['flet', [['f', ['x'], ['+', 'x', 'y']]], ['f', '2', '3']])
        ['flet', [['f', ['x'], ['+', 'x', 'y']]], ['f', '2', '3']]
        """
        token, func_list = ast[:2]
        res = [TOK_NIL] if len(ast) == 2 else ast[2]
        new_func_list = []
        for func in func_list:
            f_name, variables = func[:2]
            body = [TOK_NIL] if len(func) == 2 else func[2:]
            exp = body[0] if len(body) == 1 else [TOK_PROGN] + body
            new_func_list.append([f_name, variables, self.preprocess(exp)])
        return [token, new_func_list, self.preprocess(res)]


    def _quasiquote_extend(self, res):
        """
        preprocess() で呼ばれる内部関数
        >>> e = LispEvaluator()
        >>> e._quasiquote_extend('x')    # `x
        ['quote', 'x']
        >>> e._quasiquote_extend([TOK_UNQUOTE, 'x'])    # `,x
        'x'
        >>> e._quasiquote_extend([[TOK_UNQUOTE_SPLICING, ['list', 'x', 'y']], 'z'])    # `(,@(list x y) z)
        ['append', ['list', 'x', 'y'], ['cons', ['quote', 'z'], ['quote', []]]]
        >>> e._quasiquote_extend(['moo', [TOK_UNQUOTE_SPLICING, ['list', 'x', 'y']], [TOK_UNQUOTE, 'z']])    # `(moo ,@(list x y) ,z)
        ['cons', ['quote', 'moo'], ['append', ['list', 'x', 'y'], ['cons', 'z', ['quote', []]]]]
        """
        # atom もしくは nil の時  `x -> 'x
        if not (isinstance(res, list) and len(res) != 0):
            return [TOK_QUOTE, res]
        # listの時
        else:
            # `[TOK_UNQUOTE, [args]] -> [args]
            if res[0] is TOK_UNQUOTE:
                return res[1]
            #  冒頭に ,@ が来る時
            elif isinstance(res[0], list) and res[0] != [] and res[0][0] is TOK_UNQUOTE_SPLICING:
                return [symbolize("append"), res[0][1], self._quasiquote_extend(res[1:])]
            # 冒頭には,@ が来ていない時
            else:
                return [symbolize("cons"), self._quasiquote_extend(res[0]), self._quasiquote_extend(res[1:])]


    def eval_1(self, ast, env, func_env, dynamic_env,func_flag=False):
        """
        与えられた環境の中でASTを評価していく
        Args:
            ast (Union(list, LispSymbol, LispStr, LispInt, LispFloat)
            env (Env)
            func_env (Env)
            dynamic_env (Env):
                special 変数の場合は lexical scope ではなくこの dynamic scope で評価を行う必要がある。
            func_flag (bool):
                その atom が関数であるか否かを示す。
                関数適用のためにS式の先頭の関数オブジェクトを取得する段階 (末尾の else 節) で、このフラグが True になって呼ばれてくる。それ以外は常に False。

        Returns:
            Any
        """
        # print(ast)    # for debug
        # 真偽値
        if ast is TOK_T:
            return True
        elif ast is TOK_NIL:
            return False
        # 空のリストはLispではnil扱いだがconsとかで出てきたときにFalseに変換されてると困るのでとりあえず[]と評価しておく（暫定）
        elif ast == []:    # isはidを比較して同一オブジェクトか判定するので == にせんと
            return ast
        # 変数 (関数を示すシンボル) 参照
        elif isinstance(ast, LispSymbol) and func_flag:
            return func_env.find(ast)[ast]
        # 変数 (値を示すシンボル) 参照
        # 参照しているものがスペシャルシンボルかどうかを判定する必要がある
        elif isinstance(ast, LispSymbol):
            # まずは普通のスコープチェーンで検索。これで見つかった値がスペシャル変数のそれの場合、ダイナミックスコープで引き直す。
            # lambda や defun 内部でなければ env と dynamic_env は同一なのでもう一度同じやつが見つかる。special_value をアトリビュートアクセスすりゃOK
            # lambda や defun 内部の場合 let などで上書きされた値が見つかるかもしれない。もう一度 LispSpecial インスタンスチェックが必要。
            val = env.find(ast)[ast]
            if not isinstance(val, LispSpecial):
                return val
            else:
                dynamic_val = dynamic_env.find(ast)[ast]
                return dynamic_val.special_value if isinstance(dynamic_val, LispSpecial) else dynamic_val
        # LispStr or LispInt or LispFloat
        elif not isinstance(ast, list):
            return ast
        # list
        else:
            # return  (return) or (return val)
            # LoopInterrupted エラーの args の第一引数に抜け出す block 名、(あるなら) 第二引数に戻り値を携える
            if isinstance(ast, list) and ast[0] == TOK_RETURN:
                if len(ast) != 1 and len(ast) != 2:
                    raise ParseError("invalid AST around return. AST:{}".format(ast))
                args = (TOK_NIL, TOK_NIL) if len(ast) == 1 else (TOK_NIL, self.eval_1(ast[1], env, func_env, env))    # val を評価
                raise LoopInterrupted(*args)
            # return-from  (return-from label) or (return-from label val)
            if isinstance(ast, list) and ast[0] == TOK_RETURN_FROM:
                if len(ast) != 2 and len(ast) != 3:
                    raise ParseError("invalid AST around return. AST:{}".format(ast))
                args = (ast[1], TOK_NIL) if len(ast) == 2 else (ast[1], self.eval_1(ast[2], env, func_env, env))
                raise LoopInterrupted(*args)
            # quote  (quote LispStr)
            if ast[0] is TOK_QUOTE:
                if len(ast) != 2:
                    raise ParseError("invaid AST around quote. AST:{}".format(ast))
                _, exp = ast
                return exp
            # funcquote  (funcquote LispStr)
            elif ast[0] is TOK_FUNCQUOTE:    # シンボルではなく lambda closure (function object) を返すと考える (Python にはシンボルの考え方がないのでこうしないと高階関数を適用できん)
                if len(ast) != 2:
                    raise ParseError("invalid AST around #'. AST:{}".format(ast))
                _, exp = ast
                if isinstance(exp, list):
                    return self.eval_1(exp, env, func_env, env)    # #'(lambda ...)
                else:
                    return func_env.find(exp)[exp]    # #'f
            # if  (if (test) (then) (else))
            elif ast[0] is TOK_IF:
                if len(ast) != 3 and len(ast) != 4:
                    raise ParseError("invalid AST around if. AST:{}".format(ast))
                if len(ast) == 3:
                    _, test, then_clause = ast
                    else_clause = TOK_NIL
                else:
                    _, test, then_clause, else_clause = ast
                return self.eval_1(then_clause if self.eval_1(test, env, func_env, env) else else_clause, env, func_env, env)
            # progn  (progn (exp1) (exp2) ...)
            elif ast[0] is TOK_PROGN:
                for exp in ast[1:]:
                    val = self.eval_1(exp, env, func_env, env)
                return val
            # prog1  (prog1 (exp1) (exp2) ...)
            elif ast[0] is TOK_PROG1:
                result = None
                for ind, exp in enumerate(ast[1:]):
                    val = self.eval_1(exp, env, func_env, env)
                    if ind == 0:
                         result = val
                return result
            # when  (when (test) (exp1) (exp2) ...)
            elif ast[0] is TOK_WHEN:
                if len(ast) < 2:
                    raise ParseError("invalid AST around when. AST:{}".format(ast))
                test = ast[1]
                if self.eval_1(test, env, func_env, env):
                    for exp in ast[2:]:
                        val = self.eval_1(exp, env, func_env, env)
                else:
                    val = TOK_NIL
                return val
            # unless  (unless (test) (exp1) (exp2) ...)
            elif ast[0] is TOK_UNLESS:
                if len(ast) < 2:
                    raise ParseError("invalid AST around unless. AST:{}".format(ast))
                test = ast[1]
                if not self.eval_1(test, env, func_env, env):
                    for exp in ast[2:]:
                        val = self.eval_1(exp, env, func_env, env)
                else:
                    val = TOK_NIL
                return val
            # cond  (cond ((test1) (exp1)) ((test2) (exp2)) ... (t (expn)))
            elif ast[0] is TOK_COND:
                if len(ast) < 2:
                    raise ParseError("invalid AST around cond. AST:{}".format(ast))
                for cluase in ast[1:]:
                    if self.eval_1(cluase[0], env, func_env, env):
                        return self.eval_1(cluase[1], env, func_env, env)
            # case  (case value ((cond1) (exp1) (exp2)) (((cond2) (cond3)) (exp3) (exp4)) ...)
            elif ast[0] is TOK_CASE:
                if len(ast) <2 or not isinstance(ast[2], list):
                    raise ParseError("invalid AST around case. AST:{}".format(ast))
                value = self.eval_1(ast[1], env, func_env, env)
                for clause in ast[2:]:
                    value_matched = False
                    if clause[0] is TOK_OTHERWISE:
                        value_matched = True
                    else:
                        if isinstance(clause[0], list):
                            for cond in clause[0]:
                                if value == cond:
                                    value_matched = True
                                    break
                        else:
                            value_matched = (value == clause[0])
                    if value_matched:
                        # マッチしてたらその節のS式を順に実行して最後の結果を返しておしまい
                        for exp in clause[1:]:
                            val = self.eval_1(exp, env, func_env, env)
                        return val
            # block  (block name (exp1) (exp2) ...)
            elif ast[0] is TOK_BLOCK:
                if len(ast) < 2:
                    raise ParseError("invalid AST around block. AST:{}".format(ast))
                _, block_name, *exps = ast
                try:
                    for exp in exps:
                        val = self.eval_1(exp, env, func_env, env)
                    return val
                except LoopInterrupted as err:
                    if err.args[0] == block_name:
                        return err.args[1] if len(err.args) == 2 else TOK_NIL
                    else:
                        raise err
            # loop  (loop (exp1) (exp2) ...)
            elif ast[0] is TOK_LOOP:
                if len(ast) < 2:
                    raise ParseError("invalid AST around loop. AST:{}".format(ast))
                _, *exps = ast
                while True:
                    for exp in exps:
                        try:
                            # loop の場合は S式を使いまわすため pop されていってしまうと困る。deepcopy の tmp に犠牲になってもらう。(ASTは多次元リストなので deepcopy で)                            
                            # そうするともはや TOK_... とは別物になってしまい is で判定できなくなる。== による比較の方針にすると同一シンボルの限見るな比較が行えなくなってしまう。
                            # というわけで自作の shallow_for_atom_copy 関数を使用する。
                            tmp = shallow_for_atom_copy(exp)
                            self.eval_1(tmp, env, func_env, env)
                        except LoopInterrupted as err:
                            if err.args[0] == TOK_NIL:
                                return err.args[1] if len(err.args) == 2 else TOK_NIL
                            else:
                                raise err
            # dotimes  (dotimes (counter num) (exp1) (exp2) ...)
            elif ast[0] is TOK_DOTIMES:
                if len(ast) < 2:
                    raise ParseError("invalid AST around dotimes. AST:{}".format(ast))
                counter, exp = ast[1]    # num のところはS式が許されている
                num = self.eval_1(exp, env, func_env, env)
                for i in range(num):
                    for exp in ast[2:]:
                        try:
                            new_env = Env(parameters=counter, args=i, outer=env)
                            tmp = shallow_for_atom_copy(exp)
                            val = self.eval_1(tmp, new_env, func_env, new_env)
                        except LoopInterrupted as err:
                            if err.args[0] == TOK_NIL:
                                return err.args[1] if len(err.args) == 2 else TOK_NIL
                            else:
                                raise err
                return val
            # dolist  (dolist (elm lst) (exp1) (exp2) ...)
            elif ast[0] is TOK_DOLIST:
                if len(ast) < 2:
                    raise ParseError("invalid AST around dolist. AST:{}".format(ast))
                temporary_variable, exp = ast[1]
                lst = self.eval_1(exp, env, func_env, env)    # lst のところはS式が許されている
                for elm in lst:
                    for exp in ast[2:]:
                        try:
                            new_env = Env(parameters=temporary_variable, args=elm, outer=env)
                            tmp = shallow_for_atom_copy(exp)
                            val = self.eval_1(tmp, new_env, func_env, new_env)
                        except LoopInterrupted as err:
                            if err.args[0] == TOK_NIL:
                                return err.args[1] if len(err.args) == 2 else TOK_NIL
                            else:
                                raise err
                return val
            # lambda  (lambda (arg1 arg2 ...) (exp))  (progn でまとめられている)
            elif ast[0] is TOK_LAMBDA:
                if len(ast) != 3:
                    raise ParseError("invalid AST around lambda. AST:{}".format(ast))
                _, variables, exp = ast
                # 関数ボディ exp が pop されてしまいその list への参照をクロージャーが持っているため関数が壊れてしまう。shallow_for_atom_copyを使う。
                # 関数コール時のスコープを受け取れるようにする
                return lambda dyna, *args: self.eval_1(shallow_for_atom_copy(exp), Env(parameters=variables, args=args, outer=env), func_env, dyna)
            # defun  (defun f-name (arg1 arg2 ...) (exp))  (progn でまとめられている)
            # どこで defun されようと global に登録される
            elif ast[0] is TOK_DEFUN:
                if len(ast) != 4:
                    raise ParseError("invalid AST around defun. AST:{}".format(ast))
                _, func_name, variables, exp = ast
                # defun はその関数名を block name にしてブロックを形成する。(return-from func_name val) をキャッチする必要がある。
                # 関数ボディ exp が pop されてしまいその list への参照をクロージャーが持っているため関数が壊れてしまう。shallow_for_atom_copyを使う。
                # 関数コール時のスコープを受け取れるようにする
                def defun_body(dyna, *args):
                    tmp = shallow_for_atom_copy(exp)    
                    try:
                        return self.eval_1(tmp, Env(parameters=variables, args=args, outer=env), func_env, dyna)    
                    except LoopInterrupted as err:
                        if err.args[0] == func_name:
                            return err.args[1] if len(err.args) == 2 else TOK_NIL
                        else:
                            raise err
                self.global_func_env[func_name] = defun_body
                return func_name
            # setq  (setq var1 (exp1) var2 (exp2) ...)
            # sometimes global variable (if the variable has not yet been defined), lexical scope
            elif ast[0] is TOK_SETQ:
                if len(ast) < 3:
                    raise ParseError("invalid AST around setq. AST:{}".format(ast))
                _, *pairs = ast
                if len(pairs) % 2 != 0:
                    raise ParseError("invalid AST around setq (the number of variables and values must be the same). AST:{}".format(ast))
                for i in range(len(pairs)//2):
                    variable = pairs[i]
                    value = self.eval_1(pairs[i+1], env, func_env, env)
                    try:
                        env.find(variable)[variable] = value
                    except LookupError:
                        self.global_env[variable] = value
                return value
            # defvar  (defvar var) or (defvar var (exp))
            # 前者の場合はスルーすることにする。
            # var が定義済みの場合は上書きしない。global variable, dynamic scope
            elif ast[0] is TOK_DEFVAR:
                if len(ast) < 2:
                    raise ParseError("invalid AST around defvar. AST:{}".format(ast))
                variable = ast[1]
                if len(ast) == 3 and variable not in self.global_env:
                    value = self.eval_1(ast[2], env, func_env, env)
                    self.global_env[variable] = LispSpecial(value)
                return variable
            # defparameter  (defparameter var (exp))
            # var が定義済みでも上書きする。global variable, dynamic scope
            elif ast[0] is TOK_DEFPARAMETER:
                if len(ast) != 3:
                    raise ParseError("invalid AST around defparameter. AST:{}".format(ast))
                _, variable, exp = ast
                value = self.eval_1(exp, env, func_env, env)
                self.global_env[variable] = LispSpecial(value)
                return variable
            # let  (let ((var1 (exp1)) (var2 (exp2)) ...) (exp-a) (exp-b) ...)
            elif ast[0] is TOK_LET:
                if len(ast) < 2:
                    raise ParseError("invalid AST around let. AST:{}".format(ast))
                _, pair_list, *exps = ast
                new_env = Env(outer=env)
                for pair in pair_list:
                    new_env[pair[0]] = self.eval_1(pair[1], env, func_env, env)
                for exp in exps:
                    val = self.eval_1(exp, new_env, func_env, new_env)
                return val
            # let*  (let* ((var1 (exp1)) (var2 (exp2)) ...) (exp-a) (exp-b) ...)
            elif ast[0] is TOK_LETSTAR:
                if len(ast) < 2:
                    raise ParseError("invalid AST around let*. AST:{}".format(ast))
                _, pair_list, *exps = ast
                new_env = Env(outer=env)
                for pair in pair_list:
                    new_env[pair[0]] = self.eval_1(pair[1], new_env, func_env, new_env)
                for exp in exps:
                    val = self.eval_1(exp, new_env, func_env, new_env)
                return val
            # flet  (flet ((f1 (arg1 arg2...) (exp)) (f2 ...)) (exp-a) (exp-b) ...)  (progn でまとめられている)
            elif ast[0] is TOK_FLET:
                # import pdb; pdb.set_trace()
                if len(ast) < 2:
                    raise ParseError("invalid AST around flet. AST:{}".format(ast))
                _, func_list, *exps = ast
                new_func_env = Env(outer=func_env)
                for func in func_list:
                    func_name, variables, func_body = func
                    # 関数コール時のスコープを受け取れるようにする
                    # ここに lambda 式を直接書くと func_body がループにより上書きされてしまうためクロージャーが壊れる。外部関数を用いてバインドする。
                    new_func_env[func_name] = make_local_func_body(self, shallow_for_atom_copy(func_body), variables, env, func_env)
                for exp in exps:
                    val = self.eval_1(exp, env, new_func_env, env)
                return val
            # labels  (labels ((f1 (arg1 arg2...) (exp)) (f2 ...)) (exp-a) (exp-b) ...)  (progn でまとめられている)
            elif ast[0] is TOK_LABELS:
                if len(ast) < 2:
                    raise ParseError("invalid AST around labels. AST:{}".format(ast))
                _, func_list, *exps = ast
                new_func_env = Env(outer=func_env)
                for func in func_list:
                    func_name, variables, func_body = func
                    # 関数コール時のスコープを受け取れるようにする
                    # ここに lambda 式を直接書くと func_body がループにより上書きされてしまうためクロージャーが壊れる。外部関数を用いてバインドする。
                    new_func_env[func_name] = make_local_func_body(self, shallow_for_atom_copy(func_body), variables, env, new_func_env)
                for exp in exps:
                    val = self.eval_1(exp, env, new_func_env, env)
                return val
            # 再帰を回って最小単位のリストになったらそれに対して順に実行されていく。前置記法に従い冒頭のoperatorを残りに作用させる。
            # ここで関数エントリを引く時に func_flag を True にする
            # dynamic scope の処理のために呼び出し時の環境を渡す
            else:
                procedure = self.eval_1(ast.pop(0), env, func_env, env, func_flag=True)
                exps = [self.eval_1(exp, env, func_env, env) for exp in ast]
                return procedure(env, *exps)


    def evaluate(self, ast):
        """
        トップレベルでASTを読み込み評価していく
        Args:
            ast (Union(list, LispSymbol, LispStr, LispInt, LispFloat)
        Returns:
            evaluated (Any)
        """
        preprocessed = self.preprocess(ast)
        evaluated = self.eval_1(ast=preprocessed, env=self.global_env, func_env=self.global_func_env, dynamic_env=self.global_env, func_flag=False)
        return evaluated



#===================================================================
if __name__ == "__main__":
    import doctest
    fail_count, test_count = doctest.testmod()
    if fail_count == 0:
        print('tested {} cases: doctest passed'.format(test_count))

#===================================================================
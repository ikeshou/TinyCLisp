# coding: UTF-8
"""
Parser for S-expressions
トークンへの変更処理、再帰下降型の構文解析を行いネストリストを作る。
構文解析といいつつ()の対応以外の構文エラーは後の評価器側であげるようになっている。ここではASTを作れさえしたらその正当性 (文法に沿っているか) は判断しない。

>>> p = LispParser()
>>> p.parse(['"This is an atom"'])
'"This is an atom"'

>>> p.parse(['(', 'format', 't', '"Hello, world!"', ')'])
['format', 't', '"Hello, world!"']

>>> p.parse(['(', 'print', '"\\'(\\' is a parenthesis."', ')'])
['print', '"\\'(\\' is a parenthesis."']

>>> p.parse(["(", "+", "1", "(", "*", "2", "3", ")", ")"])
['+', 1, ['*', 2, 3]]

>>> p.parse(['(', 'length', "'", '(', '1', '2', '3', '4', '5', ')', ')'])
['length', ['quote', [1, 2, 3, 4, 5]]]

>>> p.parse(['(', 'defun', 'f', '(', 'x', ')', '(', '*', 'x', 'x', ')', ')'])
['defun', 'f', ['x'], ['*', 'x', 'x']]

>>> p.parse(['(', 'flet', '(', '(', 'f', '(', 'x', ')', '(', '*', 'x', 'x', ')', ')', ')', '(', 'mapcar', "#'", 'f', "'", '(', '1', '2', '3', ')', ')', ')'])
['flet', [['f', ['x'], ['*', 'x', 'x']]], ['mapcar', ['funcquote', 'f'], ['quote', [1, 2, 3]]]]
"""


from lisp_built_in import TOK_QUOTE, TOK_QUASIQUOTE, TOK_UNQUOTE, TOK_UNQUOTE_SPLICING, TOK_FUNCQUOTE
from lisp_built_in import LispStr, LispInt, LispFloat, LispRatio
from lisp_built_in import symbolize


class ParseError(RuntimeError):
    pass



class LispParser:
    def __init__(self):
        self.quote_family = {
            "quote": TOK_QUOTE,
            "'": TOK_QUOTE,
            "`": TOK_QUASIQUOTE, 
            ",": TOK_UNQUOTE, 
            ",@": TOK_UNQUOTE_SPLICING,
            "#'": TOK_FUNCQUOTE
        }    # これらは LispSymbol になっている

    
    def atom(self, single_sexp):
        """
        文字列はstrに、数はint, float, fractionにし、それ以外のトークンはシンボルにする。
        Args:
            single_sexp (str)
        Returns:
            Union(LispStr, LispInt, LispFloat, LispRational, LispSymbol)
        """
        if single_sexp.startswith('"'):
            return LispStr(single_sexp)
        else:
            try:
                return LispInt(single_sexp)
            except ValueError:
                try:
                    return LispFloat(single_sexp)
                except ValueError:
                    try:
                        return LispRatio(single_sexp)
                    except ValueError:
                        return symbolize(single_sexp)


    def parse(self, tokens):
        """
        トークンのリストを読み込み、各階層ごとに内部で[]にて区切られたリストを返す。
        Arg:
            tokens (list)
        Returns:
            ast (list)
        """
        if len(tokens) == 0:
            raise ParseError("LispParser.parse(): unexpected EOF while reading")
        token = tokens.pop(0)
        if '(' == token:
            ast = []
            while tokens[0] != ')':
                ast.append(self.parse(tokens))
            tokens.pop(0)    # pop off ')'
            return ast
        elif ')' == token:
            raise ParseError("LispParser.parse(): unexpected )")    # there is an excess )
        elif token in self.quote_family:
            return [self.quote_family[token], self.parse(tokens)]
        else:
            return self.atom(token)





#===================================================================
if __name__ == "__main__":
    import doctest
    fail_count, test_count = doctest.testmod()
    if fail_count == 0:
        print('tested {} cases: doctest passed'.format(test_count))
#===================================================================

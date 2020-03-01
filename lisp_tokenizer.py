# coding: UTF-8
"""
Tokenizer for S-expressions
字句解析といいつつトークンへの変更処理はパーサーの方で行う。ここではコメント除去を行い、さらに文字列を適切にバラしたリストにするのみ
ループ処理の共通化によりループ回数を削減することは可能ではあるが、流れ作業をわかりやすくするためにこのような作りになっている (add_proper_space と lex_string など)

>>> t = LispTokenizer()
>>> t.tokenize('"This is an atom"')
['"This is an atom"']

>>> t.tokenize('(format t "Hello, world!")')
['(', 'format', 't', '"Hello, world!"', ')']

>>> t.tokenize('(print "\\'(\\' is a parenthesis.")')
['(', 'print', '"\\'(\\' is a parenthesis."', ')']

>>> t.tokenize('(+ 1 (* 2 3))')
['(', '+', '1', '(', '*', '2', '3', ')', ')']

>>> t.tokenize("(length '(1 2 3 4 5))")
['(', 'length', "'", '(', '1', '2', '3', '4', '5', ')', ')']

>>> t.tokenize("(defun f (x) (* x x))")
['(', 'defun', 'f', '(', 'x', ')', '(', '*', 'x', 'x', ')', ')']

>>> t.tokenize("(flet ((f (x) (* x x))) (mapcar #'f '(1 2 3)))")
['(', 'flet', '(', '(', 'f', '(', 'x', ')', '(', '*', 'x', 'x', ')', ')', ')', '(', 'mapcar', "#'", 'f', "'", '(', '1', '2', '3', ')', ')', ')']
"""


class TokenizeError(RuntimeError):
    pass


class LispTokenizer:
    def __init__(self):
        self.add_space_table = str.maketrans({"(": "( ", ")": " )", "'": "' ", "`": "` ", ",": ", "})


    def strip_comments(self, source):
        """
        文字列中のコメント(; 及び #||#) の処理
        ; はその後改行に至るまでの文字列を、#||# はパイプの範囲内を削除する (ネスト可能)。#||# 内に ; が存在した場合 ; は無視される。
        
        >>> t = LispTokenizer()
        >>> t.strip_comments("one-line comment out; test")
        'one-line comment out'

        >>> t.strip_comments("hoge\\n; piyo\\n moo")
        'hoge\\n moo'

        >>> t.strip_comments("pipe comment out#|\\n test|#")
        'pipe comment out'

        >>> t.strip_comments("1 #| 2 #| 3 |# 4 |# 5")
        '1  5'

        >>> t.strip_comments("(+ 1 2 #| ; |#3)")
        '(+ 1 2 3)'

        Args:
            source (str)
        Returns:
            stripped (str)
        """
        size = len(source)
        stripped = ''
        pos = 0    # 現時点での走査インデックス
        previous_pos = 0    # stripped に追加していない最後の走査インデックスのメモ
        depth = 0    # 現時点での #||# のネストの深さ
       
        while 0 <= pos < size:
            if pos+2 <= size and source[pos:pos+2] == '#|':
                if depth == 0:
                    stripped += source[previous_pos:pos]
                depth += 1
                pos += 2
            elif pos+2 <= size and source[pos:pos+2] == '|#':
                if depth <= 0:
                    raise TokenizeError('LispTokenizer.strip_comments: no matching #| found')
                depth -= 1
                pos += 2
                if depth == 0:
                    previous_pos = pos
            elif depth == 0 and source[pos] == ';':
                stripped += source[previous_pos:pos]
                pos = source.find('\n', pos)
                if pos < 0:
                    pos = len(source)    # return from while loop
                else:
                    pos += 1
                previous_pos = pos
            else:
                pos += 1

        if depth != 0:
            raise TokenizeError('LispTokenizer.strip_comments: no matching |# found')
        stripped += source[previous_pos:pos]
        return stripped
    

    def add_proper_space(self, source):
        """
        (トークン化を replace メソッドを用いて簡単に行うために) 文字列リテラル以外の要素について self.table に従い変換処理を行う。
        
        >>> t = LispTokenizer()
        >>> t.add_proper_space("(+ 1 2 3)")
        '( + 1 2 3 )'

        >>> t.add_proper_space('"hoge\\\\"(piyo)"')
        '"hoge\\\\"(piyo)"'

        >>> t.add_proper_space('(progn (print "()") (print " ") (print \\'(1 2 3)))')
        '( progn ( print "()" ) ( print " " ) ( print \\' ( 1 2 3 ) ) )'

        Args:
            source (str)
        Returns:
            translated (str)
        """
        translated = ''
        pos = 0
        previous_pos = 0
        is_in_string_literal = False
        escaped = False
        
        while 0 <= pos < len(source):
            if source[pos] == '\\':
                escaped = not escaped
                pos += 1
            elif source[pos] == '"' and not escaped:
                escaped = False
                if is_in_string_literal:
                    pos += 1    # 追加の対象におわりの " を含めるために、次の読み出し開始点に " を含めないために、先に pos += 1 をする
                    translated += source[previous_pos:pos]
                    previous_pos = pos
                    is_in_string_literal = False
                else:
                    translated += source[previous_pos:pos].translate(self.add_space_table).replace(", @", ",@ ")
                    previous_pos = pos
                    pos += 1    # 追加の対象にはじまりの " を含めないために、次の読み出し開始点にはじまりの " を含めるために、後で pos += 1 をする
                    is_in_string_literal = True
            else:
                escaped = False
                pos += 1
        
        if is_in_string_literal:
            raise TokenizeError('LispTokenizer.add_proper_space: no matching " found')
        translated += source[previous_pos:pos].translate(self.add_space_table).replace(", @", ",@ ")    # ',@L' -> ', @L' -> ',@ L'
        return translated

    
    def lex_string(self, source):
        """
        前処理済みの文字列をうけとり、トークン分割されたリストを作成する。

        >>> t = LispTokenizer()
        >>> t.lex_string('( + 1 2 3 )')
        ['(', '+', '1', '2', '3', ')']

        >>> t.lex_string('"hoge\\\\"(piyo)"')
        ['"hoge\\\\"(piyo)"']

        >>> t.lex_string('( progn ( print "()" ) ( print " " ) ( print \\' ( 1 2 3 ) ) )')
        ['(', 'progn', '(', 'print', '"()"', ')', '(', 'print', '" "', ')', '(', 'print', "'", '(', '1', '2', '3', ')', ')', ')']

        Args:
            source (str)
        Returns:
            token_list (str)
        """
        token_list = []
        pos = 0
        previous_pos = 0
        is_in_string_literal = False
        escaped = False

        while 0 <= pos < len(source):
            if source[pos] == '\\':
                escaped = not escaped
                pos += 1
            elif source[pos] == '"' and not escaped:
                escaped = False
                if is_in_string_literal:
                    pos += 1
                    token_list.append(source[previous_pos:pos])
                    previous_pos = pos
                    is_in_string_literal = False
                else:
                    token_list.extend(source[previous_pos:pos].split())
                    previous_pos = pos
                    pos += 1
                    is_in_string_literal = True
            else:
                escaped = False
                pos += 1
        
        if is_in_string_literal:
            raise TokenizeError('LispTokenizer.lex_string: no matching " found. (Some bugs inside LispTokenizer.add_propser_space?)')
        token_list.extend(source[previous_pos:pos].split())
        return token_list


    def tokenize(self, source):
        """
        ソース文字列のトークン分割
        クォートや括弧などの特殊文字は間に空白文字がなくても別々のトークンに分割される。#' などの複数文字の特殊文字はしっかりまとめて分割される。
        Args:
            source (str)
        Returns:
            token (str)
        """
        stripped = self.strip_comments(source)
        translated = self.add_proper_space(source)
        token = self.lex_string(translated)
        return token



#===================================================================
if __name__ == "__main__":
    import doctest
    fail_count, test_count = doctest.testmod()
    if fail_count == 0:
        print('tested {} cases: doctest passed'.format(test_count))
#===================================================================

# coding: UTF-8
"""
(main program)
> python3 TinyLisp.py [--debug] [--prompt "prompt here"]
or
> python3 TinyLisp.py [--file "subset of Common Lisp file here"]
"""

import sys, argparse
import lisp_built_in, lisp_tokenizer, lisp_parser, lisp_evaluator
from lisp_tokenizer import LispTokenizer
from lisp_parser import LispParser
from lisp_evaluator import LispEvaluator


def arrange(exp):
    """評価後の値 (主にリスト) を読みやすい文字列へと整形する"""
    return "({})".format(" ".join(map(arrange, exp))) if isinstance(exp, list) else special_str(exp)


def special_str(exp):
    """基本的に str() と同じだが、True -> t, False -> nil へ変換を行う"""
    if exp is True:
        return 't'
    elif exp is False:
        return 'nil'
    else:
        return str(exp)


def repl(args):
    """read eval print loop"""
    t = LispTokenizer()
    p = LispParser()
    e = LispEvaluator()
    prompt = args.prompt if args.prompt else "TinyLisp.py > "
    while True:
        s_exp = input(prompt)
        if s_exp == "(exit)":
            print("You are terminating the TinyLisp. Are you sure?  --y/n")
            response = input()
            if response in ("Y", "y", "Yes", "yes"):
                exit()
            elif response in ("N", "n", "No", "no"):
                continue
            else:
                print("Type y/n.")
                continue
        if argparser.parse_args().debug:
            tokenized = t.tokenize(s_exp)
            print("tokenized!\n{}".format(tokenized))
            parsed = p.parse(tokenized)
            print("parsed!\n{}".format(parsed))
            preprocessed = e.preprocess(parsed)
            print("(preprocessed)\n{}".format(preprocessed))
            evaluated = e.eval_1(ast=preprocessed, env=e.global_env, func_env=e.global_func_env, dynamic_env=e.global_env, func_flag=False)
            print("evaluated!\n{}".format(evaluated))
            print("result: {}".format(arrange(evaluated)))
        else:
            print(arrange(e.evaluate(p.parse(t.tokenize(s_exp)))))
        print("")


def processor(args):
    """ファイル内のS式を一気に実行するインタプリタ"""
    t = LispTokenizer()
    p = LispParser()
    e = LispEvaluator()
    file_path = args.file
    with open(file_path) as f:
        s_exps = " ".join([line.strip() for line in f.readlines()])
        whole_s_exp = f"(progn {s_exps})"
        print(arrange(e.evaluate(p.parse(t.tokenize(whole_s_exp)))))




if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument("--debug", help="(optional, for REPL) With --debug, all interim progress is reported.", action="store_true")
    argparser.add_argument("--prompt", help="(optional, for REPL) change the prompt from 'TinyLisp.py > '")
    argparser.add_argument("--file", help="(optional, for file execution)")

    args = argparser.parse_args()
    if args.file:
        processor(args)
    else:
        repl(args)

#!/usr/bin/env python3

from antlr4 import *
import importlib
import sys
from antlr4.error.ErrorListener import ErrorListener

# usage: grammar prefix suffix injection

class MyErrorListener(ErrorListener):

    def __init__(self):
        super(MyErrorListener, self).__init__()

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        raise Exception("syntax error")

def main():
    grammar = sys.argv[1]
    axiom = sys.argv[2]
    prefix = sys.argv[3]
    suffix = sys.argv[4]
    injection = sys.argv[5]

    lexer_name = sys.argv[1]+"Lexer"
    try:
        lexer_module = importlib.import_module(lexer_name)
    except Exception as e:
        print("Lexer",lexer_name,"not found")
        exit(1)
    lexer = getattr(lexer_module, lexer_name)(InputStream(prefix+injection+suffix))
    lexer.addErrorListener(MyErrorListener())

    stream = CommonTokenStream(lexer)

    parser_name = sys.argv[1]+"Parser"
    try:
        parser_module = importlib.import_module(parser_name)
    except Exception as e:
        print("Parser",parser_name,"not found")
        exit(1)
    parser = getattr(parser_module, parser_name)(stream)
    parser.addErrorListener(MyErrorListener())
    try:
        p = getattr(parser, axiom)
    except Exception as e:
        print("Unknown axiom")
        exit(1)
    try:
        p()
    except Exception as e:
        print("Exception:",e)
        exit(180)
    exit(0)

if __name__ == '__main__':
    main()

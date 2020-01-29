from antlr4 import *
import importlib
import sys

# usage: grammar prefix suffix injection

def main():
    grammar = sys.argv[1]
    prefix = sys.argv[2]
    suffix = sys.argv[3]
    injection = sys.argv[4]

    lexer_name = sys.argv[1]+"Lexer"
    lexer_module = importlib.import_module(lexer_name)
    lexer = getattr(lexer_module, lexer_name)(InputStream(prefix+injection+suffix))

    stream = CommonTokenStream(lexer)

    parser_name = sys.argv[1]+"Parser"
    parser_module = importlib.import_module(parser_name)
    parser = getattr(parser_module, parser_name)(stream)
    parser.parse()

if __name__ == '__main__':
    main()

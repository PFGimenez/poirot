from ANTLRv4LexerPythonTarget import *
from ANTLRv4Parser import *
from ListenerForBNF import *
import sys

def show(e, n):
    print(" " * n, type(e))
    for c in e.getChildren():
        show(c, n + 1)

def main():
    lexer = ANTLRv4LexerPythonTarget(StdinStream())
    stream = CommonTokenStream(lexer)
    parser = ANTLRv4Parser(stream)
    tree = parser.grammarSpec()

    poirot_lex = sys.argv[1] if len(sys.argv)>=2 else ""
    poirot_parser = sys.argv[2] if len(sys.argv)>=3 else ""
    printer = ListenerForBNF(poirot_lex, poirot_parser)
    walker = ParseTreeWalker()
    walker.walk(printer, tree)

if __name__ == '__main__':
    main()

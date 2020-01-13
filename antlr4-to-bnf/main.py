from ANTLRv4LexerPythonTarget import *
from ANTLRv4Parser import *
from ListenerForBNF import *


def show(e, n):
    print(" " * n, type(e))
    for c in e.getChildren():
        show(c, n + 1)

def main():
    lexer = ANTLRv4LexerPythonTarget(StdinStream())
    stream = CommonTokenStream(lexer)
    parser = ANTLRv4Parser(stream)
    tree = parser.grammarSpec()
    #show(tree, 0)
    #print(type(tree))

    printer = ListenerForBNF()
    walker = ParseTreeWalker()
    walker.walk(printer, tree)

if __name__ == '__main__':
    main()

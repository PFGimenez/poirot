from ANTLRv4Lexer import *
from ANTLRv4Parser import *
from ANTLRv4ParserListener import *


def show(e, n):
    print(" " * n, type(e))
    for c in e.getChildren():
        show(c, n + 1)

def main():
    lexer = ANTLRv4Lexer(StdinStream())
    stream = CommonTokenStream(lexer)
    parser = ANTLRv4Parser(stream)
    tree = parser.grammarSpec()
    #show(tree, 0)
    #print(type(tree))

    printer = ANTLRv4ParserListener()
    walker = ParseTreeWalker()
    walker.walk(printer, tree)

if __name__ == '__main__':
    main()

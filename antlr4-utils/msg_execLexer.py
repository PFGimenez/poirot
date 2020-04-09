# Generated from msg_exec.g4 by ANTLR 4.8
from antlr4 import *
from io import StringIO
from typing.io import TextIO
import sys



def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\n")
        buf.write("8\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7")
        buf.write("\4\b\t\b\4\t\t\t\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3")
        buf.write("\3\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6")
        buf.write("\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3")
        buf.write("\t\2\2\n\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\3\2\2\2\67")
        buf.write("\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13")
        buf.write("\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\3\23\3")
        buf.write("\2\2\2\5\31\3\2\2\2\7\35\3\2\2\2\t\"\3\2\2\2\13&\3\2\2")
        buf.write("\2\r*\3\2\2\2\17\60\3\2\2\2\21\64\3\2\2\2\23\24\7g\2\2")
        buf.write("\24\25\7z\2\2\25\26\7g\2\2\26\27\7e\2\2\27\30\7\"\2\2")
        buf.write("\30\4\3\2\2\2\31\32\7e\2\2\32\33\7o\2\2\33\34\7f\2\2\34")
        buf.write("\6\3\2\2\2\35\36\7o\2\2\36\37\7u\2\2\37 \7i\2\2 !\7\"")
        buf.write("\2\2!\b\3\2\2\2\"#\7m\2\2#$\7g\2\2$%\7{\2\2%\n\3\2\2\2")
        buf.write("&\'\7\"\2\2\'(\7?\2\2()\7\"\2\2)\f\3\2\2\2*+\7x\2\2+,")
        buf.write("\7c\2\2,-\7n\2\2-.\7w\2\2./\7g\2\2/\16\3\2\2\2\60\61\7")
        buf.write("\"\2\2\61\62\7(\2\2\62\63\7\"\2\2\63\20\3\2\2\2\64\65")
        buf.write("\7\"\2\2\65\66\7=\2\2\66\67\7\"\2\2\67\22\3\2\2\2\3\2")
        buf.write("\2")
        return buf.getvalue()


class msg_execLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    T__0 = 1
    T__1 = 2
    T__2 = 3
    T__3 = 4
    T__4 = 5
    T__5 = 6
    T__6 = 7
    T__7 = 8

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "'exec '", "'cmd'", "'msg '", "'key'", "' = '", "'value'", "' & '", 
            "' ; '" ]

    symbolicNames = [ "<INVALID>",
 ]

    ruleNames = [ "T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", 
                  "T__7" ]

    grammarFileName = "msg_exec.g4"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.8")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None



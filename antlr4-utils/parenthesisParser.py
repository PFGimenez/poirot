# Generated from parenthesis.g4 by ANTLR 4.8
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO


def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\b")
        buf.write("\36\4\2\t\2\4\3\t\3\4\4\t\4\3\2\3\2\3\2\3\3\3\3\3\3\3")
        buf.write("\3\3\3\3\3\3\3\3\3\3\3\5\3\25\n\3\3\4\3\4\3\4\3\4\3\4")
        buf.write("\5\4\34\n\4\3\4\2\2\5\2\4\6\2\2\2\35\2\b\3\2\2\2\4\24")
        buf.write("\3\2\2\2\6\33\3\2\2\2\b\t\5\6\4\2\t\n\7\2\2\3\n\3\3\2")
        buf.write("\2\2\13\f\7\3\2\2\f\r\5\4\3\2\r\16\7\4\2\2\16\25\3\2\2")
        buf.write("\2\17\20\7\5\2\2\20\21\5\4\3\2\21\22\7\6\2\2\22\25\3\2")
        buf.write("\2\2\23\25\7\7\2\2\24\13\3\2\2\2\24\17\3\2\2\2\24\23\3")
        buf.write("\2\2\2\25\5\3\2\2\2\26\34\5\4\3\2\27\30\5\4\3\2\30\31")
        buf.write("\7\b\2\2\31\32\5\6\4\2\32\34\3\2\2\2\33\26\3\2\2\2\33")
        buf.write("\27\3\2\2\2\34\7\3\2\2\2\4\24\33")
        return buf.getvalue()


class parenthesisParser ( Parser ):

    grammarFileName = "parenthesis.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'('", "')'", "'['", "']'", "'a'", "'b'" ]

    symbolicNames = [  ]

    RULE_axiom = 0
    RULE_a = 1
    RULE_s = 2

    ruleNames =  [ "axiom", "a", "s" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    T__4=5
    T__5=6

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.8")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class AxiomContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def s(self):
            return self.getTypedRuleContext(parenthesisParser.SContext,0)


        def EOF(self):
            return self.getToken(parenthesisParser.EOF, 0)

        def getRuleIndex(self):
            return parenthesisParser.RULE_axiom

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAxiom" ):
                listener.enterAxiom(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAxiom" ):
                listener.exitAxiom(self)




    def axiom(self):

        localctx = parenthesisParser.AxiomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_axiom)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 6
            self.s()
            self.state = 7
            self.match(parenthesisParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def a(self):
            return self.getTypedRuleContext(parenthesisParser.AContext,0)


        def getRuleIndex(self):
            return parenthesisParser.RULE_a

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterA" ):
                listener.enterA(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitA" ):
                listener.exitA(self)




    def a(self):

        localctx = parenthesisParser.AContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_a)
        try:
            self.state = 18
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [parenthesisParser.T__0]:
                self.enterOuterAlt(localctx, 1)
                self.state = 9
                self.match(parenthesisParser.T__0)
                self.state = 10
                self.a()
                self.state = 11
                self.match(parenthesisParser.T__1)
                pass
            elif token in [parenthesisParser.T__2]:
                self.enterOuterAlt(localctx, 2)
                self.state = 13
                self.match(parenthesisParser.T__2)
                self.state = 14
                self.a()
                self.state = 15
                self.match(parenthesisParser.T__3)
                pass
            elif token in [parenthesisParser.T__4]:
                self.enterOuterAlt(localctx, 3)
                self.state = 17
                self.match(parenthesisParser.T__4)
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class SContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def a(self):
            return self.getTypedRuleContext(parenthesisParser.AContext,0)


        def s(self):
            return self.getTypedRuleContext(parenthesisParser.SContext,0)


        def getRuleIndex(self):
            return parenthesisParser.RULE_s

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterS" ):
                listener.enterS(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitS" ):
                listener.exitS(self)




    def s(self):

        localctx = parenthesisParser.SContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_s)
        try:
            self.state = 25
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,1,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 20
                self.a()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 21
                self.a()
                self.state = 22
                self.match(parenthesisParser.T__5)
                self.state = 23
                self.s()
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx






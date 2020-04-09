# Generated from msg_exec.g4 by ANTLR 4.8
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
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\n")
        buf.write(",\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\3\2\3\2\3\2")
        buf.write("\3\3\3\3\3\3\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3")
        buf.write("\5\5\5\36\n\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6")
        buf.write("\5\6*\n\6\3\6\2\2\7\2\4\6\b\n\2\2\2*\2\f\3\2\2\2\4\17")
        buf.write("\3\2\2\2\6\22\3\2\2\2\b\35\3\2\2\2\n)\3\2\2\2\f\r\5\n")
        buf.write("\6\2\r\16\7\2\2\3\16\3\3\2\2\2\17\20\7\3\2\2\20\21\7\4")
        buf.write("\2\2\21\5\3\2\2\2\22\23\7\5\2\2\23\24\5\b\5\2\24\7\3\2")
        buf.write("\2\2\25\26\7\6\2\2\26\27\7\7\2\2\27\36\7\b\2\2\30\31\7")
        buf.write("\6\2\2\31\32\7\7\2\2\32\33\7\b\2\2\33\34\7\t\2\2\34\36")
        buf.write("\5\b\5\2\35\25\3\2\2\2\35\30\3\2\2\2\36\t\3\2\2\2\37*")
        buf.write("\5\4\3\2 !\5\4\3\2!\"\7\n\2\2\"#\5\n\6\2#*\3\2\2\2$*\5")
        buf.write("\6\4\2%&\5\6\4\2&\'\7\n\2\2\'(\5\n\6\2(*\3\2\2\2)\37\3")
        buf.write("\2\2\2) \3\2\2\2)$\3\2\2\2)%\3\2\2\2*\13\3\2\2\2\4\35")
        buf.write(")")
        return buf.getvalue()


class msg_execParser ( Parser ):

    grammarFileName = "msg_exec.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'exec '", "'cmd'", "'msg '", "'key'", 
                     "' = '", "'value'", "' & '", "' ; '" ]

    symbolicNames = [  ]

    RULE_axiom = 0
    RULE_exe = 1
    RULE_msg = 2
    RULE_params = 3
    RULE_s = 4

    ruleNames =  [ "axiom", "exe", "msg", "params", "s" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    T__4=5
    T__5=6
    T__6=7
    T__7=8

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
            return self.getTypedRuleContext(msg_execParser.SContext,0)


        def EOF(self):
            return self.getToken(msg_execParser.EOF, 0)

        def getRuleIndex(self):
            return msg_execParser.RULE_axiom

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAxiom" ):
                listener.enterAxiom(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAxiom" ):
                listener.exitAxiom(self)




    def axiom(self):

        localctx = msg_execParser.AxiomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_axiom)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 10
            self.s()
            self.state = 11
            self.match(msg_execParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExeContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser


        def getRuleIndex(self):
            return msg_execParser.RULE_exe

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExe" ):
                listener.enterExe(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExe" ):
                listener.exitExe(self)




    def exe(self):

        localctx = msg_execParser.ExeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_exe)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 13
            self.match(msg_execParser.T__0)
            self.state = 14
            self.match(msg_execParser.T__1)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class MsgContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def params(self):
            return self.getTypedRuleContext(msg_execParser.ParamsContext,0)


        def getRuleIndex(self):
            return msg_execParser.RULE_msg

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMsg" ):
                listener.enterMsg(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMsg" ):
                listener.exitMsg(self)




    def msg(self):

        localctx = msg_execParser.MsgContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_msg)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 16
            self.match(msg_execParser.T__2)
            self.state = 17
            self.params()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ParamsContext(ParserRuleContext):

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def params(self):
            return self.getTypedRuleContext(msg_execParser.ParamsContext,0)


        def getRuleIndex(self):
            return msg_execParser.RULE_params

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterParams" ):
                listener.enterParams(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitParams" ):
                listener.exitParams(self)




    def params(self):

        localctx = msg_execParser.ParamsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_params)
        try:
            self.state = 27
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,0,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 19
                self.match(msg_execParser.T__3)
                self.state = 20
                self.match(msg_execParser.T__4)
                self.state = 21
                self.match(msg_execParser.T__5)
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 22
                self.match(msg_execParser.T__3)
                self.state = 23
                self.match(msg_execParser.T__4)
                self.state = 24
                self.match(msg_execParser.T__5)
                self.state = 25
                self.match(msg_execParser.T__6)
                self.state = 26
                self.params()
                pass


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

        def exe(self):
            return self.getTypedRuleContext(msg_execParser.ExeContext,0)


        def s(self):
            return self.getTypedRuleContext(msg_execParser.SContext,0)


        def msg(self):
            return self.getTypedRuleContext(msg_execParser.MsgContext,0)


        def getRuleIndex(self):
            return msg_execParser.RULE_s

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterS" ):
                listener.enterS(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitS" ):
                listener.exitS(self)




    def s(self):

        localctx = msg_execParser.SContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_s)
        try:
            self.state = 39
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,1,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 29
                self.exe()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 30
                self.exe()
                self.state = 31
                self.match(msg_execParser.T__7)
                self.state = 32
                self.s()
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 34
                self.msg()
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 35
                self.msg()
                self.state = 36
                self.match(msg_execParser.T__7)
                self.state = 37
                self.s()
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx






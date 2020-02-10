from ANTLRv4ParserListener import *
from ANTLRv4Parser import ANTLRv4Parser
import inspect
import pprint
import sys

lhs = None
mark = "__"

# Counters for new non-terminals
counters = {}

parser_or_lexer = None
first_lhs = True

# Used to translate "NOT"
char_table = [chr(i) for i in range(32, 127)]

def escape(s):
    if all([ord(c)>=32 and ord(c)<=126 for c in s]):
        l = s.split("'")
        out = "'"+("' "+str(ord('\''))+" '").join(l)+"'"
        out = out.replace("\'\'","")
    else:
        for i in s:
            out += " "+str(ord(i))
    return out

# nt: non-terminal
# suff: integer used for the creation of new non-terminal (with mark)
# rex: the regular expression to translate
def lexer_regexp_to_bnf(nt, suf, rex):
    fname = inspect.currentframe().f_code.co_name
    print("# BEGIN LEXER FOR ", nt, rex[0])

    # First part of the rule: will be completed with the translation of rex.
    rule = "<" + nt + "> ::="
    if suf != 0: rule = "<" + nt + mark + str(suf) + "> ::="

    # The translation depends on the type of rex.
    if rex[0] == "and":
        for child in rex[1]:
            if child[0] in ["string"]:
                # Direct translation for token and string.
                assert(child[1][0] == '\'')
                assert(child[1][-1] == '\'')
                rule += " " + escape(child[1][1::-1])
                #rule += " '" + escape(child[1][1:-1]) + "'"
            elif child[0] in ["token"]:
                # Direct translation for token and string.
                rule += " <" + child[1] + ">"
            else:
                # For complex element (sub-and, sub-or, star, etc.), we must create
                # a new non-terminal that will generate the corresponding language.
                # Notice that we don't print the rule yet: the recursive call will
                # print the new rules for this new non-terminal first!
                new_suf = counters[lhs] + 1
                counters[lhs] = new_suf
                rule += " <" + nt + mark + str(new_suf) + ">"
                lexer_regexp_to_bnf(nt, new_suf, child)
        print(rule + " ;")

    elif rex[0] == "or":
        # Translate for one rule per alternative for this non-terminal.
        for child in rex[1]:
            lexer_regexp_to_bnf(nt, suf, child)

    elif rex[0] == "eps":
        print(rule + " ;")

    elif rex[0] == "string":
        print(rule + " " + escape(rex[1]) +" ;")

    elif rex[0] == "token":
        print(rule + " <" + rex[1] + "> ;")

    elif rex[0] == "*":
        # The translation of '*' need one new non-terminal.
        # For instance: X -> a | b*
        # =>  X ::= a
        #     X ::= Y
        #     Y ::=
        #     Y ::= b Y
        # We need the rule 'X ::= Y'. The following translation leads to an error:
        # =>  X ::= a
        #     X ::=
        #     X ::= b X
        # Because it generates the language (a | b)*.
        new_suf = counters[lhs] + 1
        counters[lhs] = new_suf
        rule += " <" + nt + mark + str(new_suf) + ">"
        print(rule+ " ;")  # X ::= Y
        rule = "<" + nt + mark + str(new_suf) + "> ::="
        print(rule+ " ;")  # Y ::=
        # XXX Token is not necessarily the best name: It should be rule?
        lexer_regexp_to_bnf(nt, new_suf, ("and", [rex[1], ("token", nt + mark + str(new_suf))])) # Y ::= b Y

    elif rex[0] == "+":
        # See '*'. In this translation, we use a second non-terminal to translate the b part of the regex only once
        # We hope that, this way, we reduce the number of rules.
        new_suf1 = counters[lhs] + 1
        new_suf2 = counters[lhs] + 2
        counters[lhs] = new_suf2
        rule += " <" + nt + mark + str(new_suf1) + ">"
        print(rule+ " ;")
        rule = "<" + nt + mark + str(new_suf1) + "> ::="
        print(rule + " <" + nt + mark + str(new_suf2)+"> ;")
        print(rule + " <" + nt + mark + str(new_suf2) + "> <" + nt + mark + str(new_suf1)+"> ;")
        lexer_regexp_to_bnf(nt, new_suf2, rex[1])

    elif rex[0] == "?":
        print(rule+ " ;")                   # X ::=
        lexer_regexp_to_bnf(nt, suf, rex[1]) # X ::= b

    elif rex[0] == ".":
        for x in char_table:
            print(rule + (" %s ;" % escape(x)))

    elif rex[0] == "char_set":
        if rex[1][0] == '[' and rex[1][-1] == ']':
            flat = []
            s = rex[1][1:-1]
            i = 0
            while i < len(s):
                sa = s[i]
                if sa == '\\':
                    if i + 1 >= len(s): raise Exception("Wrong range set: ", nt, suf, rex, fname)
                    i = i + 1
                    sa = s[i]

                # "i + 2" because we need at least two more characters
                if i + 2 < len(s) and s[i + 1] == '-':
                    i = i + 2
                    if i >= len(s): raise Exception("Wrong range set: ", nt, suf, rex, fname)
                    sb = s[i]
                    if sb == '\\':
                        if i + 1 >= len(s): raise Exception("Wrong range set: ", nt, suf, rex, fname)
                        i = i + 1
                        sb = s[i]

                    for j in range(ord(sa), ord(sb) + 1):
                        print(rule + " " + escape(chr(j)) + " ;")
                        # if chr(j) == '\'':
                        #     print(rule + (" '\'' ; #X6"))
                        # elif chr(j) == '"':
                        #     print(rule + (" '\\\"' ; #X5"))
                        # elif chr(j) == '\\':
                        #     print(rule + (" '\\\\' ; #X5"))
                        # else:
                        #     print(rule + (" '%s' ; #X4" % chr(j)))

                else:
                    print(rule + escape(sa) + " ;")
                    # if sa == '\'':
                    #     print(rule + (" '\\'' ; #X1"))
                    # elif sa == '"':
                    #     print(rule + (" '\\\"' ; #X2"))
                    # elif sa == '\\':
                    #     print(rule + (" '\\\\' ; #X2"))
                    # else:
                    #     print(rule + (" '%c' ; #X3 %s" % (sa, sa)))

                i = i + 1

        else:
            raise Exception("Not yet handled: ", nt, suf, rex, fname)

    elif rex[0] == "not" and rex[1][0] == "or":
        # Replace NOT OR with AND NOT.
        rex = ("and", [("not", x) for x in rex[1][1]])
        lexer_regexp_to_bnf(nt, suf, rex)

    elif rex[0] == "not" and rex[1][0] == "token":
        if rex[1][1][0] == '\'':
            c = None
            if len(rex[1][1]) == 3 and rex[1][1][2] == '\'':
                c = rex[1][1][1]
            elif len(rex[1][1]) == 4 and rex[1][1][-1] == '\'' and rex[1][1][1] == '\\':
                c = rex[1][1][2]

            if c is not None:
                for x in char_table:
                    if x != c:
                        print(rule + (" %s ;" % escape(x)))
            else:
                raise Exception("Not yet handled: ", nt, suf, rex, fname)
        elif rex[1][1][0] == '[':
            if rex[1][1][-1] == ']':
                flat = []
                s = rex[1][1][1:-1]
                i = 0
                while i < len(s):
                    if s[i] != '\\':
                        flat.append(s[i])
                    elif i + 1 == len(s):
                        raise Exception("Wrong escape: ", nt, suf, rex, fname)
                    else:
                        flat.append(s[i + 1])
                        i = i + 1
                    i = i +1
                rem = [x for x in char_table if x not in flat]
                for x in rem:
                    print(rule + (" %s ;" % escape(x)))
            else:
                raise Exception("Not yet handled: ", nt, suf, rex, fname)

        else:
            raise Exception("Not yet handled: ", nt, suf, rex, fname)

    else:
        raise Exception("Not yet handled: ", nt, suf, rex, fname)

    print("# END")

# See lexer_regexp_to_bnf.
def parser_regexp_to_bnf(nt, suf, rex):
    fname = inspect.currentframe().f_code.co_name

    rule = "<" + nt + "> ::="
    if suf != 0: rule = "<" + nt + mark + str(suf) + "> ::="

    if rex[0] == "and":
        for child in rex[1]:
            if child[0] in ["string"]:
                rule += " " + escape(child[1]) + ""
            elif child[0] in ["token", "rule"]:
                rule += " <" + (child[1]) + ">"
            else:
                new_suf = counters[lhs] + 1
                counters[lhs] = new_suf
                rule += " <" + nt + mark + str(new_suf) + ">"
                parser_regexp_to_bnf(nt, new_suf, child)
        print(rule+ " ;")

    elif rex[0] == "or":
        for child in rex[1]:
            parser_regexp_to_bnf(nt, suf, child)

    elif rex[0] == "*":
        new_suf = counters[lhs] + 1
        counters[lhs] = new_suf
        rule += " <" + nt + mark + str(new_suf) + ">"
        print(rule+ " ;")
        rule = "<" + nt + mark + str(new_suf) + "> ::="
        print(rule+ " ;")
        parser_regexp_to_bnf(nt, new_suf, ("and", [rex[1], ("token", nt + mark + str(new_suf))])) # Y ::= b Y

    elif rex[0] == "+":
        new_suf1 = counters[lhs] + 1
        new_suf2 = counters[lhs] + 2
        counters[lhs] = new_suf2
        rule += " <" + nt + mark + str(new_suf1) + ">"
        print(rule+ " ;")
        rule = "<" + nt + mark + str(new_suf1) + "> ::="
        print(rule + " <" + nt + mark + str(new_suf2) + "> ;")
        print(rule + " <" + nt + mark + str(new_suf2) + "> <" + nt + mark + str(new_suf1) + "> ;")
        parser_regexp_to_bnf(nt, new_suf2, rex[1])

    elif rex[0] == "?":
        print(rule+ " ;")
        parser_regexp_to_bnf(nt, suf, rex[1])

    elif rex[0] == "eps":
        print(rule+ " ;")

    elif rex[0] == "string":
        print(rule + " " + escape(rex[1]) + " ;")

    elif rex[0] == "token":
        print(rule + " <" + rex[1] + "> ;")

    elif rex[0] == "rule":
        print(rule + " <" + rex[1] + "> ;")

    else:
        raise Exception("Error parser_regexp_to_bnf: " + str(rex))

def do_suffix(suffix, child, ctx):
    if suffix.STAR() is not None and len(suffix.QUESTION()) == 0:
        ctx.X_REGEXP = ("*", child)
    elif suffix.PLUS() is not None and len(suffix.QUESTION()) == 0:
        ctx.X_REGEXP = ("+", child)
    elif len(suffix.QUESTION()) == 1:
        ctx.X_REGEXP = ("?", child)
    else:
        raise Exception("do_suffix not handled: " + str([suffix.QUESTION(), suffix.PLUS(), suffix.STAR()]))

# This class defines a complete listener for a parse tree produced by ANTLRv4Parser.
class ListenerForBNF(ANTLRv4ParserListener):

    def exitGrammarDecl(self, ctx:ANTLRv4Parser.GrammarDeclContext):
        print("# Grammar name: ", ctx.getChild(1).X_REGEXP)

    def enterGrammarType(self, ctx:ANTLRv4Parser.GrammarTypeContext):
        print("# Grammar LEXER=", ctx.LEXER())
        print("# Grammar PARSER=", ctx.PARSER())
        print("# Grammar GRAMMAR=", ctx.GRAMMAR())

    def enterParserRuleSpec(self, ctx:ANTLRv4Parser.ParserRuleSpecContext):
        global lhs
        name = str(ctx.RULE_REF())
        if ' ' in name:
            raise Exception("Rule name with space not handled: " + name)
        if mark in name:
            raise Exception(("Rule name with '%s' not handled: " % mark) + name)
        lhs = name

        global first_lhs

        if first_lhs:
            print("<" + lhs + "> ;")
        first_lhs = False
        print("# PARSER: " + lhs)

        global parser_or_lexer
        parser_or_lexer = "parser"

    def exitRuleAltList(self, ctx:ANTLRv4Parser.RuleAltListContext):
        global lhs
        if lhs is None:
            raise Exception("Rule with None lhs")

        children = list(ctx.getChildren())
        # Get one child in two, to jump 'OR' (see ruleAltList rule).
        for i in range(0, len(children), 2):
            child = children[i]
            if lhs not in counters: counters[lhs] = 0
            parser_regexp_to_bnf(lhs, 0, child.X_REGEXP)
        lhs = None

    def exitLabeledAlt(self, ctx:ANTLRv4Parser.LabeledAltContext):
        if ctx.POUND() is not None:
            raise Exception("LabeledAlt with (POUND identifier) not handled yet")
        ctx.X_REGEXP = ctx.alternative().X_REGEXP

    def exitAltList(self, ctx:ANTLRv4Parser.AltListContext):
        children = list(ctx.alternative())

        if len(children) == 1:
            ctx.X_REGEXP = children[0].X_REGEXP
        else:
            rex = ("or", [child.X_REGEXP for child in children])
            ctx.X_REGEXP = rex

    def exitAlternative(self, ctx:ANTLRv4Parser.AlternativeContext):
        # TODO:
        #if ctx.elementOptions() is not None:
        #    pprint.pprint(ctx)
        #    pprint.pprint(ctx.elementOptions())
        #    raise Exception("Not yet handled: alternative with elementOptions")
        if ctx.elementOptions() is not None:
            print("Not yet handled: alternative with elementOptions", file=sys.stderr)
        children = ctx.element()
        if len(children) == 1:
            ctx.X_REGEXP = children[0].X_REGEXP
        else:
            rex = ("and", [child.X_REGEXP for child in children])
            ctx.X_REGEXP = rex

    def exitEbnf(self, ctx:ANTLRv4Parser.EbnfContext):
        child = ctx.block().X_REGEXP
        suffix = ctx.blockSuffix()

        if suffix is None:
            ctx.X_REGEXP = child
        else:
            suffix = suffix.ebnfSuffix()
            do_suffix(suffix, child, ctx)

    def enterBlockSuffix(self, ctx:ANTLRv4Parser.BlockSuffixContext):
        # blockSuffix is a copy of ebnfSuffix
        ctx.QUESTION = ctx.ebnfSuffix().QUESTION
        ctx.STAR = ctx.ebnfSuffix().STAR
        ctx.PLUS = ctx.ebnfSuffix().PLUS

    def exitBlock(self, ctx:ANTLRv4Parser.BlockContext):
        if ctx.getChildCount() != 3:
            raise Exception("ruleAction in block not yet handled")
        ctx.X_REGEXP = ctx.altList().X_REGEXP

    def enterIdentifier(self, ctx:ANTLRv4Parser.IdentifierContext):
        if ctx.RULE_REF() is not None:
            ctx.X_REGEXP = ("rule", str(ctx.RULE_REF()))
        else:
            ctx.X_REGEXP = ("token", str(ctx.TOKEN_REF()))

    def exitAtom(self, ctx:ANTLRv4Parser.AtomContext):
        fname = inspect.currentframe().f_code.co_name
        if ctx.terminal() is not None:
            ctx.X_REGEXP = ctx.terminal().X_REGEXP
        elif ctx.ruleref() is not None:
            ctx.X_REGEXP = ctx.ruleref().X_REGEXP
        elif ctx.notSet() is not None:
            raise Exception("Not yet handled (notSet): " + fname)
        elif ctx.DOT() is not None:
            raise Exception("Not yet handled (DOT): " + fname)
        else:
            raise Exception("Not yet handled (???): " + fname)

    def exitRuleref(self, ctx:ANTLRv4Parser.RulerefContext):
        fname = inspect.currentframe().f_code.co_name
        if ctx.argActionBlock() is not None:
            raise Exception("Not yet handled (ActionBlock): " + fname)
        elif ctx.elementOptions() is not None:
            raise Exception("Not yet handled (elementOptions): " + fname)
        ctx.X_REGEXP = ("rule", str(ctx.RULE_REF()))

    def exitTerminal(self, ctx:ANTLRv4Parser.TerminalContext):
        if ctx.elementOptions() is not None:
            raise Exception("Not yet handled (elementOptions): " + fname)

        if ctx.TOKEN_REF() is not None:
            ctx.X_REGEXP = ("token", str(ctx.TOKEN_REF()))
        else:
            ctx.X_REGEXP = ("string", str(ctx.STRING_LITERAL()))

    def exitElement(self, ctx:ANTLRv4Parser.ElementContext):
        if ctx.actionBlock() is not None:
            # TODO: verify with http://lms.ui.ac.ir/public/group/90/59/01/15738_ce57.pdf
            # that we can translate this part of the grammar to eps (i.e., it does not change
            # the parser behavior).
            ctx.X_REGEXP = ("eps",)
        elif ctx.labeledElement() is not None:
            child = ctx.labeledElement().X_REGEXP
            suffix = ctx.ebnfSuffix()

            if suffix is None:
                ctx.X_REGEXP = child
            else:
                do_suffix(suffix, child, ctx)
        elif ctx.ebnf() is not None:
            child = ctx.ebnf()
            ctx.X_REGEXP = child.X_REGEXP
        elif ctx.atom() is not None:
            child = ctx.atom().X_REGEXP
            suffix = ctx.ebnfSuffix()
            if suffix is None:
                ctx.X_REGEXP = child
            else:
                do_suffix(suffix, child, ctx)
        else:
            raise Exception("Not yet handled (???): " + fname)

#    def enterEbnfSuffix(self, ctx:ANTLRv4Parser.EbnfSuffixContext):
#    NOTHING: values are retreived directly from calling rules

    #######
    # LEXER
    #######

    def enterLexerRuleSpec(self, ctx:ANTLRv4Parser.LexerRuleSpecContext):
        global parser_or_lexer
        parser_or_lexer = "lexer"
        print("# LEXER: " + str(ctx.TOKEN_REF()))

    def exitLexerRuleSpec(self, ctx:ANTLRv4Parser.LexerRuleSpecContext):
        name = str(ctx.TOKEN_REF())
        if ' ' in name:
            raise Exception("Lexer name with space not handled: " + name)
        if mark in name:
            raise Exception(("Lexer name with '%s' not handled: " % mark) + name)
        if name not in counters: counters[lhs] = 0
        lexer_regexp_to_bnf(name, 0, ctx.lexerRuleBlock().lexerAltList().X_REGEXP)

#    def exitLexerRuleBlock(self, ctx:ANTLRv4Parser.LexerRuleBlockContext):
#    NOTHING: values are retreived directly from calling rules

    def exitLexerAltList(self, ctx:ANTLRv4Parser.LexerAltListContext):
        children = list(ctx.lexerAlt())

        if len(children) == 1:
            ctx.X_REGEXP = children[0].X_REGEXP
        else:
            rex = ("or", [children[i].X_REGEXP for i in range(0, len(children), 1)])
            ctx.X_REGEXP = rex

    def exitLexerAlt(self, ctx:ANTLRv4Parser.LexerAltContext):
        # TODO: We ignore lexerCommands
        if ctx.lexerElements() is None:
            ctx.X_REGEXP = ["eps"]
        else:
            ctx.X_REGEXP = ctx.lexerElements().X_REGEXP

    def exitLexerElements(self, ctx:ANTLRv4Parser.LexerElementsContext):
        children = ctx.lexerElement()
        if len(children) == 1:
            ctx.X_REGEXP = children[0].X_REGEXP
        else:
            rex = ("and", [child.X_REGEXP for child in children])
            ctx.X_REGEXP = rex

    def exitLexerElement(self, ctx:ANTLRv4Parser.LexerElementContext):
        if ctx.actionBlock() is not None:
            # TODO: verify with http://lms.ui.ac.ir/public/group/90/59/01/15738_ce57.pdf
            # that we can translate this part of the grammar to eps (i.e., it does not change
            # the parser behavior).
            ctx.X_REGEXP = ("eps",)
        elif ctx.labeledLexerElement() is not None:
            child = ctx.labeledLexerElement().X_REGEXP
            suffix = ctx.ebnfSuffix()

            if suffix is None:
                ctx.X_REGEXP = child
            else:
                do_suffix(suffix, child, ctx)
        elif ctx.lexerBlock() is not None:
            child = ctx.lexerBlock().X_REGEXP
            suffix = ctx.ebnfSuffix()
            if suffix is None:
                ctx.X_REGEXP = child
            else:
                do_suffix(suffix, child, ctx)
        elif ctx.lexerAtom() is not None:
            child = ctx.lexerAtom().X_REGEXP
            suffix = ctx.ebnfSuffix()
            if suffix is None:
                ctx.X_REGEXP = child
            else:
                do_suffix(suffix, child, ctx)
        else:
            raise Exception("Not yet handled (???): " + fname)

    def exitLexerAtom(self, ctx:ANTLRv4Parser.LexerAtomContext):
        fname = inspect.currentframe().f_code.co_name
        if ctx.terminal() is not None:
            ctx.X_REGEXP = ctx.terminal().X_REGEXP
        elif ctx.notSet() is not None:
            ctx.X_REGEXP = ctx.notSet().X_REGEXP
        elif ctx.DOT() is not None:
            if ctx.elementOptions() is not None:
                raise Exception("Not yet handled (DOT with elementOptions): " + fname)
            ctx.X_REGEXP = (".", )
        elif ctx.LEXER_CHAR_SET() is not None:
            ctx.X_REGEXP = ("char_set", str(ctx.LEXER_CHAR_SET()))
        else:
            raise Exception("Not yet handled (???): " + fname)

    def exitNotSet(self, ctx:ANTLRv4Parser.NotSetContext):
        global parser_or_lexer
        if parser_or_lexer != "lexer":
            raise Exception("NotSet with parser not yet handled: " + fname)

        if ctx.setElement() is not None:
            ctx.X_REGEXP = ("not", ctx.setElement().X_REGEXP)
        else:
            ctx.X_REGEXP = ("not", ctx.blockSet().X_REGEXP)

    def exitBlockSet(self, ctx:ANTLRv4Parser.BlockSetContext):
        children = [child.X_REGEXP for child in ctx.setElement()]
        if len(children) == 1:
            ctx.X_REGEXP = children[0]
        else:
            ctx.X_REGEXP = ("or", children)

    def exitSetElement(self, ctx:ANTLRv4Parser.SetElementContext):
        if ctx.elementOptions() is not None:
            raise Exception("Not yet handled: setElement with elementOptions")

        if ctx.TOKEN_REF() is not None:
            ctx.X_REGEXP = ("token", str(ctx.TOKEN_REF()))
        elif ctx.STRING_LITERAL() is not None:
            ctx.X_REGEXP = ("token", (ctx.STRING_LITERAL()).getText())
        elif ctx.LEXER_CHAR_SET() is not None:
            ctx.X_REGEXP = ("token", str(ctx.LEXER_CHAR_SET()))
        else:
            fname = inspect.currentframe().f_code.co_name
            raise Exception("Not yet handled (???): " + fname)

    def exitLexerBlock(self, ctx:ANTLRv4Parser.LexerBlockContext):
        ctx.X_REGEXP = ctx.lexerAltList().X_REGEXP

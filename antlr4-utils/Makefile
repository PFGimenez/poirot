all: SQLite.bnf
grammarv4_commit = 31f313996d1be86df4cb0cc9af50051f4051432f
antlr_version = 4.8

# ANTLR4 processing

ANTLRv4LexerPythonTarget.py: antlr-$(antlr_version)-complete.jar ANTLRv4LexerPythonTarget.g4 ANTLRv4Parser.g4 LexerAdaptor.py LexBasic.g4
	java -jar antlr-$(antlr_version)-complete.jar -Dlanguage=Python3 ANTLRv4LexerPythonTarget.g4

ANTLRv4Parser.py: antlr-$(antlr_version)-complete.jar ANTLRv4LexerPythonTarget.g4 ANTLRv4Parser.g4 LexerAdaptor.py LexBasic.g4
	java -jar antlr-$(antlr_version)-complete.jar -Dlanguage=Python3 ANTLRv4Parser.g4

ANTLRv4LexerPythonTarget.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/antlr/antlr4/ANTLRv4LexerPythonTarget.g4

ANTLRv4Parser.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/antlr/antlr4/ANTLRv4Parser.g4
	sed -i 's/ANTLRv4Lexer/ANTLRv4LexerPythonTarget/' ANTLRv4Parser.g4

LexerAdaptor.py:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/antlr/antlr4/LexerAdaptor.py

LexBasic.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/antlr/antlr4/LexBasic.g4

antlr-$(antlr_version)-complete.jar:
	wget https://www.antlr.org/download/antlr-$(antlr_version)-complete.jar

# Generate the lexer and parsers file from the grammar

PhpLexer.py: PhpLexer.g4 PhpParser.g4 antlr-$(antlr_version)-complete.jar
	java -jar antlr-$(antlr_version)-complete.jar -Dlanguage=Python3 PhpLexer.g4 PhpParser.g4

XMLLexer.py: XMLLexer.g4 XMLParser.g4 antlr-$(antlr_version)-complete.jar
	java -jar antlr-$(antlr_version)-complete.jar -Dlanguage=Python3 XMLLexer.g4 XMLParser.g4

%Lexer.py: %.g4 antlr-$(antlr_version)-complete.jar
	java -jar antlr-$(antlr_version)-complete.jar -Dlanguage=Python3 $<

# Conversion from .g4 to .bnf

SQLite.bnf: SQLite.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat SQLite.g4 | python3 antlr4-to-bnf.py true "<WS_POIROT>" > SQLite.bnf
	echo "<WS_POIROT> ::= ' ';" >> SQLite.bnf
#	echo "<WS_POIROT> ::= <MULTILINE_COMMENT>;" >> SQLite.bnf

Lua.bnf: SQLite.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat Lua.g4 | python3 antlr4-to-bnf.py true "" "' '" > Lua.bnf

PHP.bnf: PhpLexer.g4 PhpParser.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat PhpParser.g4 | python3 antlr4-to-bnf.py true "' '" > $@
	cat PhpLexer.g4 | python3 antlr4-to-bnf.py false "' '" >> $@

XML.bnf: XMLLexer.g4 XMLParser.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat XMLParser.g4 | python3 antlr4-to-bnf.py true "' '" > $@
	cat XMLLexer.g4 | python3 antlr4-to-bnf.py false "' '" >> $@

XMLexample.bnf: ldap.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat XMLexample.g4 | python3 antlr4-to-bnf.py true > XMLexample.bnf

ldap.bnf: ldap.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat ldap.g4 | python3 antlr4-to-bnf.py true > ldap.bnf

mime.bnf: mime.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat mime.g4 | python3 antlr4-to-bnf.py true > mime.bnf

%.bnf: %.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat $< | python3 antlr4-to-bnf.py true "' '" > $@

# Some grammars

PhpLexer.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/php/PhpLexer.g4
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/php/PhpParser.g4
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/php/Python/PhpBaseLexer.py
	patch < PhpLexer.diff
	patch < PhpParser.diff

XMLLexer.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/xml/XMLLexer.g4
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/xml/XMLParser.g4
	patch < XMLLexer.diff
	patch < XMLParser.diff

SQLite.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/sql/sqlite/SQLite.g4
	patch < SQLite.diff

Lua.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/$(grammarv4_commit)/lua/Lua.g4

# Remove generated files

clean:
	rm -f *.bnf *Parser.py *Lexer.py

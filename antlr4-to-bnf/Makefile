all: SQLite.bnf

# ANTLR4 processing

ANTLRv4LexerPythonTarget.py: antlr-4.7.2-complete.jar ANTLRv4LexerPythonTarget.g4 ANTLRv4Parser.g4 LexerAdaptor.py LexBasic.g4
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Python3 ANTLRv4LexerPythonTarget.g4

ANTLRv4Parser.py: antlr-4.7.2-complete.jar ANTLRv4LexerPythonTarget.g4 ANTLRv4Parser.g4 LexerAdaptor.py LexBasic.g4
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Python3 ANTLRv4Parser.g4

ANTLRv4LexerPythonTarget.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/ANTLRv4LexerPythonTarget.g4

ANTLRv4Parser.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/ANTLRv4Parser.g4
	sed -i 's/ANTLRv4Lexer/ANTLRv4LexerPythonTarget/' ANTLRv4Parser.g4

LexerAdaptor.py:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/LexerAdaptor.py

LexBasic.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/antlr4/LexBasic.g4

antlr-4.7.2-complete.jar:
	wget https://www.antlr.org/download/antlr-4.7.2-complete.jar

# Generate the lexer and parsers file from the grammar

%Lexer.py %Parser.py: %.g4
	java -jar antlr-4.7.2-complete.jar -Dlanguage=Python3 $<

# Conversion from .g4 to .bnf

%.bnf: %.g4 ANTLRv4LexerPythonTarget.py ANTLRv4Parser.py ListenerForBNF.py antlr4-to-bnf.py
	cat $< | python3 antlr4-to-bnf.py > $@

# Some grammars

SQLite.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/sqlite/SQLite.g4

Lua.g4:
	wget https://raw.githubusercontent.com/antlr/grammars-v4/master/lua/Lua.g4

# Remove generated files

clean:
	rm -f *.bnf *Parser.py *Lexer.py

# How to install ANTLR4

_Prerequisite:_ Java (JRE) 1.7

1. Download https://www.antlr.org/download/antlr-4.7.2-complete.jar

2. Execute in shell:

    $ sudo cp antlr-4.7.2-complete.jar /usr/local/lib/

3. Add to your .bashrc:

    export CLASSPATH=".:/usr/local/lib/antlr-4.7.2-complete.jar:$CLASSPATH"

That's all!

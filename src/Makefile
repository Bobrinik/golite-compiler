CC=gcc

FLEX=lex.yy.c y.tab.c y.tab.h
DEPS=main.c ast/weeder.c ast/typecheck.c ast/tree.c ast/symbol.c ast/pretty.c ast/codegen.c utils/memory.c utils/util.c

all:
	flex parser/golite.l
	bison --yacc -d parser/golite.y
	$(CC) -Wall $(FLEX) $(DEPS) -ll
	mv a.out golitec

clean:
	rm lex.yy.c y.tab.c y.tab.h y.tab.h.gch


# |------------------------------------------|
# | CO2 6502, COMPILER OPTIMIZER TO 6502     |
# | File: Makefile                           |
# | v1.0, October 2012                       |
# | Author: Emilio Arango Delgado de Mendoza |
# |------------------------------------------|

CFLAGS   = -I BackEnd -I CommonFiles -I FrontEnd
CXXFLAGS = -std=c++0x -I BackEnd -I CommonFiles -I FrontEnd

OBJETCS  = CO2 lex.gc.o gc.tab.o main.o CommonFiles/errManager.o CommonFiles/symbolsTable.o CommonFiles/ocGen.o BackEnd/asmGen.o BackEnd/optimizer.o FrontEnd/semanticAnalyzer.o

SOURCES  = main.cpp lex.gc.c gc.tab.c CommonFiles/errManager.cpp CommonFiles/symbolsTable.cpp CommonFiles/ocGen.cpp BackEnd/asmGen.cpp BackEnd/optimizer.cpp FrontEnd/semanticAnalyzer.cpp

CLEAN_FS = BackEnd/*.o CommonFiles/*.o FrontEnd/*.o



# Main rule
CO2: $(OBJETCS)
	g++ -o $(OBJETCS)

# Lexer building rule
lex.gc.o: FrontEnd/gc.l lex.gc.c gc.tab.h
lex.gc.c: FrontEnd/gc.l
	flex -o lex.gc.c -Pgc -Cem FrontEnd/gc.l

# Parser building rule
gc.tab.o: gc.tab.c gc.tab.h
gc.tab.c: FrontEnd/gc.y
	bison -d -b gc -p gc FrontEnd/gc.y
gc.tab.h: FrontEnd/gc.y
	bison -d -b gc -p gc FrontEnd/gc.y



# Cleaning rules
clean:
	$(RM) *.o $(CLEAN_FS)

cleanall: clean
	$(RM) CO2 gc.tab.c gc.tab.h lex.gc.c



# Dependances creation rule
depend:
	makedepend $(CFLAGS) $(SOURCES)



# DO NOT DELETE

main.o: /usr/include/stdio.h /usr/include/features.h /usr/include/libio.h
main.o: /usr/include/_G_config.h /usr/include/wchar.h main.h
main.o: CommonFiles/ocGen.h CommonFiles/symbolsTable.h
main.o: FrontEnd/semanticAnalyzer.h BackEnd/beErrMsg.h FrontEnd/feErrMsg.h
lex.gc.o: /usr/include/stdio.h /usr/include/features.h /usr/include/libio.h
lex.gc.o: /usr/include/_G_config.h /usr/include/wchar.h /usr/include/string.h
lex.gc.o: /usr/include/xlocale.h /usr/include/errno.h /usr/include/stdlib.h
lex.gc.o: /usr/include/alloca.h main.h FrontEnd/feErrMsg.h
lex.gc.o: FrontEnd/semanticAnalyzer.h BackEnd/beErrMsg.h FrontEnd/feErrMsg.h
lex.gc.o: gc.tab.h
gc.tab.o: /usr/include/stdio.h /usr/include/features.h /usr/include/libio.h
gc.tab.o: /usr/include/_G_config.h /usr/include/wchar.h
gc.tab.o: CommonFiles/symbolsTable.h CommonFiles/errManager.h
gc.tab.o: FrontEnd/semanticAnalyzer.h BackEnd/beErrMsg.h FrontEnd/feErrMsg.h
CommonFiles/errManager.o: CommonFiles/errManager.h
CommonFiles/symbolsTable.o: CommonFiles/symbolsTable.h
CommonFiles/ocGen.o: CommonFiles/ocGen.h CommonFiles/symbolsTable.h
CommonFiles/ocGen.o: FrontEnd/semanticAnalyzer.h BackEnd/beErrMsg.h
CommonFiles/ocGen.o: FrontEnd/feErrMsg.h main.h
BackEnd/asmGen.o: BackEnd/asmGen.h CommonFiles/symbolsTable.h
BackEnd/asmGen.o: CommonFiles/errManager.h CommonFiles/ocGen.h
BackEnd/asmGen.o: CommonFiles/symbolsTable.h FrontEnd/semanticAnalyzer.h
BackEnd/asmGen.o: BackEnd/beErrMsg.h FrontEnd/feErrMsg.h main.h
BackEnd/optimizer.o: BackEnd/optimizer.h CommonFiles/symbolsTable.h
BackEnd/optimizer.o: CommonFiles/ocGen.h CommonFiles/symbolsTable.h
BackEnd/optimizer.o: FrontEnd/semanticAnalyzer.h BackEnd/beErrMsg.h
BackEnd/optimizer.o: FrontEnd/feErrMsg.h BackEnd/asmGen.h
BackEnd/optimizer.o: CommonFiles/errManager.h
FrontEnd/semanticAnalyzer.o: FrontEnd/semanticAnalyzer.h BackEnd/beErrMsg.h
FrontEnd/semanticAnalyzer.o: FrontEnd/feErrMsg.h CommonFiles/errManager.h

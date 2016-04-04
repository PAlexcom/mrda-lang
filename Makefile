build: lexer parser compile

demo: build test

lexer:
		alex Lexer.x

parser:
		happy Parser.y

clean:
		rm Lexer.hs Parser.hs Compiler

compile: 
		ghc -o Compiler Compiler.hs

test: 
		./Compiler tests/example-complete.sca
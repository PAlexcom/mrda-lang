build: lexer parser

demo: build test

lexer:
		alex Lexer.x

parser:
		happy Parser.y

clean:
		rm Lexer.hs Parser.hs

test: 
		runhaskell Compiler.hs tests/examples/example-complete.sca
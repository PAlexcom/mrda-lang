all: build

build: lexer parser


lexer: 
		alex Lexer.x

parser: 
		happy Parser.y

clean:
		rm Lexer.hs Parser.hs
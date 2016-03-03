all: build

build: lexer parser


lexer: 
		alex MRDALexer.x

parser: 
		happy MRDAParser.y

clean:
		rm MRDALexer.hs MRDAParser.hs
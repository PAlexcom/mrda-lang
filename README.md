Sca Language
=================
Project created during my Master's Degree in Computer Science for the _Languages and Compilers_ class, with Tommaso Vuaran, Andrea De Luca.

_(please do not judge the code quality, it was written in rush and hurry)_


-----------------


#### Project Goals
Project a suitable abstract syntax similar to [Scala](http://www.scala-lang.org/) language, and write in [Haskell](https://www.haskell.org/) a compiler for the new engineered language.

#### Sub-Goals
* Project an Abstract Syntax 
* Type-System
* Lexer (using [Alex](https://www.haskell.org/alex/))
* Parser (using [Happy](https://www.haskell.org/happy/)) 
* Type-Checker
* [Three-address code](https://en.wikipedia.org/wiki/Three-address_code) (intermediate code generation)

#### Usage
Type and run from you terminal `make demo`, it will automatically create a demo.
Or simply run `./Compiler filename.sca` to compile the source code from `filename.sca`, there is also a verbose debug mode: `./Compiler -d filename.sca`.


#### Sca language syntax
```scala
// Basic Declaration
var k: Boolean = ! true;
var w: Boolean = (1 <= 5) && (true || false); 
var c: Char = 'c';
var c: Int = -1;
var x: Int = 5 + 6;
var y: Int = 5 * 4;
var z: Int = 5 / 4;
var p: *Int = &c;
var kk: *Int = p;
var zza: Float = 5.0;
var zzzz: Int = zza;

// Compound Declaration
var s: Array[Array[Char](2)](2) = Array(Array('a', 'b'), Array('a', 'b'));
var i: Array[Array[Int](2)](2) = Array(Array(1+2,3), Array(5+3/7,1));
var a: Boolean = true;

def proc(valres c: Int, b: Boolean): Int = {
    return (c);
}

// Function Declaration
def func(a: Int, b: Boolean, d: Char, n: Float, l: Boolean, m: Array[Array[Char](2)](2)): Int = {
    // Basic Declaration
    var counter: Int = a;
    var z: Int = proc(counter, b);
    zza = 5;
    counter = a;
    counter += a;
    x = ++x + x;

    // Iteration Statement
    while (b == true) {
        // Function Call
        var z: Int = proc(counter, b);
        break;
        continue;
        proc(counter, b);
    }

    // Selection Statement
    if ((10 >= 8) || (counter > 5)) {
        var counter: Int = 2;
    } else {
        var counter: Int = 4;
    }

    if ((!true) || (counter > 5)) {
        var counter: Int = 2;
    }

    // Left Expression Declaration
    ++counter;
    --counter;

    // Primitive function call
    writeInt(a);

    x = readInt("anything");

    x = ++(++x) + ++x;
    x = ++x + x;
    y-- = x-- - ++x--;
    y-- = x;
    y++;

    s[1][2];
    d = s[1][1];
    s[1][0] = 'a';

    *p = 5;


    for (c <- 1 to 10){
        c = 5;
        if (x>5) {
            x++;
        }
        
        continue;
        break;
    }

    // Try Catch
    try {
        x=5;
    } catch {
        case ex: Exception => {

        }
    }

    // Jump Statement
    return (1+1);
}

def main(): Unit = {
    val z: Int = func(1, true, 'a', 3, false, s);
    return;
}

// Program execution
val run: Unit = main();
```


License
-------
Do what ever you want to.
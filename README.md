# Fortran implementation of lox language

This repository contains the implementation of the lox language following the book [Crafting Interpreters].
The implementation of the [lox language] does not always follow the book.

[Crafting Interpreters]: https://craftinginterpreters.com
[lox language]: https://craftinginterpreters.com/the-lox-language.html


## Building with fpm

[fpm]: https://fpm.fortran-lang.org

This project support the Fortran package manager ([fpm]) as build system.
Invoke fpm in the project root with

```
fpm build
```

To run the REPL use

```
fpm run
```

## Language features

This section describes the implemented language features of the [lox language] including deviations from the book.

The basic data types available in lox are numbers (IEEE754 binary64), strings (delimited by double quotes), booleans as well as a null value (`nil`).

```
❯ fpm run
1> 1;
=> number(1.0000000000000000)
2> 3.1415926;
=> number(3.1415926000000001)
3> "not a number";
=> string("not a number")
4> true;
=> boolean(true)
5> false;
=> boolean(false)
6> nil;
```

Unary operators for negation, minus (`-`) for numbers, bang (`!`) for booleans, are supported.
Negation on non-booleans implicitly casts the object to its boolean representation, everything except for `false` and `nil` are truthy.

```
❯ fpm run
1> -1.0;
=> number(-1.0000000000000000)
2> -false;
runtime error: Cannot negate 'boolean(false)'
  |
1 |  -false ;
  |  ^
  |
3> !1.0;
=> boolean(false)
4> !false;
=> boolean(true)
5> !true;
=> boolean(false)
6> !nil;
=> boolean(true)
```

Binary operations for equality comparison (`==`, `!=`), relation comparison of numbers (`>`, `>=`, `<`, `<=`), addition and substraction (`+`, `-`), and multiplication and division (`*`, `/`) are available.

```
❯ fpm run
1> 6 / 3 - 1;
=> number(1.0000000000000000)
2> 4 > 2 * 3 - 2;
=> boolean(false)
3> "this" == "self";
=> boolean(false)
4> true == false;
=> boolean(false)
5> true > false;
runtime error: Cannot compare 'boolean(true)' and 'boolean(false)'
  |
1 |  true > false;
  |       ^
  |
6> 1 - "one";
runtime error: Cannot subtract 'number(1.0000000000000000)' and 'string("one")'
  |
1 |  1 - "one";
  |    ^
  |
```

Variables are defined using the `var` keyword, the `print` keyword can be used to write to the terminal.
Note that assignment returns the assigned value and can therefore be chained.
Block constructs (curly braces) allow the definition of scope and shadowing of variables.
This lox implementation does not reserve keywords, instead the intent of an indentifier is defined from the semantics.
To avoid overwriting literals, like `true` and `false`, those are defined as constants.

```
❯ fpm run
1> var drink = "tea";
2> print drink;
tea
3> drink = "coffee";
=> string("coffee")
4> print drink;
coffee
5> var var = "no reserved keywords";
6> print var;
no reserved keywords
7> var true = "but constants";
runtime error: Cannot redefine frozen symbol 'true'
  |
1 |  var true = "but constants";
  |      ^^^^
  |
8> var a = 1;
9> {
..   var a = a + 2;
..   print a;
.. }
3.0000000000000000
10> print a;
1.0000000000000000
```

Conditional statements are available with `if`.
The available logical operators (`or`, `and`) support short-circuiting;

```
❯ fpm run
1> var a = 10;
AST: (root (var a 10))
2> if (a > 5) {
..   print "a is larger than five";
.. } else {
..   print a;
.. }
a is larger than five
3> false and a;
=> number(10.000000000000000)
```

Loop constructs are supported by `while` and `for` statements.

```
❯ fpm run
1> var a = 5;
2> while (a > 0) {
..   print a;
..   a = a - 1;
.. }
5.0000000000000000
4.0000000000000000
3.0000000000000000
2.0000000000000000
1.0000000000000000
3> for (var i = 0; i < 5; i = i + 1) print i;
0.0000000000000000
1.0000000000000000
2.0000000000000000
3.0000000000000000
4.0000000000000000
```


## License

Licensed under the Apache License, Version 2.0 (the “License”);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an *“as is” basis*,
*without warranties or conditions of any kind*, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in this project by you, as defined in the
Apache-2.0 license, shall be licensed as above, without any additional
terms or conditions.

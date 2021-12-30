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

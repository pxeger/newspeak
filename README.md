# Newspeak
In George Orwell's *Ninteen Eighty-Four*, **Newspeak** is a language with the goal of linguistically eliminating the
ability of its speakers to express "subversive" ideas, and thereby to supposedly eliminate such ideas completely.

Newspeak, the programming language, is analogous. The goal is to syntactically eliminate the possibility of programs
which don't work. By "don't work", I mean programs which, in conventional languages, would cause syntax errors or type
errors.

In Newspeak, all programs are, by definition, syntactically correct and type safe. Newspeak accomplishes this with a
special encoding. At each step of decoding, the compiler enumerates all the possible *valid* "next steps" the program
could take, and chooses one specified by the encoding. For more specific details, see [Syntax](#Syntax).

The ultimate goal of Newspeak is to be a competitive [Golfing language](https://en.wikipedia.org/wiki/Code_golf#Dedicated_golfing_languages).
As a result, it has many features similar to existing golfing languages, such as implicit input and output and a large
selection of highly composable builtin operations, and fewer features of practical languages which are typically less
useful in golfing languages, such as variables and comprehensive I/O facilities.

# Syntax
Newspeak is stack-based and statically-typed.

Decoding (which is really also parsing and compilation) works as follows:
1. Know the types of the values

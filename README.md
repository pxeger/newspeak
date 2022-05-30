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

1. The program is decoded as a [bijective base](https://en.wikipedia.org/wiki/Bijective_numeration) 256 integer. Call
   this integer `N`.

2. While N is not 0:

3. Know the types of the values currently on the stack. For the first step in decoding, these are the input types as
declared when invoking the compiler.

4. Enumerate all possible builtins which match the types on the stack. For example, if the stack's types are `[string,
   number]`, then the valid ones are builtins which take a single number, or builtins which take a string and a number.

5. `N-1` is divided by the number of possible builtins. The quotient becomes the new value of `N`

6. The remainder of this division is taken as an index into the list of possible builtins, and that builtin is compiled

7. The input types of the selected builtin are removed from the stack, and its output types are added

8. Go back to step 2

There are some special operations which deviate from this process slightly.

# Integer literal syntax
The encoding of integers is designed to reflect the typical distribution of integers in code golf: smaller integers are
used more frequently, especially `0` and `1`.

Further reading: <https://en.wikipedia.org/wiki/Universal_code_(data_compression)>

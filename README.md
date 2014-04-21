Tables
------
Tables is a command-line program for accessing Quill databases. It supports an English-like argument structure, typed Elements, truly whitespace independant parsing, and some advanced composite functions.

## Argument rundown
Tables [filename] get x from y
Tables [filename] get x
Tables [filename] add x as y
Tables [filename] add x to y as z
Tables [filename] remove x from y
Tables [filename] remove x
Tables [filename] change x in y to z
Tables [filename] map x to y as z
Tables [filename] combine x with y as z
Tables [filename] file x
Tables [filename] list
Tables [filename] x and y and...
Tables [filename] repl

## Table syntax
The actual flat file has a somewhat human readable syntax. Here is a file showing all possible use cases:

    table(tableName){`preserve whitespace`:val;key1:val1;}
	list(listName){a;b;c} /* I'm a comment! */

### Sales pitch
Tables is relatively safe (it won't delete its own file anymore, and probably won't burn your house down). It is built upon a rock-solid library (Cookbook) and a slightly-less rock-solid concept. The Haskell type system makes the base Quill library safe in regards to unexpected behavior, although there are some errors that Tables cannot recover from.

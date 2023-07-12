Tables
------
Tables is an executable for interacting with **Quill** database flat-files.

It is equivalent to `redis-cli` for Redis databases, allowing 1-off commands or a command shell.

## Argument rundown
* `Tables [filename] get x from y`
* `Tables [filename] get x`
* `Tables [filename] add x as y`
* `Tables [filename] add x to y as z`
* `Tables [filename] remove x from y`
* `Tables [filename] remove x`
* `Tables [filename] change x in y to z`
* `Tables [filename] map x to y as z`
* `Tables [filename] combine x with y as z`
* `Tables [filename] file x`
* `Tables [filename] list`
* `Tables [filename] x and y and...`
* `Tables [filename] repl`

## Table syntax
The actual flat file has a somewhat human readable syntax. Here is a file showing all possible use cases:

    table(tableName){`preserve whitespace`:val;key1:val1;}
	list(listName){a;b;c} /* I'm a comment! */

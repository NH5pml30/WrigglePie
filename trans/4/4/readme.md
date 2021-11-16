# LALR(1) Parser Generator

Supports LALR(1) grammars with synthesized attributes. The input file grammar is expressed in itself in `self.nh5` file.

Note that all whitespaces are removed from the preamble and attributes expressions.

To escape unmatched curly braces inside attribute expressions, use `\`. To escape `/` inside lexer regexes use `\`.

Also note that the generated parser table (`parser_table.dat`) must always be on the current path of the executable, otherwise you will get `1:1: Parse error`.

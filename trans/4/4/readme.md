# LALR(1) Parser Generator

Supports LALR(1) grammars with synthesized attributes. The input file grammar is expressed in itself in `self.nh5` file.

All whitespaces are left as-is inside the attributes and the preamble through raw input caching, controlled with `!pop_cache`/`!push_cache` directives, executed when the associated rule is reduced. You can access the cache through `std::string $0` variable inside the attribute.
Note that caching includes only the next token after `!push_cache` (because of the lookahead) and the previous token before `!pop_cache`.

To match any terminal except some inside a non-terminal rule, use `~( some_terminal some_terminal1 ... )` or `~some_terminal`.

To escape any unmatched curly braces inside attribute expressions, use `\`. To escape `/` inside lexer regexes use `\`.

Also note that the generated parser table (`parser_table.dat`) must always be on the specified to the generator realtive path, starting from the executable.

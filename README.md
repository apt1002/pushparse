# Push Parser library

`pushparse` library for writing push parsers.

A push parser is one where the input stream is the caller and the output stream is the callee. For example, the caller might be a loop that reads a line of input and feeds it byte by byte into the parser, and the callee might be a handler that executes the code that the parser produces.

For parsing entire files, a push parser has no great advantages over a more conventional pull parser. However, it does have atvantages when parsing streams. Examples of streams include interactive sessions, communication protocols, real time data, and very large files.

# Push Parser library

I have abandoned this project in favour of [welly-parser]. I think it could still be completed, but it is unnecessarily difficult in Rust's type system. Maybe the idea will be useful to somebody one day.

[welly-parser]: https://github.com/apt1002/welly-parser

# Previous README

`pushparse` library for writing push parsers.

A push parser is one where the input stream is the caller and the output stream is the callee. For example, the caller might be a loop that reads a line of input and feeds it byte by byte into the parser, and the callee might be a handler that executes the code that the parser produces.

For parsing entire files, a push parser has no great advantages over a more conventional pull parser. However, it does have atvantages when parsing streams. Examples of streams include interactive sessions, communication protocols, real time data, and very large files.

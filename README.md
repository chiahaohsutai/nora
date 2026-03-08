# NORA - Writing a C Compiler

Nora is a personal project focused on building a C compiler for a subset of the C programming language. The implementation follows the structure presented in Nora Sandler’s book *Writing a C Compiler*, which also inspired the name of the project. I initially implemented many of the compiler components by closely following the structure and recommendations described in the book.

As I continued learning from additional resources on functional programming patterns and finite state automata, I decided to revisit and redesign parts of the compiler. This led me to restart portions of the project with the goal of intentionally applying these theoretical ideas to the design and implementation. The project therefore serves as an exploration of how concepts from theory can influence practical software design.

Because of this shift in direction, the commit history is not perfectly clean. Several components were rewritten as I experimented with different approaches and design choices. I also wanted to lean more into Rust specific patterns. My first implementation relied heavily on simple functions, which works, but does not fully take advantage of the abstractions and design idioms that Rust encourages. As a result, this repository mostly serves as a set of personal notes and experiments while studying compiler construction, Rust design patterns, and general software engineering ideas.

If someone happens to come across the project and finds it useful or interesting, that is a welcome bonus.

As always, happy coding!

## References

- *Writing a C Compiler* — Nora Sandler  
- *Functional Programming in Scala* — Paul Chiusano and Rúnar Bjarnason  
- *Modeling Software with Finite State Machines* — Ferdinand Wagner, Ruedi Schmuki, and Thomas Wagner

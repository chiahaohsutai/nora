# NORA — Writing a C Compiler

Nora is a personal project focused on building a C compiler for a subset of the C programming language. The implementation is largely guided by Nora Sandler’s book *Writing a C Compiler*, which also inspired the name of the project. In the early stages, I followed the structure and recommendations from the book closely while implementing many of the core compiler components. As the project evolved, however, I decided to rewrite significant portions of the implementation after exploring additional ideas and design approaches.

While working through the compiler, I was also reading material on functional programming patterns and finite state machines. These topics influenced how I thought about the architecture of the project and motivated me to revisit parts of the implementation. As a result, I restarted several sections of the compiler with the goal of intentionally applying some of these theoretical ideas to the design and code.

Another reason for the rewrite was to better embrace Rust specific design patterns. My initial implementation relied heavily on simple functions, which works, but does not fully take advantage of the abstractions and idioms that Rust encourages. Reworking parts of the project gave me the opportunity to explore those patterns more deliberately. As a result, this repository serves largely as a collection of notes, experiments, and design explorations while I continue learning about compiler construction, Rust patterns, and software design in general.

If someone happens to come across this project and finds it useful or interesting, that is a welcome bonus.

As always, happy coding!

## References

- *Writing a C Compiler* — Nora Sandler  
- *Functional Programming in Scala* — Paul Chiusano and Rúnar Bjarnason  
- *Modeling Software with Finite State Machines* — Ferdinand Wagner, Ruedi Schmuki, and Thomas Wagner
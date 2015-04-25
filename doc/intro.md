# Introduction to mapfs

mapfs is a command-line shell that uses a Clojure map as a filesystem.  The program functions in behavior somewhere between a shell and a REPL, allowing the user to write small clojure programs for personal computing projects. 

## Design

[Notes for what to discuss are below.]

* feels like shell, acts like a REPL
* unlike unix, not just binary data is being piped between commands
* discuss processes, signals (and implement as threads tracked by a process table...)
* all data, functions, code, etc. are just standard Clojure code 


## .mapfsrc

If a file called .mapfsrc is located in the user's home directory, this file will be read in at the start of mapfs. Like ~/.profile is used with bash, a user may want to add function and variable definitions here to customize their global mapfs experience.  




# Hakshell
This is my attempt at creating a set of functions that can be imported
into GHCi to turn GHCi into a shell that is nearly as useful as using
Bash (sometimes called the UNIX/Linux command line).

It is inspired by Turtle Shell, but with a different goal. The goal of
Turtle Shell is to make it easier to write Bash-like shell scripts in
Haskell. The goal of Hakshell is to actually be used as a Bash-like
shell within a Read-Eval-Print Loop (REPL), and it does this by
extending an ordinary GHCi session with shell-like capabilities,
namely launching foreground and background processes and process
pipelines.

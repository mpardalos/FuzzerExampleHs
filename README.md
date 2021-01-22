# Example for Software Reliability coursework

This example simply generates random cnf files in a directory. To run:

``` sh
stack run -- fuzz-sat <output-dir>
```
This will generate 10 test cases in <output-dir>

It will take a while the first time as it is downloading the Haskell binary and compiling libraries

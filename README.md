# asciilines

Copyright 2019 Liam Wynn

Application that accepts a single .tvg file and renders that on standard output.

# How do I run asciilines?
## Compiling asciilines
First, you must have the Glasgow Haskell Compiler installed. Once that is installed, you
should be able run the following commands:

```
make all
```

## Running asciilines
To run asciilines, navigate to the `bin/` folder and run `./asciilines FILE_NAME.tvg`. There
are several files in the `tests/` folder which have .tvg files for you to use.

### Running tests
There are several tests one can run. To do so, from the root of the project, run the command:
`./bin/tests/path/to/test/run.out`. Provided the test is functional, it will print out whether
the test works or not. If it failed, it will tell you that either the program is not working
as intended, or the test script itself failed.


# Issue Summary
- The application will fail if the user supplies a file or several. Should the user supply
no files, the application will work as intended as it prints a message saying how to run.
- Running the command `make run` does not work.

# License
This program is licensed uder the "MIT License". Please see the `LICENSE` in the source distribution
of this software for license terms.

# GoLite compiler

## Team Members
* [bobrinik](https://github.com/Bobrinik)
* [takanarisasaki](https://github.com/takanarisasaki)
* [mwood](https://github.com/mwoodb)

## About
- This project has been built for [comp 520](https://www.cs.mcgill.ca/~cs520/2019/).
  - Relevant material can be found on the course webpage
- **GoLite specification**
  - [Feature overview](doc/golite.pdf)
  - [Syntactic specification](doc/spec1.pdf)
  - [Semantic specification](doc/spec2.pdf)
- As part of this project. GoLite can be translated into JS.

## Requirements
- We are using flex and bison to build scanner and parser.
  - Annotated flex and bison [example](https://github.com/comp520/Examples/tree/master/flex%2Bbison/scanner%2Bparser) by [akrolik](https://github.com/akrolik)
  - Excellent book on flex is `flex & bison by John Levine`
- We are using `make` to build our project.

```shell
sudo apt install make flex bison
```

## Building
```bash
cd src
make install
```

## Running

```shell
# Run tests located in Programs directory
./test.sh 
```

```shell
# Run scanner on go string
printf "package main\n func main(){ println(\"Hello\")\n }" | ./golite scan

# Scan with the file
./golite scan < my_golite.go

# Parse
./golite parse < my_golite.go # outputs OK if the program is syntactically correct

# Symbol
./golite symbol < my_golite.go # outputs symbol table that is being built for the program

# Typecheck
./golite typecheck < my_golite.go # outputs ok if the program typechecks

# Codegen
./golite codegen generated_file_name < my_golite_to_tranlsate_from.go
```
## Development

- I found that Emacs has the best support for writing C code. I wish I knew about this back in the time when I took operating systems.
  - [Excellent tutorial on setting up Emacs for C](https://tuhdo.github.io/c-ide.html)
- [`valgrind`](http://valgrind.org/docs/manual/quick-start.html) very useful when debugging segfault errors. Just follow installation instructions from their website.
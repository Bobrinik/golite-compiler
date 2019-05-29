## [Project](https://github.com/Bobrinik/golite-compiler)

- GoLite compiler is able to compile a subset of Go into Javascript.

## Project Difficulties

- Organising C project to increase code reuse and modularity.

## Solution

- Each compiler related task has been split into a different file. Each of the files would contain **AST** traversal code and node specific logic. Notice that this solution results in a duplication of **AST** traversal code over the tasks.
- It's possible to abstract **AST** traversal into a separate module and then passes specific logic as a callback function. I didn't go with this pattern because it would decrease code readability and ease of debugging and the benefit would be  only a reduction in code size. 

## Technology used

- C
- flex&bison
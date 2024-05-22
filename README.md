# MIPS Compiler Dev Notes

## Lexer
- Break up text into tokens for each line (denoted by semicolons)
- A line may either contain an assignment expression, function call or language construct (while, for, if)
- Tokens may be delimited by encountering specific tokens (operators, spaces, semicolons, newlines)

## Parser
- Process the stream of tokens into a list of statement trees. Statements may be any of the following:
    - Expression
    - Language construct - (Function declaration, while, for, if)
- Generate abstract syntax trees for each expression encountered.
### Parser ToDo
- [X] Add pretty printing for statement tree for debugging
- [X] Support for variable declartion/initialisation in statement tree
- [X] Support for function declaration in statement tree
- [X] Support for while loop in statement tree
- [X] Support for for loop in statement tree
- [X] Support for if statement in statement tree

## Translator
1. Generate an environment of globals and function declarations
2. Start translation from main()
3. If a function call is encountered - generate its associated declaration if it doesn't already exist

#### Expression Translation:  
Recursively parse expression until we reach a binary/unary operation we can translate - store immediate intermediary results in the target register. If we encounter a grouping intermediate results must be stored in the tmp register. Every grouping will require it's own intermediate register. Since we only have 16 registers to work with this highlights the importance of implementing compile time evaluation.

**Literal Example**  
**LunaC:**
```c
int x = (1 + 1) + ((2 + 3) / (1 + 3));
```
**MIPS**
```
add r0 1 1
add r1 2 3
add r2 1 3
div r1 r1 r2
add r0 r0 r1
```
Additionally, once we exceed 15 registers used, we must switch to stack variables which require 2 operations per read/write.

#### Stack Variables:  
A given scope's environment will contain a mapping of variable to stack address.  

**Environment:**
```
{
  x => 1
  y => 2
  z => 3
}
```
**C:**
```c
{
  int x = 0;
  int y = 1;
  int z = 2;
  y = y + 1;
}
```

**MIPS:**
```
move r0 0;
push r0;

move r0 1;
push r0;

move r0 2;
push r0;

move sp 2;
peek r0;
add r0 r0 2;
```
In the interest of reducing assembly file size we must minimize stack usage i.e. variables not accessed after a function call may be discarded. Additionally, only push to the stack if the number of register mappings exceeds 15 (1 tmp register is required for stack ops).
- Assembly generation for functions:
  - locals and parameters are stored in registers
  - If a function call is encountered:
    - A tmp return register is allocated 
    - Locals and parameters are pushed onto the stack - We must maintain a mapping in order to load parameters after return
    - Function parameters and locals are loaded into registers

## Long Term
- [ ] Ternary if statements
- [ ] Assignment by operation (e.g. +=)
- [ ] Structs
- [ ] Pointers
- [ ] Arrays
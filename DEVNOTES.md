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

#### Memory Layout:
- 16x general purpose registers (64 bit)
- 1x stack pointer register
- 1x return address register

Since we don't have access to an addressable memory space we must treat the stack as 
  
| Register # | Usage |
|------------|-------|
| 0 | Return values are stored in this register. |
| 1 | Frame pointer register. |
| 2..15 | Scratch registers. |
#### Expression Translation:  
Recursively parse expression until we reach a binary/unary operation we can translate - store immediate intermediary results in the target register. If we encounter a grouping intermediate results must be stored in the tmp register.

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
**Variable Example**  
**LunaC:**
```c
int a = 5;
int x = (1 + a) + ((2 + a) / (a + 3));
```
**MIPS**
```
move r0 5
add r1 1 r0
add r2 2 r0
add r3 r0 3
div r2 r2 r3
add r1 r1 r2
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
#### If Statements
In order to translate an if statement we must first compute the condition expression and store the result in r6 and insert a branch if equal instruction, we can then translate the body of each branch adding labels where necessary. Since we don't have a cmp operator in MIPS we must define a regime for translating logical operations which include comparisons. MIPS does have a set of comparison-and-set operators

## Optimisers
- [ ] Conditional branch reduction - The translation of logical statements utilises 's**' operations (e.g. sgt) meaning a the result is stored in a specified register, in the case of conditional statements this operation is always followed by a 'beq' or 'bne' operation these can be reduced to just 'bgt'
## Long Term
- [ ] Ternary if statements
- [X] Assignment by operation (e.g. +=)
- [ ] Structs
- [X] Pointers
- [ ] Arrays
- [ ] 
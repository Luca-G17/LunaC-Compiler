# Stationeers IC10-C Compiler

This project provides a compiler capable of translating a subset of [c89](https://en.wikipedia.org/wiki/ANSI_C) to [Stationeers IC10](https://stationeers-wiki.com/IC10) assembly code (please note that this is still WIP and is certain to have bugs)

## Usage
1. Download the relavent binary for your operating system (c_to_mips for Linux, c_to_mips.exe for Windows).
2. Make the file an executable (linux): `chmod +x ./lunaC`
3. Compile a `.c` program: `./lunaC <program>.c`
4. The IC10 mips file will be built with the name `./<program>`

### IDE Static Analysis
In order to allow programmers to interface with devices/call some `math.h` functions the compiler performs a preprocessing step identifying function calls defined in the [IC10 API](#ic10-api)

## IC10 API

## Unsupported Features

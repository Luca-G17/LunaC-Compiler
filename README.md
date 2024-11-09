# Stationeers IC10-C Compiler

This project provides a compiler capable of translating a subset of [c89](https://en.wikipedia.org/wiki/ANSI_C) to [Stationeers IC10](https://stationeers-wiki.com/IC10) assembly code (please note that this is still WIP and is certain to have bugs)

## Usage
1. Download the relavent binary for your operating system (c_to_mips for Linux, c_to_mips.exe for Windows).
2. Make the file an executable (linux): `chmod +x ./lunaC`
3. Compile a `.c` program: `./lunaC <program>.c`
4. The IC10 mips file will be built with the name `./<program>`

### IDE Static Analysis
LunaC is a subset of c89 and therefore LunaC programs are valid C programs, this allows LunaC programmers to use their favourite static analysis tool in whichever IDE or text-editor to provide syntax highlighting and code completion.

In order to allow programmers to interface with devices/call some `math.h` functions, the compiler performs a preprocessing step identifying function calls defined in the [IC10 API](#ic10-api) and replaces them with the relavent IC10 instructions. However IDE static analysis tools will report an error as they cannot locate a definition for these functions. For this reason I have written a c library which can be included in programs `#include "mips.h"` allowing an IDE to use a standard c compiler (such as gcc or clang) to perform code completion and static analysis.  

The c library can be found in `c_libs`.

## IC10 API
The following functions are special functions for which the compiler will directly replace calls with their corresponding assembly instruction. These are defined within the compiler and therefore there is no requirement to download or include the `mips.h` header if you don't want to.

TODO: Properly document these functions:
```
void load(float* dest, device_id_t device, device_variable_t device_var) {}
void load_reagent(float* dest, device_id_t device, reagent_mode_t reagent_mode, reagent_t reagent) {}
void load_slot(float* dest, device_id_t device, int slot_index, slot_variable_t slot_var) {}
void load_batch(float* dest, entity_t device_type, device_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_slot(float* dest, entity_t device_type, int slot_index, slot_variable_t slot_var, batch_mode_t batch_mode) {}
void load_batch_with_name(float* dest, entity_t device_type, char* device_name, device_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_with_name_slot(float* dest, entity_t device_type, char* device_name, int slot_index, slot_variable_t slot_var, batch_mode_t batch_mode) {}
void store(device_id_t device, device_variable_t device_var, float source) {}
void store_slot(device_id_t device, int slot_index, slot_variable_t slot_var, float source) {}
void store_batch(entity_t device_type, device_variable_t device_var, float source) {}
void store_batch_with_name(entity_t device_type, char* device_name, device_variable_t device_var, float source) {}
void store_batch_slot(entity_t device_type, int slot_index, slot_variable_t slot_var, float source) {}
float m_sin(float __x);
float m_cos(float __x);
float m_tan(float __x);
float m_asin(float __x);
float m_acos(float __x);
float m_atan(float __x);
float m_atan2(float __x, float __y);
float m_abs(float __x);
float m_ceil(float __x);
float m_floor(float __x);
float m_trunc(float __x);
float m_round(float __x);
float m_min(float __x, float __y);
float m_max(float __x, float __y);
float m_exp(float __x);
float m_log(float __x);
float m_sqrt(float __x);
float m_rand(float __x);
void m_sleep(float __x);
void m_yeild();
```

## Unsupported Features

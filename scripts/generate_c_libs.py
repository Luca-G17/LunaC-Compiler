import json

IDENTIFIERS_FILE="scripts/mips_identifiers.json"
HEADER_FILE="c_libs/mips_types.h"
FUNCTIONS_FILE="c_libs/mips.h"
ITEMS_FILE="c_libs/mips_items.h"

ARG_TYPE_TO_STR={
    "device_id_t": "device",
    "device_variable_t": "device_var",
    "reagent_mode_t": "reagent_mode",
    "slot_variable_t": "slot_var",
    "entity_t": "device_type",
    "batch_mode_t": "batch_mode",
    "float*": "__x_ptr",
    "int*": "__x_ptr",
    "int": "__x",
    "float": "__x"
}

def load_game_identifiers():
    with open(IDENTIFIERS_FILE, "r") as identifiers_json:
        idens = json.loads(identifiers_json.read())
    return idens

def write_types_header(idens, header_file_path, type):
    with open(header_file_path, "w") as header_file:
        for catagory in idens[type]:
            identifier = catagory['identifier']
            header_file.write("typedef enum {\n")
            for variant in catagory['variants']:
                header_file.write(f"    {variant},\n")
            header_file.write(f"}} {identifier};\n")
            header_file.write("\n")

def get_arg_name(arg):
    if arg[1] != "":
        return arg[1]
    return ARG_TYPE_TO_STR[arg[0]] 


def write_mips_functions(idens):
    with open(FUNCTIONS_FILE, "w") as functions_file:
        functions_file.write("#include \"mips_types.h\"\n")
        functions_file.write("#include \"mips_items.h\"\n")
        functions_file.write("\n")
        for function in idens["special_functions"]:
            func_name = function['identifier']
            ret_type = function['ret']
            args = function['args']
            arg_str = ""
            for i, a in enumerate(args):
                arg_str += f"{a[0]} {get_arg_name(a)}"
                if i != len(args) - 1:
                    arg_str += ", "
            if function['implement']:
                end_str = ";"
            else:
                end_str = " {}"
            functions_file.write(f"{ret_type} {func_name}({arg_str}){end_str}\n")

idens = load_game_identifiers()
write_mips_functions(idens)
write_types_header(idens, ITEMS_FILE, "item_types")
write_types_header(idens, HEADER_FILE, "type_catagories")
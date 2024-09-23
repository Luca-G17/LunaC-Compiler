
use std::collections::HashMap;
use lazy_static::lazy_static;

const SPECIAL_IDENTIFIERS_STR: &str = include_str!("../../scripts/mips_identifiers.json");
const CRC_HASHER: crc::Crc<u32> = crc::Crc::<u32>::new(&crc::CRC_32_ISO_HDLC);

pub struct SpecialFunction {
    pub(super) args: Vec<String>,
    pub(super) mips_name: String,
    pub(super) storing: bool,
}

lazy_static! {
    pub(super) static ref SPECIAL_FUNCTIONS: HashMap<String, SpecialFunction> = {
        let mut m = HashMap::new();
        let functions_json = json::parse(SPECIAL_IDENTIFIERS_STR).unwrap();
        if let json::JsonValue::Array(function_arr) = &functions_json["special_functions"] {
            for function in function_arr {
                let identifier = match &function["identifier"].as_str() {
                    Some(iden) => iden.to_string(),
                    None => "".to_string()
                };

                let mips_name = match &function["mips_name"].as_str() {
                    Some(name) => name.to_string(),
                    None => "".to_string()
                };
                let mut args = vec![];
                if let json::JsonValue::Array(args_arr) = &function["args"] {
                    for arg in args_arr {
                        if let json::JsonValue::Array(arg_type_arr) = arg {
                            if let Some(type_arg) = &arg_type_arr[0].as_str() {
                                args.push(type_arg.to_string());
                            }
                        }
                    }
                }
                let storing = match &function["storing"] {
                    json::JsonValue::Boolean(storing_bool) => *storing_bool,
                    _ => false
                };
                
                m.insert(identifier, SpecialFunction { args, mips_name, storing });
            }
        }
        m
    };
}

fn read_type_constants(grouping: &str) ->  HashMap<String, Vec<String>> {
    let game_types = json::parse(SPECIAL_IDENTIFIERS_STR).unwrap();
    let mut enums = HashMap::new();

    // TODO: This is a mess but kinda fine cause its only used once - if I need to json parse again I may write a template parser.
    if let json::JsonValue::Array(catagories) = &game_types[grouping] {
        for catagory in catagories {
            if let Some(identifier) = &catagory["identifier"].as_str() {
                let mut variants = vec![];
                if let json::JsonValue::Array(json_variants) = &catagory["variants"] {
                    for variant in json_variants {
                        if let Some(variant_str) = variant.as_str() {
                            variants.push(variant_str.to_string());
                        }
                    }
                }
                enums.insert(identifier.to_string(), variants);
            }
        }
    }
    enums
}

fn to_pascal_case(str: &str) -> String {
    let replacement_caps = str;
    // everywhere with the pattern '_[alpha]' needs to be replaced with '_[Upper(alpha)]'
    let mut shifted = replacement_caps[1..replacement_caps.len()].to_string();
    shifted.push('-');

    let mut chars = vec![];
    let mut skip_char = false;
    for (i, (c1, c2)) in replacement_caps.chars().zip(shifted.chars()).enumerate() {
        if !skip_char {
            if c1 == '_' && (c2.is_ascii_lowercase() || c2.is_ascii_uppercase()) {
                chars.push(c2.to_ascii_uppercase());
                skip_char = true;
            } else if i == 0 {
                chars.push(c1.to_ascii_uppercase());
            } else {
                chars.push(c1.to_ascii_lowercase());
            }
        }
        else {
            skip_char = false;
        }
    }
    chars.iter().collect()
}

lazy_static! {
    pub(super) static ref SPECIAL_CONSTANTS: SpecialConstants = SpecialConstants { mips_types: read_type_constants("type_catagories"), mips_items: read_type_constants("item_types") };
}

pub struct SpecialConstants {
    pub(super) mips_types: HashMap<String, Vec<String>>,
    pub(super) mips_items: HashMap<String, Vec<String>>
}

impl SpecialConstants {

    pub(super) fn is_mips_type_variant(str: &str) -> bool {
        for c_enum in SPECIAL_CONSTANTS.mips_types.values() {
            for item in c_enum {
                if item == str {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn is_mips_item_variant(str: &str) -> bool {
        for c_enum in SPECIAL_CONSTANTS.mips_items.values() {
            for item in c_enum {
                if item == str {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn constant_at_index(index: usize, const_type: &str) -> Option<String> {
        if let Some(c_enum) = SPECIAL_CONSTANTS.mips_items.get(const_type) {
            return c_enum.get(index).cloned();
        }
        else if let Some(c_enum) = SPECIAL_CONSTANTS.mips_types.get(const_type) {
            return c_enum.get(index).cloned();
        }
        None
    }

    pub(super) fn index_at_constant_without_type(constant: &str) -> Option<usize> {
        for key in  SPECIAL_CONSTANTS.mips_items.keys().chain(SPECIAL_CONSTANTS.mips_types.keys()) {
            if let Some(i) = SpecialConstants::index_at_constant(constant, key) {
                return Some(i);
            }
        }
        None
    }

    pub(super) fn index_at_constant(constant: &str, const_type: &str) -> Option<usize> {
        if let Some(c_enum) = SPECIAL_CONSTANTS.mips_items.get(const_type) {
            for (i, item) in c_enum.iter().enumerate() {
                if item == constant {
                    return Some(i);
                }
            }
        }
        else if let Some(c_enum) = SPECIAL_CONSTANTS.mips_types.get(const_type) {
            for (i, item) in c_enum.iter().enumerate() {
                if item == constant {
                    return Some(i);
                }
            }        
        }
        None
    }

    pub(super) fn is_item_type(i_type: &str) -> bool {
        SPECIAL_CONSTANTS.mips_items.contains_key(i_type)
    }

    pub(super) fn is_mips_type(m_type: &str) -> bool {
        SPECIAL_CONSTANTS.mips_types.contains_key(m_type)
    }

    pub(super) fn hash_replacement(replacee: &str, item_type: &str) -> Option<i32> {
        if let Some(c_enum) = SPECIAL_CONSTANTS.mips_items.get(item_type) {
            for variant in c_enum {
                if variant == replacee {
                    let as_pascal = to_pascal_case(replacee);
                    println!("{}", as_pascal);
                    return Some(CRC_HASHER.checksum(as_pascal.as_bytes()) as i32);
                }
            }
        }
        None
    }

    pub(super) fn replacement_string(variant: &str, enum_identifier: &str) -> Option<String> {
        if let Some(c_enum) = SPECIAL_CONSTANTS.mips_types.get(enum_identifier) {
            for element in c_enum {
                if element == variant {
                    return Some(to_pascal_case(element));
                }
            }
        }
        None
    }
}
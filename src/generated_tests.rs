#[allow(unused_imports)]
use crate::tests::test_translation;

#[cfg(test)]
pub mod translator_tests {
   use super::*;
    #[test]
    pub fn add_auto() {
        test_translation(String::from("add_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn minus_auto() {
        test_translation(String::from("minus_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn times_auto() {
        test_translation(String::from("times_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn divide_auto() {
        test_translation(String::from("divide_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn or_auto() {
        test_translation(String::from("or_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn and_auto() {
        test_translation(String::from("and_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn xor_auto() {
        test_translation(String::from("xor_auto"), String::from("auto_generated_tests"), 1);
    }
    #[test]
    pub fn mod_auto() {
        test_translation(String::from("mod_auto"), String::from("auto_generated_tests"), 1);
    }
}

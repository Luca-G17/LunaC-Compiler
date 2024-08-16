#[allow(unused_imports)]
use crate::tests::test_translation;

#[cfg(test)]
pub mod translator_tests {
   use super::*;
    const TEST_DIRECTORY: &str = "auto_generated_tests";
    #[test]
    pub fn add_auto() {
        test_translation("add_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn minus_auto() {
        test_translation("minus_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn times_auto() {
        test_translation("times_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn divide_auto() {
        test_translation("divide_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn or_auto() {
        test_translation("or_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn and_auto() {
        test_translation("and_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn xor_auto() {
        test_translation("xor_auto", TEST_DIRECTORY, 1);
    }
    #[test]
    pub fn mod_auto() {
        test_translation("mod_auto", TEST_DIRECTORY, 1);
    }
}

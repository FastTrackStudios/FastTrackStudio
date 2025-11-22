#[cfg(test)]
mod tests {
    use crate::parsing::key::KeyParser;

    #[test]
    fn test_parse_major_key() {
        let mut parser = KeyParser::new();
        
        let result = parser.parse("C Major");
        match result {
            Ok(key) => {
                println!("Parsed key: {:?}", key);
                // TODO: Implement Display for Key to enable proper string comparison
            }
            Err(errors) => {
                println!("Parse errors: {:?}", errors);
                panic!("Failed to parse major key");
            }
        }
    }

    #[test]
    fn test_parse_minor_key() {
        let mut parser = KeyParser::new();
        
        let result = parser.parse("A Minor");
        match result {
            Ok(key) => {
                println!("Parsed key: {:?}", key);
                // TODO: Implement Display for Key to enable proper string comparison
            }
            Err(errors) => {
                println!("Parse errors: {:?}", errors);
                panic!("Failed to parse minor key");
            }
        }
    }

    #[test]
    fn test_parse_dorian_key() {
        let mut parser = KeyParser::new();
        
        let result = parser.parse("D Dorian");
        match result {
            Ok(key) => {
                println!("Parsed key: {:?}", key);
                // TODO: Implement Display for Key to enable proper string comparison
            }
            Err(errors) => {
                println!("Parse errors: {:?}", errors);
                panic!("Failed to parse dorian key");
            }
        }
    }

    #[test]
    fn test_parse_custom_scale() {
        let mut parser = KeyParser::new();
        
        let result = parser.parse("C [0,2,4,5,7,9,11]");
        match result {
            Ok(key) => {
                println!("Parsed key: {:?}", key);
                // TODO: Implement Display for Key to enable proper string comparison
            }
            Err(errors) => {
                println!("Parse errors: {:?}", errors);
                panic!("Failed to parse custom scale");
            }
        }
    }

    #[test]
    fn test_key_parsing_errors() {
        let mut parser = KeyParser::new();
        
        // Test invalid note name
        let result = parser.parse("X Major");
        match result {
            Ok(_) => panic!("Should have failed to parse invalid note"),
            Err(errors) => {
                println!("Errors for 'X Major': {:?}", errors);
                assert!(!errors.errors.is_empty());
            }
        }
        
        // Test missing scale type
        let result = parser.parse("C");
        match result {
            Ok(_) => panic!("Should have failed to parse missing scale"),
            Err(errors) => {
                println!("Errors for 'C': {:?}", errors);
                assert!(!errors.errors.is_empty());
            }
        }
        
        // Test invalid scale name
        let result = parser.parse("C InvalidScale");
        match result {
            Ok(_) => panic!("Should have failed to parse invalid scale"),
            Err(errors) => {
                println!("Errors for 'C InvalidScale': {:?}", errors);
                assert!(!errors.errors.is_empty());
            }
        }
    }
}

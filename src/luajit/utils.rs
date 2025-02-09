pub mod uleb128 {
    use std::collections::VecDeque;

    pub fn read_usize(vec: &mut VecDeque<u8>) -> usize {
        let mut result = 0;
        let mut shift = 0;

        loop {
            let byte = vec.pop_front().unwrap();
            let low_bits = (byte & 0x7F) as usize;
            result |= low_bits << shift;

            if byte & 0x80 == 0 {
                return result;
            }

            shift += 7;
        }
    }

    pub fn read_u32(vec: &mut VecDeque<u8>) -> u32 {
        let mut result = 0;
        let mut shift = 0;

        loop {
            let byte = vec.pop_front().unwrap();
            let low_bits = (byte & 0x7F) as u32;
            result |= low_bits << shift;

            if byte & 0x80 == 0 {
                return result;
            }

            shift += 7;
        }
    }

    pub fn write_usize(vec: &mut Vec<u8>, mut val: usize) {
        loop {
            let mut byte = (val & 0x7F) as u8;
            val >>= 7;
            if val != 0 {
                // More bytes to come, so set the continuation bit.
                byte |= 0x80;
            }
            vec.push(byte);
            if val == 0 {
                break;
            }
        }
    }

    pub fn write_u32(vec: &mut Vec<u8>, mut val: u32) {
        loop {
            let mut byte = (val & 0x7F) as u8;
            val >>= 7;
            if val != 0 {
                // More bytes to come, so set the continuation bit.
                byte |= 0x80;
            }
            vec.push(byte);
            if val == 0 {
                break;
            }
        }
    }
}

pub mod unescape {
    use std::borrow::Cow;

    fn to_digit_hex(c: char) -> u8 {
        match c {
            '0'..='9' => c as u8 & 0b1111,
            'a'..='f' => c as u8 - b'a' + 10,
            'A'..='F' => c as u8 - b'A' + 10,
            _ => panic!("bad hex char '{c}'"),
        }
    }

    pub fn unescape(string: &str) -> Cow<'_, str> {
        if !string.as_bytes().contains(&b'\\') {
            return Cow::Borrowed(string);
        }

        let mut result = String::with_capacity(string.len());

        let mut chars = string.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\\' {
                let c = chars.next().unwrap();
                let v = match c {
                    'a' => '\x07',
                    'b' => '\x08',
                    'f' => '\x0C',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'v' => '\x0B',
                    'x' => {
                        let hi = chars.next().unwrap();
                        let hi = hi.to_digit(16).unwrap();

                        let lo = chars.next().unwrap();
                        let lo = lo.to_digit(16).unwrap();

                        let value = ((hi << 4) | lo) as u8;

                        value as char
                    }
                    'u' => {
                        assert_eq!(chars.next(), Some('{'));

                        let mut c = chars.next().unwrap();
                        let mut value = 0;

                        loop {
                            value = (value << 4) | (to_digit_hex(c) as u32);

                            if value >= 0x110000 {
                                panic!("bad unicode escape")
                            }

                            c = chars.next().unwrap();
                            if c == '}' {
                                break;
                            }
                        }

                        char::from_u32(value).unwrap()
                    }
                    '\\' | '"' => c,
                    '0'..='9' => {
                        let mut value = c as u32 - '0' as u32;

                        if let Some(c) = chars.next_if(|c| c.is_ascii_digit()) {
                            value *= 10;
                            value += c as u32 - '0' as u32;

                            if let Some(c) = chars.next_if(|c| c.is_ascii_digit()) {
                                value *= 10;
                                value += c as u32 - '0' as u32;
                            }
                        }

                        if value > 255 {
                            panic!("bad escape");
                        }

                        value as u8 as char
                    }

                    _ => panic!("bad escape"),
                };

                result.push(v);
            } else {
                result.push(c);
            }
        }

        Cow::Owned(result)
    }
}

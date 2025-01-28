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

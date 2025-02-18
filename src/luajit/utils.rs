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

                            assert!(value < 0x110000, "bad unicode escape");

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

                        if let Some(c) = chars.next_if(char::is_ascii_digit) {
                            value *= 10;
                            value += c as u32 - '0' as u32;

                            if let Some(c) = chars.next_if(char::is_ascii_digit) {
                                value *= 10;
                                value += c as u32 - '0' as u32;
                            }
                        }

                        assert!(value <= 255, "bad escape");

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

pub mod numlit {
    pub fn parse(s: &str) -> f64 {
        let b = s.as_bytes();
        if b.starts_with(b"0x") | b.starts_with(b"0X") {
            let (sig, exp) = parse_hex_inner(b);
            scalbn(sig, exp)
        } else {
            s.parse().unwrap()
        }
    }

    fn parse_hex_inner(s: &[u8]) -> (u64, i32) {
        let mut cur = 2;
        let mut point = None;

        let mut mantissa = 0;
        let mut idx = 0;
        while cur < s.len() && idx < 16 && (s[cur] | 0x20) != b'p' {
            if s[cur] == b'.' {
                point = Some(cur);
                cur += 1;
                continue;
            }

            let mut d = s[cur];
            cur += 1;
            if d > b'9' {
                d += 9;
            }

            mantissa = mantissa << 4 | (d & 0xF) as u64;
            idx += 1;
        }

        let mut exponent = 0;
        if let Some(point) = point {
            exponent += 4 * (point as i32 - (cur - 1) as i32);
            let mut dp = cur - 1;
            while exponent < 0 && s[dp] == b'0' {
                dp -= 1;
                mantissa >>= 4;
                exponent += 4;
            }
        }

        while cur < s.len() && (s[cur] | 0x20) != b'p' {
            if s[cur] == b'.' {
                point = Some(cur);
                cur += 1;
            }

            if s[cur] != b'0' && point.is_none() {
                mantissa |= 1;
                exponent += 4;
            }

            cur += 1;
        }

        if mantissa == 0 {
            return (0, 0);
        }

        if cur >= s.len() {
            return (mantissa, exponent);
        }

        // skip pP
        debug_assert_eq!(s[cur] | 0x20, b'p');
        cur += 1;

        let negx = if s[cur] == b'-' {
            cur += 1;
            true
        } else if s[cur] == b'+' {
            cur += 1;
            false
        } else {
            false
        };

        let mut xx = (s[cur] & 0xF) as i32;
        if negx {
            xx = -xx;
        }
        cur += 1;
        while cur < s.len() && s[cur].is_ascii_digit() {
            xx = xx.saturating_mul(10);
            let d = (s[cur] & 0xF) as i32;
            xx = if negx {
                xx.saturating_sub(d)
            } else {
                xx.saturating_add(d)
            };
            cur += 1;
        }

        exponent += xx;

        (mantissa, exponent)
    }

    // stolen from rust libm
    fn scalbn(mantissa: u64, mut exponent: i32) -> f64 {
        const BITS: u32 = 64;
        const SIG_BITS: u32 = f64::MANTISSA_DIGITS - 1;
        const SIG_MASK: u64 = (1 << SIG_BITS) - 1;
        const EXP_BITS: u32 = BITS - f64::MANTISSA_DIGITS;
        const EXP_MASK: u32 = (1 << EXP_BITS) - 1;
        const EXP_BIAS: u32 = EXP_MASK >> 1;

        // Maximum and minimum values when biased
        const EXP_MAX: i32 = f64::MAX_EXP - 1;
        const EXP_MIN: i32 = f64::MIN_EXP - 1;

        // Minimum and maximum positive normals with null significand (0x1p1023 for f64)
        const F_EXP_MAX: f64 = f64_from_parts(EXP_BIAS << 1, 0);
        const F_EXP_MIN: f64 = f64::MIN_POSITIVE;

        // 2 ^ sig_total_bits, moltiplier to normalize subnormals (0x1p53 for f64)
        const F_POW_SUBNORM: f64 = f64_from_parts(f64::MANTISSA_DIGITS + EXP_BIAS, 0);

        let mut sig = mantissa as f64;

        /*
         * The goal is to multiply `x` by a scale factor that applies `n`. However, there are cases
         * where `2^n` is not representable by `F` but the result should be, e.g. `x = 2^Emin` with
         * `n = -EMin + 2` (one out of range of 2^Emax). To get around this, reduce the magnitude of
         * the final scale operation by prescaling by the max/min power representable by `F`.
         */

        if exponent > EXP_MAX {
            // Worse case positive `n`: `x`  is the minimum subnormal value, the result is `F::MAX`.
            // This can be reached by three scaling multiplications (two here and one final).
            debug_assert!(-EXP_MIN + f64::MANTISSA_DIGITS as i32 + EXP_MAX <= EXP_MAX * 3);

            sig *= F_EXP_MAX;
            exponent -= EXP_MAX;
            if exponent > EXP_MAX {
                sig *= F_EXP_MAX;
                exponent -= EXP_MAX;
                if exponent > EXP_MAX {
                    exponent = EXP_MAX;
                }
            }
        } else if exponent < EXP_MIN {
            // When scaling toward 0, the prescaling is limited to a value that does not allow `x` to
            // go subnormal. This avoids double rounding.
            // `mul` s.t. `!(x * mul).is_subnormal() ∀ x`
            const MUL: f64 = F_EXP_MIN * F_POW_SUBNORM;
            const ADD: i32 = -EXP_MIN - f64::MANTISSA_DIGITS as i32;

            // Worse case negative `n`: `x`  is the maximum positive value, the result is `F::MIN`.
            // This must be reachable by three scaling multiplications (two here and one final).
            debug_assert!(-EXP_MIN + SIG_BITS as i32 + EXP_MAX <= ADD * 2 + -EXP_MIN);

            sig *= MUL;
            exponent += ADD;

            if exponent < EXP_MIN {
                sig *= MUL;
                exponent += ADD;

                if exponent < EXP_MIN {
                    exponent = EXP_MIN;
                }
            }
        }

        let scale = f64_from_parts((EXP_BIAS as i32 + exponent) as u32, 0);
        return sig * scale;

        const fn f64_from_parts(exponent: u32, significand: u64) -> f64 {
            f64::from_bits((((exponent & EXP_MASK) as u64) << SIG_BITS) | (significand & SIG_MASK))
        }
    }

    #[cfg(test)]
    mod test {
        use super::{parse_hex_inner, scalbn};

        #[test]
        fn parse() {
            assert_eq!(parse_hex_inner(b"0x3.14"), (0x314, -8));
            assert_eq!(parse_hex_inner(b"0x3.14fp+3"), (0x314f, 3 - 12));
            assert_eq!(parse_hex_inner(b"0xAbC.p1"), (0xabc, 1));
            assert_eq!(parse_hex_inner(b"0x0.7p1"), (0x7, 1 - 4));
            assert_eq!(parse_hex_inner(b"0x.dEfP-1"), (0xdef, -1 - 12));
            assert_eq!(parse_hex_inner(b"0x0p1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0P1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0.p1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0.P1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0.0p1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0.0P1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x.0p1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x.0P1"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0p0"), (0, 0));
            assert_eq!(parse_hex_inner(b"0x0.p999999999"), (0, 0));
            assert_eq!(
                parse_hex_inner(b"0x0.p99999999999999999999999999999"),
                (0, 0)
            );
            assert_eq!(
                parse_hex_inner(b"0x0.p-99999999999999999999999999999"),
                (0, 0)
            );
            assert_eq!(
                parse_hex_inner(b"0x1.p99999999999999999999999999999"),
                (1, i32::MAX)
            );
            assert_eq!(
                parse_hex_inner(b"0x1.p-99999999999999999999999999999"),
                (1, i32::MIN)
            );
            assert_eq!(parse_hex_inner(b"0x4.00000000000000000p55"), (4, 55));
            assert_eq!(parse_hex_inner(b"0x4.00001000000000000p55"), (0x400001, 35));
            assert_eq!(parse_hex_inner(b"0x4.00000000000000000001p55"), (4, 55));
            assert_eq!(parse_hex_inner(b"0x1p-149"), parse_hex_inner(b"0x1.0p-149"));
        }

        #[test]
        fn convert() {
            assert_eq!(scalbn(0, 0), 0.0);
            assert_eq!(scalbn(1, 0), 1.0);
            assert_eq!(scalbn(10, 0), 10.0);
            assert_eq!(scalbn(10, 1), 20.0);
            assert_eq!(scalbn(10, -1), 5.0);
        }

        #[test]
        fn convert_normal_truncation() {
            assert_eq!(scalbn(0x001f_ffff_ffff_ffff, 0), 9007199254740991.0);
            assert_eq!(scalbn(0x003f_ffff_ffff_ffff, 0), 18014398509481984.0);
            assert_eq!(scalbn(0xffff_ffff_ffff_f800, -11), 9007199254740991.0);
            assert_eq!(scalbn(0xffff_ffff_ffff_fc00, -11), 9007199254740992.0);
        }

        #[test]
        fn convert_denormal_truncation() {
            assert_eq!(scalbn(0x000fffffffffffff, -1074), 2.225073858507201e-308);
            assert_eq!(scalbn(0x001fffffffffffff, -1075), 2.2250738585072014e-308);
            assert_eq!(scalbn(0x001ffffffffffffe, -1075), 2.225073858507201e-308);
            assert_eq!(scalbn(0xfffffffffffff800, -1086), 2.2250738585072014e-308);
            assert_eq!(scalbn(0xfffffffffffff000, -1086), 2.225073858507201e-308);
        }

        #[test]
        fn convert_minimum() {
            assert_eq!(scalbn(0x0000000000000001, -1074), 5e-324);
            assert_eq!(scalbn(0x0000000000000001, -1075), 0.0);
            assert_eq!(scalbn(0x0000000000000002, -1075), 5e-324);
            assert_eq!(scalbn(0x0000000000000002, -1076), 0.0);
            assert_eq!(scalbn(0x0000000000000003, -1075), 1e-323);
            assert_eq!(scalbn(0x0000000000000003, -1076), 5e-324);
            assert_eq!(scalbn(0x8000000000000000, -1137), 5e-324);
            assert_eq!(scalbn(0x8000000000000000, -1138), 0.0);
        }

        #[test]
        fn convert_maximum() {
            assert_eq!(scalbn(0x001fffffffffffff, 971), f64::MAX);
            assert_eq!(scalbn(0x003fffffffffffff, 971), f64::INFINITY);
            assert_eq!(scalbn(0x003ffffffffffffe, 971), f64::INFINITY);
            assert_eq!(scalbn(0x0000000000000001, 1024), f64::INFINITY);
            assert_eq!(scalbn(0x8000000000000000, 961), f64::INFINITY);
            assert_eq!(scalbn(0xfffffffffffff800, 960), f64::MAX);
            assert_eq!(scalbn(0xfffffffffffffc00, 960), f64::INFINITY);
        }
    }
}

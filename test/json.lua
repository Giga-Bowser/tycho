local json = {
	_version = "0.1.2",
}
local encode
local escape_char_map = {
	["\\"] = "\\",
	["\""] = "\"",
	["\b"] = "b",
	["\f"] = "f",
	["\n"] = "n",
	["\r"] = "r",
	["\t"] = "t",
}
local escape_char_map_inv = {
	["/"] = "/",
}
for k, v in pairs(escape_char_map) do
	escape_char_map_inv[v] = k
end
local function escape_char(c)
	return ("\\" .. ((escape_char_map[c] or string.format("u%04x", c:byte()))))
end
local function encode_nil(val)
	return "null"
end
local function encode_table(val, stack)
	local res = {}
	stack = (stack or {})
	if stack[val] then
		error("circular reference")
	end
	stack[val] = true
	if ((rawget(val, 1) ~= nil) or (next(val) == nil)) then
		local n = 0
		for k, _ in pairs(val) do
			if (type(k) ~= "number") then
				error("invalid table: mixed or invalid key types")
			end
			n = n + 1
		end
		if (n ~= #val) then
			error("invalid table: sparse array")
		end
		for i, v in pairs(val) do
			table.insert(res, encode(v, stack))
		end
		stack[val] = nil
		return ("[" .. (table.concat(res, ",") .. "]"))
	else
		for k, v in pairs(val) do
			if (type(k) ~= "string") then
				error("invalid table: mixed or invalid key types")
			end
			table.insert(res, (encode(k, stack) .. (":" .. encode(v, stack))))
		end
		stack[val] = nil
		return ("{" .. (table.concat(res, ",") .. "}"))
	end
end
local function encode_string(val, stack)
	return ("\"" .. (val:gsub("[%z\1-\31\\\"]", escape_char) .. "\""))
end
local function encode_number(val, stack)
	if (((val ~= val) or (val <= -math.huge)) or (val >= math.huge)) then
		error(("unexpected number value '" .. (tostring(val) .. "'")))
	end
	return string.format("%.14g", val)
end
local type_func_map = {
	["nil"] = encode_nil,
	["table"] = encode_table,
	["string"] = encode_string,
	["number"] = encode_number,
	["boolean"] = tostring,
}
encode = function(val, stack)
	local t = type(val)
	local f = type_func_map[t]
	if f then
		return f(val, stack)
	end
	error("unexpected type '" .. t .. "'")
end
json.encode = function(val)
	return encode(val, {})
end
local parse
local function create_set(...)
	local res = {}
	for i = 1, select("#", ...) do
		res[select(i, ...)] = true
	end
	return res
end
local space_chars = create_set(" ", "\t", "\r", "\n")
local delim_chars = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars = create_set("\\", "/", "\"", "b", "f", "n", "r", "t", "u")
local literals = create_set("true", "false", "null")
local literal_map = {
	["true"] = true,
	["false"] = false,
	["null"] = nil,
}
local function next_char(str, idx, set, negate)
	for i = idx, #str do
		if (set[str:sub(i, i)] ~= negate) then
			return i
		end
	end
	return #str + 1
end
local function decode_error(str, idx, msg)
	local line_count = 1
	local col_count = 1
	for i = 1, idx - 1 do
		col_count = col_count + 1
		if (str:sub(i, i) == "\n") then
			line_count = line_count + 1
			col_count = 1
		end
	end
	error(string.format("%s at line %d col %d", msg, line_count, col_count))
end
local function codepoint_to_utf8(n)
	local f = math.floor
	if n <= 0x7f then
		return string.char(n)
	elseif n <= 0x7ff then
		return string.char((f(n / 64) + 192), n % 64 + 128)
	elseif n <= 0xffff then
		return string.char((f(n / 4096) + 224), (f(n % 4096 / 64) + 128), n % 64 + 128)
	elseif n <= 0x10ffff then
		return string.char((f(n / 262144) + 240), (f(n % 262144 / 4096) + 128), (f(n % 4096 / 64) + 128), n % 64 + 128)
	end
	error(string.format("invalid unicode codepoint '%x'", n))
end
local function parse_unicode_escape(s)
	local n1 = tonumber(s:sub(1, 4), 16)
	local n2 = tonumber(s:sub(7, 10), 16)
	if n2 then
		return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
	else
		return codepoint_to_utf8(n1)
	end
end
local function parse_string(str, i)
	local res = ""
	local j = i + 1
	local k = j
	while j <= #str do
		local x = str:byte(j)
		if x < 32 then
			decode_error(str, j, "control character in string")
		elseif x == 92 then
			res = (res .. str:sub(k, j - 1))
			j = j + 1
			local c = str:sub(j, j)
			if c == "u" then
				local hex = ((str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1) or str:match("^%x%x%x%x", j + 1)) or decode_error(str, j - 1, "invalid unicode escape in string"))
				res = (res .. parse_unicode_escape(hex))
				j = j + #hex
			else
				local escape_char = escape_chars[c]
				if not escape_char then
					decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string")
				end
				res = (res .. escape_char_map_inv[c])
			end
			k = j + 1
		elseif x == 34 then
			res = (res .. str:sub(k, j - 1))
			return res, j + 1
		end
		j = j + 1
	end
	decode_error(str, i, "expected closing quote for string")
end
local function parse_number(str, i)
	local x = next_char(str, i, delim_chars)
	local s = str:sub(i, x - 1)
	local n = tonumber(s)
	if not n then
		decode_error(str, i, "invalid number '" .. s .. "'")
	end
	return n, x
end
local function parse_literal(str, i)
	local x = next_char(str, i, delim_chars)
	local word = str:sub(i, x - 1)
	if not literals[word] then
		decode_error(str, i, "invalid literal '" .. word .. "'")
	end
	return literal_map[word], x
end
local function parse_array(str, i)
	local res = {}
	local n = 1
	i = i + 1
	while true do
		local x
		i = next_char(str, i, space_chars, true)
		if (str:sub(i, i) == "]") then
			i = i + 1
			break
		end
		x, i = parse(str, i)
		res[n] = x
		n = n + 1
		i = next_char(str, i, space_chars, true)
		local chr = str:sub(i, i)
		i = i + 1
		if chr == "]" then
			break
		end
		if (chr ~= ",") then
			decode_error(str, i, "expected ']' or ','")
		end
	end
	return res, i
end
local function parse_object(str, i)
	local res = {}
	i = i + 1
	while true do
		local key
		local val
		i = next_char(str, i, space_chars, true)
		if (str:sub(i, i) == "}") then
			i = i + 1
			break
		end
		if (str:sub(i, i) ~= "\"") then
			decode_error(str, i, "expected string for key")
		end
		key, i = parse(str, i)
		i = next_char(str, i, space_chars, true)
		if (str:sub(i, i) ~= ":") then
			decode_error(str, i, "expected ':' after key")
		end
		i = next_char(str, i + 1, space_chars, true)
		val, i = parse(str, i)
		res[key] = val
		i = next_char(str, i, space_chars, true)
		local chr = str:sub(i, i)
		i = i + 1
		if chr == "}" then
			break
		end
		if (chr ~= ",") then
			decode_error(str, i, "expected '}' or ','")
		end
	end
	return res, i
end
local char_func_map = {
	["\""] = parse_string,
	["0"] = parse_number,
	["1"] = parse_number,
	["2"] = parse_number,
	["3"] = parse_number,
	["4"] = parse_number,
	["5"] = parse_number,
	["6"] = parse_number,
	["7"] = parse_number,
	["8"] = parse_number,
	["9"] = parse_number,
	["-"] = parse_number,
	["t"] = parse_literal,
	["f"] = parse_literal,
	["n"] = parse_literal,
	["["] = parse_array,
	["{"] = parse_object,
}
parse = function(str, idx)
	local chr = str:sub(idx, idx)
	local f = char_func_map[chr]
	if f then
		local f = f
		return f(str, idx)
	end
	decode_error(str, idx, "unexpected character '" .. chr .. "'")
end
json.decode = function(str)
	if (type(str) ~= "string") then
		error(("expected argument of type string, got " .. type(str)))
	end
	local res, idx = parse(str, next_char(str, 1, space_chars, true))
	idx = next_char(str, idx, space_chars, true)
	if idx <= #str then
		decode_error(str, idx, "trailing garbage")
	end
	return res
end
local fmt = string.format
local function test(name, f)
	xpcall(function()
		f()
		print(fmt("[pass] %s", name))
	end, function(err)
		print(fmt("[fail] %s : %s", name, err))
	end)
end
local function equal(a, b)
	if ((type(a) == "table") and (type(b) == "table")) then
		local a = a
		local b = b
		for k, _ in pairs(a) do
			if not equal(a[k], b[k]) then
				return false
			end
		end
		for k, _ in pairs(a) do
			if not equal(b[k], a[k]) then
				return false
			end
		end
		return true
	end
	return a == b
end
test("numbers", function()
	local t = {
		["123.456"] = 123.456,
		["-123"] = -123,
		["-567.765"] = -567.765,
		["12.3"] = 12.3,
		["0"] = 0,
		["0.10000000012"] = 0.10000000012,
	}
	for k, v in pairs(t) do
		local res = json.decode(k)
		assert(res == v, fmt("expected '%s', got '%s'", k, res))
		local res = json.encode(v)
		assert(res == k, fmt("expected '%s', got '%s'", v, res))
	end
	assert((json.decode("13e2") == 13e2))
	assert((json.decode("13E+2") == 13e2))
	assert((json.decode("13e-2") == 13e-2))
end)
test("literals", function()
	assert((json.decode("true") == true))
	assert((json.encode(true) == "true"))
	assert((json.decode("false") == false))
	assert((json.encode(false) == "false"))
	assert((json.decode("null") == nil))
	assert((json.encode(nil) == "null"))
end)
test("strings", function()
	local s = ""
	assert((s == json.decode(json.encode(s))))
	local s = "\\"
	assert((s == json.decode(json.encode(s))))
	local s = "Hello world"
	assert((s == json.decode(json.encode(s))))
	local s = "\0 \13 \27"
	assert((s == json.decode(json.encode(s))))
	local s = "\0\r\n\8"
	assert((s == json.decode(json.encode(s))))
end)
test("unicode", function()
	local s = "こんにちは世界"
	assert((s == json.decode(json.encode(s))))
end)
test("arrays", function()
	local t = {
		"cat",
		"dog",
		"owl",
	}
	assert(equal(t, json.decode(json.encode(t))))
end)
test("objects", function()
	local t = {
		x = 10,
		y = 20,
		z = 30,
	}
	assert(equal(t, json.decode(json.encode(t))))
end)
test("decode invalid", function()
	local t = {
		"",
		" ",
		"{",
		"[",
		"{\"x\" : ",
		"{\"x\" : 1",
		"{\"x\" : z }",
		"{\"x\" : 123z }",
		"{x : 123 }",
		"{10 : 123 }",
		"{]",
		"[}",
		"\"a",
		"10 xx",
		"{}123",
	}
	for i, v in pairs(t) do
		local status = pcall(json.decode, v)
		assert(not status, fmt("'%s' was parsed without error", v))
	end
end)
test("decode invalid string", function()
	local t = {
		"\"\\z\"",
		"\"\\1\"",
		"\"\\u000z\"",
		"\"\\ud83d\\ude0q\"",
		"\"x\ny\"",
		"\"x\0y\"",
	}
	for i, v in pairs(t) do
		local status, err = pcall(json.decode, v)
		assert(not status, fmt("'%s' was parsed without error", v))
	end
end)
test("decode escape", function()
	local t = {
		["\"\\u263a\""] = "☺",
		["\"\\ud83d\\ude02\""] = "😂",
		["\"\\r\\n\\t\\\\\\\"\""] = "\r\n\t\\\"",
		["\"\\\\\""] = "\\",
		["\"\\\\\\\\\""] = "\\\\",
		["\"\\/\""] = "/",
		["\"\\\\u \\u263a\""] = "\\u ☺",
	}
	for k, v in pairs(t) do
		local res = json.decode(k)
		assert(res == v, fmt("expected '%s', got '%s'", v, res))
	end
end)
test("decode empty", function()
	local t = {
		["[]"] = {},
		["{}"] = {},
		["\"\""] = "",
	}
	for k, v in pairs(t) do
		local res = json.decode(k)
		assert(equal(res, v), fmt("'%s' did not equal expected", k))
	end
end)
test("decode collection", function()
	local t = {
		["[1, 2, 3, 4, 5, 6]"] = {
		1,
		2,
		3,
		4,
		5,
		6,
	},
		["[1, 2, 3, \"hello\"]"] = {
		1,
		2,
		3,
		"hello",
	},
		["{ \"name\": \"test\", \"id\": 231 }"] = {
		name = "test",
		id = 231,
	},
		["{\"x\":1,\"y\":2,\"z\":[1,2,3]}"] = {
		x = 1,
		y = 2,
		z = {
		1,
		2,
		3,
	},
	},
	}
	for k, v in pairs(t) do
		local res = json.decode(k)
		assert(equal(res, v), fmt("'%s' did not equal expected", k))
	end
end)
test("encode invalid", function()
	local t = {
		{
		[1000] = "b",
	},
		{
		[function()
	end] = 12,
	},
		{
		nil,
		2,
		3,
		4,
	},
		{
		x = 10,
		[1] = 2,
	},
		{
		[1] = "a",
		[3] = "b",
	},
		{
		x = 10,
		[4] = 5,
	},
	}
	for i, v in pairs(t) do
		local status, res = pcall(json.encode, v)
		assert(not status, fmt("encoding idx %d did not result in an error", i))
	end
end)
test("encode invalid number", function()
	local t = {
		math.huge,
		-math.huge,
		(math.huge * 0),
	}
	for i, v in pairs(t) do
		local status, res = pcall(json.encode, v)
		assert(not status, fmt("encoding '%s' did not result in an error", v))
	end
end)
test("encode escape", function()
	local t = {
		["\"x\""] = "\"\\\"x\\\"\"",
		["x\ny"] = "\"x\\ny\"",
		["x\0y"] = "\"x\\u0000y\"",
		["x\27y"] = "\"x\\u001by\"",
		["\r\n\t\\\""] = "\"\\r\\n\\t\\\\\\\"\"",
	}
	for k, v in pairs(t) do
		local res = json.encode(k)
		assert(res == v, fmt("'%s' was not escaped properly", k))
	end
end)

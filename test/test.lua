local function factorial(n)
	if n == 0 then
		return 1
	end
	return (n * factorial(n - 1))
end
local function is_even(n)
	return n % 1 == 0
end
local function countSpaces(str)
	local count = 0
	for i = 1, #str do
		if (str:sub(i) == " ") then
			count = count + 1
		end
	end
	return count
end
local function lerp(a, b, t)
	return (1 - t) * a + t * b
end
local function sign(n)
	local val = 0
	if n > 0 then
		val = 1
	elseif n < 0 then
		val = -1
	end
	return val
end
local function clamp(value, min, max)
	return math.min(math.max(value, min), max)
end
local function clampMag(value, min, max)
	return (sign(value) * clamp(math.abs(value), math.abs(min), math.abs(max)))
end
local function while_break_example()
	local i = 0
	while i < 10 do
		local n = math.random(20)
		if n > 15 then
			break
		end
		i = i + 1
	end
end
local function getTrapezoidSpeed(startSpeed, middleSpeed, endSpeed, totalDistance, rampUpDistance, rampDownDistance, currentDistance)
	if rampDownDistance + rampUpDistance > totalDistance then
		if currentDistance < 0 then
			return startSpeed
		elseif totalDistance < currentDistance then
			return endSpeed
		end
		return lerp(startSpeed, endSpeed, currentDistance / totalDistance)
	end
	if currentDistance < 0 then
		return startSpeed
	elseif currentDistance < rampUpDistance then
		return lerp(startSpeed, middleSpeed, currentDistance / rampUpDistance)
	elseif currentDistance < totalDistance - rampDownDistance then
		return middleSpeed
	elseif currentDistance < totalDistance then
		local rampDownStartDistance = (totalDistance - rampDownDistance)
		return lerp(middleSpeed, endSpeed, (currentDistance - rampDownStartDistance) / rampDownDistance)
	else
		return endSpeed
	end
end
local function bezier(t, p1, p2, p3, p4)
	local q1 = lerp(p1, p2, t)
	local q2 = lerp(p2, p3, t)
	local q3 = lerp(p3, p4, t)
	local r1 = lerp(q1, q2, t)
	local r2 = lerp(q2, q3, t)
	return lerp(r1, r2, t)
end
local function doGrossRampStuff(curr, targ, accel, decel)
	if ((curr == 0 or ((curr > 0 and targ > curr))) or ((curr < 0 and targ < curr))) then
		local change = (math.min(math.abs(curr - targ), accel) * sign(targ - curr))
		curr = curr + change
	elseif (((curr > 0 and targ < curr)) or ((curr < 0 and targ > curr))) then
		local change = (math.min(math.abs(curr - targ), decel) * sign(targ - curr))
		curr = curr + change
	end
	return curr
end
local function deadband(value, band)
	if value > band then
		return (value - band) / (1 - band)
	elseif value < -band then
		return (value + band) / (1 - band)
	end
	return 0
end
local Vector = {}
Vector.__index = Vector
Vector.new = function(_self, x, y)
	local self = {}
	self.x = x
	self.y = y
	setmetatable(self, _self)
	_self.__index = _self
	return self
end
Vector.length = function(self)
	return math.sqrt(((self.x * self.x) + (self.y * self.y)))
end
Vector.normalized = function(self)
	return Vector:new((self.x / self:length()), (self.y / self:length()))
end
Vector.rotate = function(self, radAng)
	return Vector:new((((self.x * math.cos(radAng))) - ((self.y * math.sin(radAng)))), (((self.x * math.sin(radAng))) + ((self.y * math.cos(radAng)))))
end
Vector.dot = function(self, vec)
	return ((self.x * vec.x) + (self.y * vec.y))
end
local Ramp = {}
Ramp.__index = Ramp
Ramp.new = function(_self, timeToMax, timeToStop)
	local self = {}
	self.currentSpeed = 0
	self.maxAccel = 1 / (50 * timeToMax)
	self.maxDecel = 1 / (50 * timeToStop)
	setmetatable(self, _self)
	_self.__index = _self
	return self
end
Ramp.ramp = function(self, targetSpeed)
	self.currentSpeed = doGrossRampStuff(self.currentSpeed, targetSpeed, self.maxAccel, self.maxDecel)
	return self.currentSpeed
end
local PIDController = {}
PIDController.__index = PIDController
PIDController.new = function(_self, p, i, d)
	local self = {}
	self.kp = (p or 0)
	self.ki = (i or 0)
	self.kd = (d or 0)
	self.integral = 0
	self.previousError = nil
	self.previousOutput = nil
	self.previousTime = 0
	self.dt = 0
	self.shouldRunIntegral = false
	setmetatable(self, _self)
	_self.__index = _self
	return self
end
PIDController.clear = function(self, time)
	self.dt = 0
	self.previousTime = time
	self.integral = 0
	self.previousError = nil
	self.previousOutput = nil
	self.shouldRunIntegral = false
end
PIDController.pid = function(self, input, setpoint, thresh, maxChange, maxOutput)
	local threshold = (thresh or 0)
	local error = setpoint - input
	local p = (error * self.kp)
	local i = 0
	if self.shouldRunIntegral then
		if (threshold == 0 or ((input < (threshold + setpoint) and input > (setpoint - threshold)))) then
			self.integral = (self.integral + (self.dt * error))
		else
			self.integral = 0
		end
	else
		self.shouldRunIntegral = true
	end
	local d
	if ((self.previousError == nil) or (self.dt == 0)) then
		d = 0
	else
		d = (((((error - self.previousError)) / self.dt)) * self.kd)
	end
	self.previousError = error
	local output = p + i + d
	if ((self.previousOutput ~= nil) and (maxChange ~= nil)) then
		output = (self.previousOutput + clampMag((output - self.previousOutput), 0, maxChange / 50))
	end
	if (maxOutput ~= nil) then
		output = clampMag(output, 0, maxOutput)
	end
	self.previousOutput = output
	return output
end
local function foo()
	return 42
end
local a = {
	foo = foo(),
	[3] = 43,
	0x45,
}
print("hello, world")
local T = {}
T.__index = T
local tests = {}
local function test(name, f)
	tests[name] = f
end
local reset = "\27[0m"
local red = "\27[31m"
local green = "\27[32m"
local blue = "\27[94m"
T.assertEqual = function(self, actual, expected, message)
	message = (message or (("expected " .. (tostring(expected) .. (", but got " .. tostring(actual))))))
	assert((math.abs(actual - expected) < 0.00001), message)
end
test("Vector tests", function()
	T:assertEqual(Vector:new(3, 4):length(), 5, "Vector:length()")
	T:assertEqual(Vector:new(3, 4):normalized().x, 0.6, "Vector:normalized().x")
	T:assertEqual(Vector:new(3, 4):normalized().y, 0.8, "Vector:normalized().y")
	local actual = Vector:new(1, 2):rotate((math.pi / 2))
	T:assertEqual(actual.x, -2, "Vector:rotate().x")
	T:assertEqual(actual.y, 1, "Vector:rotate().y")
end)
test("getTrapezoidSpeed", function()
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, -1), 0.2)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 0), 0.2)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 0.5), 0.5)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 1), 0.8)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 1.5), 0.8)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 2), 0.8)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 3), 0.8)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 4), 0.6)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 5), 0.4)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 6), 0.4)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, -1), 0.2)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 0), 0.2)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 0.5), 0.5)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 1), 0.8)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 2), 0.6)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 3), 0.4)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 4), 0.4)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, -1), 0.2)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 0), 0.2)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 1.5), 0.3)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 3), 0.4)
	T:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 4), 0.4)
end)
test("doGrossRampStuff", function()
	T:assertEqual(doGrossRampStuff(0.5, 1, 0.2, 0.1), 0.7, "should accelerate by 0.2")
	T:assertEqual(doGrossRampStuff(0.5, 0.1, 0.2, 0.1), 0.4, "should decelerate by 0.1")
	T:assertEqual(doGrossRampStuff(0.5, 0, 0.2, 0.1), 0.4, "should decelerate by 0.1")
	T:assertEqual(doGrossRampStuff(0.5, -1, 0.2, 0.1), 0.4, "should decelerate by 0.1")
	T:assertEqual(doGrossRampStuff(0.5, 0.5, 0.2, 0.1), 0.5, "speed should not change when current and target are equal")
	T:assertEqual(doGrossRampStuff(-0.5, 1, 0.2, 0.1), -0.4, "should decelerate by 0.1")
	T:assertEqual(doGrossRampStuff(-0.5, 0, 0.2, 0.1), -0.4, "should decelerate by 0.1")
	T:assertEqual(doGrossRampStuff(-0.5, -0.1, 0.2, 0.1), -0.4, "should decelerate by 0.1")
	T:assertEqual(doGrossRampStuff(-0.5, -1, 0.2, 0.1), -0.7, "should accelerate by 0.2")
	T:assertEqual(doGrossRampStuff(-0.5, -0.5, 0.2, 0.1), -0.5, "speed should not change when current and target are equal")
	T:assertEqual(doGrossRampStuff(0, 0, 0.2, 0.1), 0, "should go nowhere at zero")
	T:assertEqual(doGrossRampStuff(0, 1, 0.2, 0.1), 0.2, "should accelerate by 0.2 positively")
	T:assertEqual(doGrossRampStuff(0, -1, 0.2, 0.1), -0.2, "should accelerate by 0.2 negatively")
	T:assertEqual(doGrossRampStuff(0.5, 0.6, 1, 0.1), 0.6, "acceleration overshot when positive")
	T:assertEqual(doGrossRampStuff(-0.5, -0.6, 1, 0.1), -0.6, "acceleration overshot when negative")
	T:assertEqual(doGrossRampStuff(0.5, -0.1, 0.1, 1), -0.1, "deceleration overshot when positive")
	T:assertEqual(doGrossRampStuff(-0.5, 0.1, 0.1, 1), 0.1, "deceleration overshot when negative")
end)
test("ramp", function()
	local ramp = Ramp:new(0.1, 0.2)
	T:assertEqual(ramp:ramp(0.9), 0.2)
	T:assertEqual(ramp:ramp(0.9), 0.4)
	T:assertEqual(ramp:ramp(1.1), 0.6)
	T:assertEqual(ramp:ramp(1.1), 0.8)
	T:assertEqual(ramp:ramp(1), 1.0)
	T:assertEqual(ramp:ramp(1), 1.0)
	T:assertEqual(ramp:ramp(0.1), 0.9)
	T:assertEqual(ramp:ramp(0.1), 0.8)
	T:assertEqual(ramp:ramp(0), 0.7)
	T:assertEqual(ramp:ramp(0), 0.6)
	T:assertEqual(ramp:ramp(-0.1), 0.5)
	T:assertEqual(ramp:ramp(-0.1), 0.4)
	T:assertEqual(ramp:ramp(-1), 0.3)
	T:assertEqual(ramp:ramp(-1), 0.2)
	T:assertEqual(ramp:ramp(-1), 0.1)
	T:assertEqual(ramp:ramp(0), 0.0)
	T:assertEqual(ramp:ramp(0), 0.0)
	T:assertEqual(ramp:ramp(-0.9), -0.2)
	T:assertEqual(ramp:ramp(-0.9), -0.4)
	T:assertEqual(ramp:ramp(-1.1), -0.6)
	T:assertEqual(ramp:ramp(-1.1), -0.8)
	T:assertEqual(ramp:ramp(-1), -1.0)
	T:assertEqual(ramp:ramp(-1), -1.0)
	T:assertEqual(ramp:ramp(-0.1), -0.9)
	T:assertEqual(ramp:ramp(-0.1), -0.8)
	T:assertEqual(ramp:ramp(0), -0.7)
	T:assertEqual(ramp:ramp(0), -0.6)
	T:assertEqual(ramp:ramp(0.1), -0.5)
	T:assertEqual(ramp:ramp(0.1), -0.4)
	T:assertEqual(ramp:ramp(1), -0.3)
	T:assertEqual(ramp:ramp(1), -0.2)
	T:assertEqual(ramp:ramp(1), -0.1)
	T:assertEqual(ramp:ramp(0), 0.0)
	T:assertEqual(ramp:ramp(0), 0.0)
end)
test("number tests", function()
	assert((0x3.14 == tonumber("0x3.14")))
	assert((0x3.14fp+3 == tonumber("0x3.14fp+3")))
	assert((0xAbC.p1 == tonumber("0xAbC.p1")))
	assert((0x0.7p1 == tonumber("0x0.7p1")))
	assert((0x.dEfP-1 == tonumber("0x.dEfP-1")))
	assert((0x0p1 == tonumber("0x0p1")))
	assert((0x0P1 == tonumber("0x0P1")))
	assert((0x0.p1 == tonumber("0x0.p1")))
	assert((0x0.P1 == tonumber("0x0.P1")))
	assert((0x0.0p1 == tonumber("0x0.0p1")))
	assert((0x0.0P1 == tonumber("0x0.0P1")))
	assert((0x.0p1 == tonumber("0x.0p1")))
	assert((0x.0P1 == tonumber("0x.0P1")))
	assert((0x0p0 == tonumber("0x0p0")))
	assert((0x0.p9999 == tonumber("0x0.p9999")))
	assert((0x001fffffffffffffp0 == tonumber("0x001fffffffffffffp0")))
	assert((0x003fffffffffffffp0 == tonumber("0x003fffffffffffffp0")))
	assert((0xfffffffffffff800p-11 == tonumber("0xfffffffffffff800p-11")))
	assert((0xfffffffffffffc00p-11 == tonumber("0xfffffffffffffc00p-11")))
	assert((0x000fffffffffffffp-1074 == tonumber("0x000fffffffffffffp-1074")))
	assert((0x001fffffffffffffp-1075 == tonumber("0x001fffffffffffffp-1075")))
	assert((0x001ffffffffffffep-1075 == tonumber("0x001ffffffffffffep-1075")))
	assert((0xfffffffffffff800p-1086 == tonumber("0xfffffffffffff800p-1086")))
	assert((0xfffffffffffff000p-1086 == tonumber("0xfffffffffffff000p-1086")))
	assert((0x0000000000000001p-1074 == tonumber("0x0000000000000001p-1074")))
	assert((0x0000000000000001p-1075 == tonumber("0x0000000000000001p-1075")))
	assert((0x0000000000000002p-1075 == tonumber("0x0000000000000002p-1075")))
	assert((0x0000000000000002p-1076 == tonumber("0x0000000000000002p-1076")))
	assert((0x0000000000000003p-1075 == tonumber("0x0000000000000003p-1075")))
	assert((0x0000000000000003p-1076 == tonumber("0x0000000000000003p-1076")))
	assert((0x8000000000000000p-1137 == tonumber("0x8000000000000000p-1137")))
	assert((0x8000000000000000p-1138 == tonumber("0x8000000000000000p-1138")))
	assert((0x001fffffffffffffp971 == tonumber("0x001fffffffffffffp971")))
	assert((0x003fffffffffffffp971 == tonumber("0x003fffffffffffffp971")))
	assert((0x003ffffffffffffep971 == tonumber("0x003ffffffffffffep971")))
	assert((0x0000000000000001p1024 == tonumber("0x0000000000000001p1024")))
	assert((0x8000000000000000p961 == tonumber("0x8000000000000000p961")))
	assert((0xfffffffffffff800p960 == tonumber("0xfffffffffffff800p960")))
	assert((0xfffffffffffffc00p960 == tonumber("0xfffffffffffffc00p960")))
end)
for name, test in pairs(tests) do
	local result = blue .. name .. ": " .. reset
	local ok, err = pcall(function()
		test()
	end)
	if ok then
		result = result .. green .. "passed"
	else
		result = result .. red .. "failed"
		result = result .. "\n" .. err
	end
	result = result .. reset
	print(result)
end

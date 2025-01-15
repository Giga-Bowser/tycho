local factorial
factorial = function(n)
	if n == 0 then
		return 1
	end
	return (n * factorial(n - 1))
end
local countSpaces
countSpaces = function(str)
	local count = 0
	for i = 1, #str do
		if (str[i] == " ") then
			count = count + 1
		end
	end
	return count
end
local lerp
lerp = function(a, b, t)
	return (1 - t) * a + t * b
end
local sign
sign = function(n)
	local val = 0
	if n > 0 then
		val = 1
	elseif n < 0 then
		val = -1
	end
	return val
end
local clamp
clamp = function(value, min, max)
	return math.min(math.max(value, min), max)
end
local clampMag
clampMag = function(value, min, max)
	return (sign(value) * clamp(math.abs(value), math.abs(min), math.abs(max)))
end
local getTrapezoidSpeed
getTrapezoidSpeed = function(startSpeed, middleSpeed, endSpeed, totalDistance, rampUpDistance, rampDownDistance, currentDistance)
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
local bezier
bezier = function(t, p1, p2, p3, p4)
	local q1 = lerp(p1, p2, t)
	local q2 = lerp(p2, p3, t)
	local q3 = lerp(p3, p4, t)
	local r1 = lerp(q1, q2, t)
	local r2 = lerp(q2, q3, t)
	return lerp(r1, r2, t)
end
local doGrossRampStuff
doGrossRampStuff = function(curr, targ, accel, decel)
	if ((curr == 0 or ((curr > 0 and targ > curr))) or ((curr < 0 and targ < curr))) then
		local change = (math.min(math.abs(curr - targ), accel) * sign(targ - curr))
		curr = curr + change
	elseif (((curr > 0 and targ < curr)) or ((curr < 0 and targ > curr))) then
		local change = (math.min(math.abs(curr - targ), decel) * sign(targ - curr))
		curr = curr + change
	end
	return curr
end
local deadband
deadband = function(value, band)
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
print("hello, world")
local T = {}
T.__index = T
local tests = {
}
local test
test = function(name, f)
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
test("Vector tests", function(t)
	t:assertEqual(Vector:new(3, 4):length(), 5, "Vector:length()")
	t:assertEqual(Vector:new(3, 4):normalized().x, 0.6, "Vector:normalized().x")
	t:assertEqual(Vector:new(3, 4):normalized().y, 0.8, "Vector:normalized().y")
	local actual = Vector:new(1, 2):rotate((math.pi / 2))
	t:assertEqual(actual.x, -2, "Vector:rotate().x")
	t:assertEqual(actual.y, 1, "Vector:rotate().y")
end)
test("getTrapezoidSpeed", function(t)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, -1), 0.2)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 0), 0.2)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 0.5), 0.5)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 1), 0.8)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 1.5), 0.8)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 2), 0.8)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 3), 0.8)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 4), 0.6)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 5), 0.4)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 5, 1, 2, 6), 0.4)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, -1), 0.2)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 0), 0.2)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 0.5), 0.5)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 1), 0.8)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 2), 0.6)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 3), 0.4)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 1, 2, 4), 0.4)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, -1), 0.2)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 0), 0.2)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 1.5), 0.3)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 3), 0.4)
	t:assertEqual(getTrapezoidSpeed(0.2, 0.8, 0.4, 3, 2, 2, 4), 0.4)
end)
test("doGrossRampStuff", function(t)
	t:assertEqual(doGrossRampStuff(0.5, 1, 0.2, 0.1), 0.7, "should accelerate by 0.2")
	t:assertEqual(doGrossRampStuff(0.5, 0.1, 0.2, 0.1), 0.4, "should decelerate by 0.1")
	t:assertEqual(doGrossRampStuff(0.5, 0, 0.2, 0.1), 0.4, "should decelerate by 0.1")
	t:assertEqual(doGrossRampStuff(0.5, -1, 0.2, 0.1), 0.4, "should decelerate by 0.1")
	t:assertEqual(doGrossRampStuff(0.5, 0.5, 0.2, 0.1), 0.5, "speed should not change when current and target are equal")
	t:assertEqual(doGrossRampStuff(-0.5, 1, 0.2, 0.1), -0.4, "should decelerate by 0.1")
	t:assertEqual(doGrossRampStuff(-0.5, 0, 0.2, 0.1), -0.4, "should decelerate by 0.1")
	t:assertEqual(doGrossRampStuff(-0.5, -0.1, 0.2, 0.1), -0.4, "should decelerate by 0.1")
	t:assertEqual(doGrossRampStuff(-0.5, -1, 0.2, 0.1), -0.7, "should accelerate by 0.2")
	t:assertEqual(doGrossRampStuff(-0.5, -0.5, 0.2, 0.1), -0.5, "speed should not change when current and target are equal")
	t:assertEqual(doGrossRampStuff(0, 0, 0.2, 0.1), 0, "should go nowhere at zero")
	t:assertEqual(doGrossRampStuff(0, 1, 0.2, 0.1), 0.2, "should accelerate by 0.2 positively")
	t:assertEqual(doGrossRampStuff(0, -1, 0.2, 0.1), -0.2, "should accelerate by 0.2 negatively")
	t:assertEqual(doGrossRampStuff(0.5, 0.6, 1, 0.1), 0.6, "acceleration overshot when positive")
	t:assertEqual(doGrossRampStuff(-0.5, -0.6, 1, 0.1), -0.6, "acceleration overshot when negative")
	t:assertEqual(doGrossRampStuff(0.5, -0.1, 0.1, 1), -0.1, "deceleration overshot when positive")
	t:assertEqual(doGrossRampStuff(-0.5, 0.1, 0.1, 1), 0.1, "deceleration overshot when negative")
end)
test("ramp", function(t)
	local ramp = Ramp:new(0.1, 0.2)
	t:assertEqual(ramp:ramp(0.9), 0.2)
	t:assertEqual(ramp:ramp(0.9), 0.4)
	t:assertEqual(ramp:ramp(1.1), 0.6)
	t:assertEqual(ramp:ramp(1.1), 0.8)
	t:assertEqual(ramp:ramp(1), 1.0)
	t:assertEqual(ramp:ramp(1), 1.0)
	t:assertEqual(ramp:ramp(0.1), 0.9)
	t:assertEqual(ramp:ramp(0.1), 0.8)
	t:assertEqual(ramp:ramp(0), 0.7)
	t:assertEqual(ramp:ramp(0), 0.6)
	t:assertEqual(ramp:ramp(-0.1), 0.5)
	t:assertEqual(ramp:ramp(-0.1), 0.4)
	t:assertEqual(ramp:ramp(-1), 0.3)
	t:assertEqual(ramp:ramp(-1), 0.2)
	t:assertEqual(ramp:ramp(-1), 0.1)
	t:assertEqual(ramp:ramp(0), 0.0)
	t:assertEqual(ramp:ramp(0), 0.0)
	t:assertEqual(ramp:ramp(-0.9), -0.2)
	t:assertEqual(ramp:ramp(-0.9), -0.4)
	t:assertEqual(ramp:ramp(-1.1), -0.6)
	t:assertEqual(ramp:ramp(-1.1), -0.8)
	t:assertEqual(ramp:ramp(-1), -1.0)
	t:assertEqual(ramp:ramp(-1), -1.0)
	t:assertEqual(ramp:ramp(-0.1), -0.9)
	t:assertEqual(ramp:ramp(-0.1), -0.8)
	t:assertEqual(ramp:ramp(0), -0.7)
	t:assertEqual(ramp:ramp(0), -0.6)
	t:assertEqual(ramp:ramp(0.1), -0.5)
	t:assertEqual(ramp:ramp(0.1), -0.4)
	t:assertEqual(ramp:ramp(1), -0.3)
	t:assertEqual(ramp:ramp(1), -0.2)
	t:assertEqual(ramp:ramp(1), -0.1)
	t:assertEqual(ramp:ramp(0), 0.0)
	t:assertEqual(ramp:ramp(0), 0.0)
end)
for name, test in pairs(tests) do
	local result = blue .. name .. ": " .. reset
	local ok, err = pcall(function()
		test(T)
	end)
	if ok then
		result = result .. green .. "ok, computer"
	else
		result = result .. red .. "test failed"
		result = result .. "\n" .. err
	end
	result = result .. reset
	print(result)
end

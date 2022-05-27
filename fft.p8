pico-8 cartridge // http://www.pico-8.com
version 34
__lua__

printh("---------start----------")

spectro_y = 100
spectro_height = 28
t = 0
bg_col = 5
foreground_col = 7

function _update60()
end

function tick()
    local to_buffer = 2048 - stat(108)
    local buffersize = 256
    local draw_incr = 512 / (8*buffersize)
    if force_reset or to_buffer > buffersize then
        force_reset = false
        for i=0,buffersize do
            t += 0.1
            local t_pat = (t\80) % 64
            local wave = 0

            local note_mult_a = 0.5 
            if (note_mult_a > 0) then
                local input_sin = t * 0.5 * note_mult_a
                wave += (sin(input_sin)) * 0.5
                wave += (sin(t * 0.5 * note_mult_a)) * 0.5
            end


            if wave > 1 then
            wave = 1
            elseif wave < -1 then
            wave = -1
            end

            if i == 0 then
                cls(bg_col)
                line(16, spectro_y, 16, spectro_y, foreground_col)
            elseif i % 8 == 0 then
                line(16+draw_incr * i, spectro_y + wave * spectro_height, foreground_col)
            end
            poke(0x4300 + i, (0.5 + 0.5*wave)*255)
        end

        serial(0x808, 0x4300, buffersize)
    end
end

function _draw()
end

local string = {} 
string.format = function(x) return x end
pi = 3.141
floor = flr

local table = {}
table.insert = function(x, y) add(x, y) end


-- 'complex' provides common tasks with complex numbers

-- function complex.to( arg ); complex( arg )
-- returns a complex number on success, nil on failure
-- arg := number or { number,number } or ( "(-)<number>" and/or "(+/-)<number>i" )
--      e.g. 5; {2,3}; "2", "2+i", "-2i", "2^2*3+1/3i"
--      note: 'i' is always in the numerator, spaces are not allowed

-- a complex number is defined as carthesic complex number
-- complex number := { real_part, imaginary_part }
-- this gives fast access to both parts of the number for calculation
-- the access is faster than in a hash table
-- the metatable is just a add on, when it comes to speed, one is faster using a direct function call

-- http://luaforge.net/projects/LuaMatrix
-- http://lua-users.org/wiki/ComplexNumbers

-- Licensed under the same terms as Lua itself.

--/////////////--
--// complex //--
--/////////////--

-- link to complex table
complex = {}

-- link to complex metatable
local complex_meta = {}

-- complex.to( arg )
-- return a complex number on success
-- return nil on failure
local _retone = function() return 1 end
local _retminusone = function() return -1 end
function complex.to( num )
   -- check for table type
   if type( num ) == "table" then
      -- check for a complex number
      if getmetatable( num ) == complex_meta then
         return num
      end
      local real,imag = tonumber( num[1] ),tonumber( num[2] )
      if real and imag then
         return setmetatable( { real,imag }, complex_meta )
      end
      return
   end
   -- check for number
   local isnum = tonumber( num )
   if isnum then
      return setmetatable( { isnum,0 }, complex_meta )
   end
   if type( num ) == "string" then
      -- check for real and complex
      -- number chars [%-%+%*%^%d%./Ee]
      local real,sign,imag = string.match( num, "^([%-%+%*%^%d%./Ee]*%d)([%+%-])([%-%+%*%^%d%./Ee]*)i$" )
      if real then
         if string.lower(string.sub(real,1,1)) == "e"
         or string.lower(string.sub(imag,1,1)) == "e" then
            return
         end
         if imag == "" then
            if sign == "+" then
               imag = _retone
            else
               imag = _retminusone
            end
         elseif sign == "+" then
            imag = loadstring("return tonumber("..imag..")")
         else
            imag = loadstring("return tonumber("..sign..imag..")")
         end
         real = loadstring("return tonumber("..real..")")
         if real and imag then
            return setmetatable( { real(),imag() }, complex_meta )
         end
         return
      end
      -- check for complex
      local imag = string.match( num,"^([%-%+%*%^%d%./Ee]*)i$" )
      if imag then
         if imag == "" then
            return setmetatable( { 0,1 }, complex_meta )
         elseif imag == "-" then
            return setmetatable( { 0,-1 }, complex_meta )
         end
         if string.lower(string.sub(imag,1,1)) ~= "e" then
            imag = loadstring("return tonumber("..imag..")")
            if imag then
               return setmetatable( { 0,imag() }, complex_meta )
            end
         end
         return
      end
      -- should be real
      local real = string.match( num,"^(%-*[%d%.][%-%+%*%^%d%./Ee]*)$" )
      if real then
         real = loadstring( "return tonumber("..real..")" )
         if real then
            return setmetatable( { real(),0 }, complex_meta )
         end
      end
   end
end

-- complex( arg )
-- same as complex.to( arg )
-- set __call behaviour of complex
setmetatable( complex, { __call = function( _,num ) return complex.to( num ) end } )

-- complex.new( real, complex )
-- fast function to get a complex number, not invoking any checks
function complex.new( ... )
   return setmetatable( { ... }, complex_meta )
end

-- complex.type( arg )
-- is argument of type complex
function complex.type( arg )
   if getmetatable( arg ) == complex_meta then
      return "complex"
   end
end

-- complex.convpolar( r, phi )
-- convert polar coordinates ( r*e^(i*phi) ) to carthesic complex number
-- r (radius) is a number
-- phi (angle) must be in radians; e.g. [0 - 2pi]
function complex.convpolar( radius, phi )
   return setmetatable( { radius * cos( phi ), radius * sin( phi ) }, complex_meta )
end

-- complex.convpolardeg( r, phi )
-- convert polar coordinates ( r*e^(i*phi) ) to carthesic complex number
-- r (radius) is a number
-- phi must be in degrees; e.g. [0° - 360°]
function complex.convpolardeg( radius, phi )
   phi = phi/180 * pi
   return setmetatable( { radius * cos( phi ), radius * sin( phi ) }, complex_meta )
end

--// complex number functions only

-- complex.tostring( cx [, formatstr] )
-- to string or real number
-- takes a complex number and returns its string value or real number value
function complex.tostring( cx,formatstr )
   local real,imag = cx[1],cx[2]
   if formatstr then
      if imag == 0 then
         return string.format( formatstr, real )
      elseif real == 0 then
         return string.format( formatstr, imag ).."i"
      elseif imag > 0 then
         return string.format( formatstr, real ).."+"..string.format( formatstr, imag ).."i"
      end
      return string.format( formatstr, real )..string.format( formatstr, imag ).."i"
   end
   if imag == 0 then
      return real
   elseif real == 0 then
      return ((imag==1 and "") or (imag==-1 and "-") or imag).."i"
   elseif imag > 0 then
      return real.."+"..(imag==1 and "" or imag).."i"
   end
   return real..(imag==-1 and "-" or imag).."i"
end

-- complex.print( cx [, formatstr] )
-- print a complex number
function complex.print( ... )
   print( complex.tostring( ... ) )
end

-- complex.polar( cx )
-- from complex number to polar coordinates
-- output in radians; [-pi,+pi]
-- returns r (radius), phi (angle)
function complex.polar( cx )
   return sqrt( cx[1]^2 + cx[2]^2 ), atan2( cx[2], cx[1] )
end

-- complex.polardeg( cx )
-- from complex number to polar coordinates
-- output in degrees; [-180°,180°]
-- returns r (radius), phi (angle)
function complex.polardeg( cx )
   return sqrt( cx[1]^2 + cx[2]^2 ), atan2( cx[2], cx[1] ) / pi * 180
end

-- complex.mulconjugate( cx )
-- multiply with conjugate, function returning a number
function complex.mulconjugate( cx )
   return cx[1]^2 + cx[2]^2
end

-- complex.abs( cx )
-- get the absolute value of a complex number
function complex.abs( cx )
   return sqrt( cx[1]*cx[1] + cx[2]*cx[2] )
end

-- complex.get( cx )
-- returns real_part, imaginary_part
function complex.get( cx )
   return cx[1],cx[2]
end

-- complex.set( cx, real, imag )
-- sets real_part = real and imaginary_part = imag
function complex.set( cx,real,imag )
   cx[1],cx[2] = real,imag
end

-- complex.is( cx, real, imag )
-- returns true if, real_part = real and imaginary_part = imag
-- else returns false
function complex.is( cx,real,imag )
   if cx[1] == real and cx[2] == imag then
      return true
   end
   return false
end

--// functions returning a new complex number

-- complex.copy( cx )
-- copy complex number
function complex.copy( cx )
   return setmetatable( { cx[1],cx[2] }, complex_meta )
end

-- complex.add( cx1, cx2 )
-- add two numbers; cx1 + cx2
function complex.add( cx1,cx2 )
   return setmetatable( { cx1[1]+cx2[1], cx1[2]+cx2[2] }, complex_meta )
end

-- complex.sub( cx1, cx2 )
-- subtract two numbers; cx1 - cx2
function complex.sub( cx1,cx2 )
   return setmetatable( { cx1[1]-cx2[1], cx1[2]-cx2[2] }, complex_meta )
end

-- complex.mul( cx1, cx2 )
-- multiply two numbers; cx1 * cx2
function complex.mul( cx1,cx2 )
   return setmetatable( { cx1[1]*cx2[1] - cx1[2]*cx2[2],cx1[1]*cx2[2] + cx1[2]*cx2[1] }, complex_meta )
end

-- complex.mulnum( cx, num )
-- multiply complex with number; cx1 * num
function complex.mulnum( cx,num )
   return setmetatable( { cx[1]*num,cx[2]*num }, complex_meta )
end

-- complex.div( cx1, cx2 )
-- divide 2 numbers; cx1 / cx2
function complex.div( cx1,cx2 )
   -- get complex value
   local val = cx2[1]^2 + cx2[2]^2
   -- multiply cx1 with conjugate complex of cx2 and divide through val
   return setmetatable( { (cx1[1]*cx2[1]+cx1[2]*cx2[2])/val,(cx1[2]*cx2[1]-cx1[1]*cx2[2])/val }, complex_meta )
end

-- complex.divnum( cx, num )
-- divide through a number
function complex.divnum( cx,num )
   return setmetatable( { cx[1]/num,cx[2]/num }, complex_meta )
end

-- complex.pow( cx, num )
-- get the power of a complex number
function complex.pow( cx,num )
   if floor( num ) == num then
      if num < 0 then
         local val = cx[1]^2 + cx[2]^2
         cx = { cx[1]/val,-cx[2]/val }
         num = -num
      end
      local real,imag = cx[1],cx[2]
      for i = 2,num do
         real,imag = real*cx[1] - imag*cx[2],real*cx[2] + imag*cx[1]
      end
      return setmetatable( { real,imag }, complex_meta )
   end
   -- we calculate the polar complex number now
   -- since then we have the versatility to calc any potenz of the complex number
   -- then we convert it back to a carthesic complex number, we loose precision here
   local length,phi = sqrt( cx[1]^2 + cx[2]^2 )^num, atan2( cx[2], cx[1] )*num
   return setmetatable( { length * cos( phi ), length * sin( phi ) }, complex_meta )
end

-- complex.sqrt( cx )
-- get the first squareroot of a complex number, more accurate than cx^.5
function complex.sqrt( cx )
   local len = sqrt( cx[1]^2+cx[2]^2 )
   local sign = (cx[2]<0 and -1) or 1
   return setmetatable( { sqrt((cx[1]+len)/2), sign*sqrt((len-cx[1])/2) }, complex_meta )
end

-- complex.ln( cx )
-- natural logarithm of cx
function complex.ln( cx )
   return setmetatable( { log(sqrt( cx[1]^2 + cx[2]^2 )),
      atan2( cx[2], cx[1] ) }, complex_meta )
end

-- complex.exp( cx )
-- exponent of cx (e^cx)
function complex.exp( cx )
   local expreal = exp(cx[1])
   return setmetatable( { expreal*cos(cx[2]), expreal*sin(cx[2]) }, complex_meta )
end

-- complex.conjugate( cx )
-- get conjugate complex of number
function complex.conjugate( cx )
   return setmetatable( { cx[1], -cx[2] }, complex_meta )
end

-- complex.round( cx [,idp] )
-- round complex numbers, by default to 0 decimal points
function complex.round( cx,idp )
   local mult = 10^( idp or 0 )
   return setmetatable( { floor( cx[1] * mult + 0.5 ) / mult,
      floor( cx[2] * mult + 0.5 ) / mult }, complex_meta )
end

--// metatable functions

complex_meta.__add = function( cx1,cx2 )
   local cx1,cx2 = complex.to( cx1 ),complex.to( cx2 )
   return complex.add( cx1,cx2 )
end
complex_meta.__sub = function( cx1,cx2 )
   local cx1,cx2 = complex.to( cx1 ),complex.to( cx2 )
   return complex.sub( cx1,cx2 )
end
complex_meta.__mul = function( cx1,cx2 )
   local cx1,cx2 = complex.to( cx1 ),complex.to( cx2 )
   return complex.mul( cx1,cx2 )
end
complex_meta.__div = function( cx1,cx2 )
   local cx1,cx2 = complex.to( cx1 ),complex.to( cx2 )
   return complex.div( cx1,cx2 )
end
complex_meta.__pow = function( cx,num )
   if num == "*" then
      return complex.conjugate( cx )
   end
   return complex.pow( cx,num )
end
complex_meta.__unm = function( cx )
   return setmetatable( { -cx[1], -cx[2] }, complex_meta )
end
complex_meta.__eq = function( cx1,cx2 )
   if cx1[1] == cx2[1] and cx1[2] == cx2[2] then
      return true
   end
   return false
end
complex_meta.__tostring = function( cx )
   return tostring( complex.tostring( cx ) )
end
complex_meta.__concat = function( cx,cx2 )
   return tostring(cx)..tostring(cx2)
end
-- cx( cx, formatstr )
complex_meta.__call = function( ... )
   print( complex.tostring( ... ) )
end
complex_meta.__index = {}
for k,v in pairs( complex ) do
   complex_meta.__index[k] = v
end

---------------------------------------------------------------
--This is a lua port of the KissFFT Library by Mark Borgerding
--It provides a simple function to carry out a fast fourier transformation (FFT).

local luafft = {}
local debugging = true

local function msg(...)
	if debugging == true then
		print(...)
	end
end

---------------------------------------------------------------
-- Returns the next possible size for data input
--
--@param n	Size
--
--@return	Next fast size.
local function next_possible_size(n)
  local m = n
  while (1) do
    m = n
    while m%2 == 0 do m = m/2 end
    while m%3 == 0 do m = m/3 end
    while m%5 == 0 do m = m/5 end
	if m <= 1 then break end
    n = n + 1
  end
  return n
end

---------------------------------------------------------------
--Calculates the Fast Fourier Transformation of the given input
--
--@param input		A set of points that will be transformed.
--					At this point, the input has to be a list of complex numbers,
--					according to the format in complex.lua.
--@param inverse	Boolean that controls whether a transformation
--					or inverse transformation will be carried out.
--@return			Returns a list of complex numbers with the same size
--					as the input list. Contains the fourier transformation of the input.
---------------------------------------------------------------
function luafft.fft(input, inverse)
	--the size of input defines the number of total points
	local num_points = #input

    local nps = next_possible_size(#input)
    print(nps)
	assert(#input == nps, string.format("The size of your input is not correct. For your size=%i, use a table of size=%i with zeros at the end.", #input, nps))

	local twiddles = {}
   -- DAN MOD
   --num_dan = num_points
   num_dan = num_points / 2
	--for i = 0,num_points-1 do
		--local phase = -2*pi * i / num_points
	for i = 0,num_dan - 1 do
		local phase = 1 * i / num_dan
		if inverse then phase = phase * -1 end
      local twiddle_real = cos(phase)
      local twiddle_imaginary = sin(phase)
      --msg("Twiddle phase=")
      --msg(phase)
      --msg("Twiddle real=")
      --msg(twiddle_real)
      --msg("Twiddle imaginary=")
      --msg(twiddle_imaginary)
		twiddles[1+i] = {real = twiddle_real, imag = twiddle_imaginary}
	end
   --printh("twiddles")
   --for i,c in pairs(twiddles) do
   --   printh(c.real .. "+" .. c.imag .. "i")
   --end
   --while true do end
	msg("Twiddles initialized...")
	local factors = calculate_factors(num_dan)
	local output = {}
	msg("FFT Initialization completed.\nFactors of size " .. #factors)
	work(input, output, 1, 1, factors,1, twiddles, 1, 1, inverse)
	return output

end

---------------------------------------------------------------
--Calculates the real Fast Fourier Transformation of the given real input
--

---------------------------------------------------------------
function luafft.fftr(input, inverse)
	print("Not implemented.")
end



---------------------------------------------------------------
-- Short helper function that provides an easy way to print a list with values.
---------------------------------------------------------------
function print_list(list)
  for i,v in ipairs(list) do print(i,v) end
end

---------------------------------------------------------------
--The essential work function that performs the FFT
---------------------------------------------------------------
function work(input, output, out_index, f, factors, factors_index, twiddles, fstride, in_stride, inverse)
	local p = factors[factors_index]
	local m = factors[factors_index+1]
	factors_index = factors_index + 2
   --msg("Work p = ")
   --msg(p)
   --msg("Work m = ")
   --msg(m)
	--msg(p,m)
	local last = out_index + p*m
	local beg = out_index

   --print("work m=" .. m)

	if m == 1 then
		repeat
			--if type(input[f]) == "number" then output[out_index] = complex.new(input[f],0)
			--else output[out_index] = input[f] end
         --print("f = " .. f)
         --print(input[f].real)
         --print(input[f].imag)
			output[out_index] = input[f]
			f = f + fstride*in_stride
			out_index = out_index +1
		until out_index == last
	else
		repeat
			--msg("Out_index", out_index,"f", f)
			work(input, output,out_index,  f, factors, factors_index, twiddles, fstride*p, in_stride, inverse)
         --print(output[out_index].real)
         --print(output[out_index].imag)
			f = f + fstride*in_stride
			out_index = out_index + m
		until out_index == last
	end

   --for i,c in pairs(output) do
   --   print(c)
   --   print(c.real)
   --   print(c.imag)
   --end

	out_index = beg

	if p == 2 then 			butterfly2_dan(output,out_index, fstride, twiddles, m)
	elseif p == 3 then 		butterfly3_dan(output,out_index, fstride, twiddles, m)
	elseif p == 4 then 		butterfly4_dan(output,out_index, fstride, twiddles, m)
	elseif p == 5 then 	butterfly5_dan(output,out_index, fstride, twiddles, m)
	else 					butterfly_generic_dan(output,out_index, fstride, twiddles, m, p) end

   --print("done butterfly")

   --for i,c in pairs(output) do
   --   print(c)
   --   print(c.real)
   --   print(c.imag)
   --end
end


---------------------------------------------------------------
---devides a number into a sequence of factors
--
--@param num_points	Number of points that are used.
--
--@return		Returns a list with the factors
---------------------------------------------------------------
function calculate_factors(num_points)

  local buf = {}
  local p = 4

  floor_sqrt = floor( sqrt( num_points) )
  local n = num_points
  repeat
    while n%p > 0 do
      if 		p == 4 then p = 2
      elseif 	p == 2 then p = 3
      else 					p = p + 2 end

      if p > floor_sqrt then p = n end
    end
    n = n / p
    table.insert(buf, p)
    table.insert(buf, n)
  until n <= 1
  return buf
end


function complex_add(x, y)
   return {real = x.real + y.real, imag = x.imag + y.imag}
end

function complex_sub(x, y)
   return {real = x.real - y.real, imag = x.imag - y.imag}
end

function complex_mult(x, y)
   return {real = x.real * y.real - x.imag * y.imag, imag = x.real * y.imag + x.imag * y.real}
end

function complex_abs(c)
   return sqrt(c.real * c.real + c.imag * c.imag)
end

---------------------------------------------------------------
--Carries out a butterfly 2 run of the input sample.
---------------------------------------------------------------
function butterfly2_dan(input,out_index,fstride, twiddles, m)
    local i1 = out_index
    local i2 = out_index + m
    local ti = 1
    repeat
      local t = complex_mult(input[i2], twiddles[ti])
      ti = ti + fstride
      input[i2] = complex_sub(input[i1], t)
      input[i1] = complex_add(input[i1], t)

      --printh("---------")
      --printh("Butterfly2")
      --printh("Fout2 " .. input[i2].real .. "+" .. input[i2].imag .. "i")
      --printh("Fout1 " .. input[i1].real .. "+" .. input[i1].imag .. "i")

      i1 = i1 + 1
      i2 = i2 + 1
      m = m - 1
    until m == 0

   --print("Butterfly2")
   --for i,c in pairs(input) do
   --   print(c.real .. "+" .. c.imag .. "i")
   --end
   --while true do end
end
function butterfly2(input,out_index,fstride, twiddles, m, inverse)
    local i1 = out_index
    local i2 = out_index + m
    local ti = 1
    repeat
      local t = input[i2] * twiddles[ti]
      ti = ti + fstride
      input[i2] = input[i1] - t
      input[i1] = input[i1] + t
      i1 = i1 + 1
      i2 = i2 + 1
      m = m - 1
    until m == 0
end

---------------------------------------------------------------
--Carries out a butterfly 4 run of the input sample.
---------------------------------------------------------------
function butterfly4_dan(input,out_index, fstride, twiddles, m)
	local ti1, ti2, ti3 = 1,1,1
	local scratch = {}
	local k = m
	local m2 = 2*m
	local m3 = 3*m
	local i = out_index

   --print("Butterfly4_pre")
   --for i,c in pairs(input) do
   --   print(c.real .. "+" .. c.imag .. "i")
   --end
   --while true do end

	repeat
      --printh("Butterfly4_pre")
      --printh("Fout " .. input[i].real .. "+" .. input[i].imag .. "i")
      --printh("Fout[m] " .. input[i+m].real .. "+" .. input[i+m].imag .. "i")
      --printh("Fout[m2] " .. input[i+m2].real .. "+" .. input[i+m2].imag .. "i")
      --printh("Fout[m3] " .. input[i+m3].real .. "+" .. input[i+m3].imag .. "i")
		scratch[0] = complex_mult(input[i+m],twiddles[ti1])
		scratch[1] = complex_mult(input[i+m2],twiddles[ti2])
		scratch[2] = complex_mult(input[i+m3],twiddles[ti3])

		scratch[5] = complex_sub(input[i],scratch[1])
		input[i] = complex_add(input[i],scratch[1])

		scratch[3] = complex_add(scratch[0],scratch[2])
		scratch[4] = complex_sub(scratch[0],scratch[2])

		input[i+m2] = complex_sub(input[i],scratch[3])
      --printh("twiddles[ti1] " .. twiddles[ti1].real .. "+" .. twiddles[ti1].imag .. "i")
      --printh("twiddles[ti2] " .. twiddles[ti2].real .. "+" .. twiddles[ti2].imag .. "i")
      --printh("twiddles[ti3] " .. twiddles[ti3].real .. "+" .. twiddles[ti3].imag .. "i")

		ti1 = ti1 + fstride
		ti2 = ti2 + fstride*2
		ti3 = ti3 + fstride*3
		input[i] = complex_add(input[i],scratch[3])

      input[i+m].real = (scratch[5].real + scratch[4].imag)
      input[i+m].imag = (scratch[5].imag - scratch[4].real)

      input[i+m3].real = (scratch[5].real - scratch[4].imag)
      input[i+m3].imag = (scratch[5].imag + scratch[4].real)

      --for i,c in pairs(scratch) do
      --   printh("scratch " .. i .. " - " .. c.real .. "+" .. c.imag .. "i")
      --end

      --printh("---------")
      --printh("--Butterfly4")
      --printh("---------")
      --printh("Fout " .. input[i].real .. "+" .. input[i].imag .. "i")
      --printh("Fout[m] " .. input[i+m].real .. "+" .. input[i+m].imag .. "i")
      --printh("Fout[m2] " .. input[i+m2].real .. "+" .. input[i+m2].imag .. "i")
      --printh("Fout[m3] " .. input[i+m3].real .. "+" .. input[i+m3].imag .. "i")

		i = i + 1
		k = k - 1
	until k == 0

   --print("Butterfly4")
   --for i,c in pairs(input) do
      --print(c.real .. "+" .. c.imag .. "i")
   --end
end
function butterfly4(input,out_index, fstride, twiddles, m, inverse)
	local ti1, ti2, ti3 = 1,1,1
	local scratch = {}
	local k = m
	local m2 = 2*m
	local m3 = 3*m
	local i = out_index

	repeat
		scratch[0] = input[i+m]*twiddles[ti1]
		scratch[1] = input[i+m2]*twiddles[ti2]
		scratch[2] = input[i+m3]*twiddles[ti3]

		scratch[5] = input[i]-scratch[1]
		input[i] = input[i] + scratch[1]

		scratch[3] = scratch[0] + scratch[2]
		scratch[4] = scratch[0] - scratch[2]

		input[i+m2] = input[i] - scratch[3]
		ti1 = ti1 + fstride
		ti2 = ti2 + fstride*2
		ti3 = ti3 + fstride*3
		input[i] = input[i] + scratch[3]

		if inverse then
			input[i+m][1] = scratch[5][1] - scratch[4][2]
			input[i+m][2] = scratch[5][2] + scratch[4][1]

			input[i+m3][1] = scratch[5][1] + scratch[4][2]
			input[i+m3][2] = scratch[5][2] - scratch[4][1]
		else
			input[i+m][1] = scratch[5][1] + scratch[4][2]
			input[i+m][2] = scratch[5][2] - scratch[4][1]

			input[i+m3][1] = scratch[5][1] - scratch[4][2]
			input[i+m3][2] = scratch[5][2] + scratch[4][1]
		end
		i = i + 1
		k = k - 1
	until k == 0
end

---------------------------------------------------------------
--Carries out a butterfly 3 run of the input sample.
---------------------------------------------------------------
function butterfly3_dan(input,out_index, fstride, twiddles, m)
	local k = m
	local m2 = m*2
	local tw1, tw2 = 1,1
	local scratch = {}
	local epi3 = twiddles[fstride*m]
	local i = out_index

	repeat
		scratch[1] = complex_mult(input[i+m], twiddles[tw1])
		scratch[2] = complex_mult(input[i+m2], twiddles[tw2])
		scratch[3] = complex_add(scratch[1], scratch[2])
		scratch[0] = complex_sub(scratch[1], scratch[2])
		tw1 = tw1 + fstride
		tw2 = tw2 + fstride*2

		input[i+m].real = input[i].real - scratch[3].real*0.5
		input[i+m].imag = input[i].imag - scratch[3].imag*0.5

      scratch[0].real = scratch[0].real * epi3.imag
      scratch[0].imag = scratch[0].iamg * epi3.imag
		input[i] = complex_add(input[i], scratch[3])

		input[i+m2].real = input[i+m].real + scratch[0].imag
		input[i+m2].imag = input[i+m].imag - scratch[0].real

		input[i+m].real = input[i+m].real - scratch[0].imag
		input[i+m].imag = input[i+m].imag + scratch[0].real

		i = i + 1
		k = k-1
	until k == 0
end

function butterfly3(input,out_index, fstride, twiddles, m, inverse)
	local k = m
	local m2 = m*2
	local tw1, tw2 = 1,1
	local scratch = {}
	local epi3 = twiddles[fstride*m]
	local i = out_index

	repeat
		scratch[1] = input[i+m] * twiddles[tw1]
		scratch[2] = input[i+m2] * twiddles[tw2]
		scratch[3] = scratch[1] + scratch[2]
		scratch[0] = scratch[1] - scratch[2]
		tw1 = tw1 + fstride
		tw2 = tw2 + fstride*2

		input[i+m][1] = input[i][1] - scratch[3][1]*0.5
		input[i+m][2] = input[i][2] - scratch[3][2]*0.5

		scratch[0] = scratch[0]:mulnum(epi3[2] )
		input[i] = input[i] + scratch[3]

		input[i+m2][1] = input[i+m][1] + scratch[0][2]
		input[i+m2][2] = input[i+m][2] - scratch[0][1]

		input[i+m][1] = input[i+m][1] - scratch[0][2]
		input[i+m][2] = input[i+m][2] + scratch[0][1]

		i = i + 1
		k = k-1
	until k == 0

end

---------------------------------------------------------------
--Carries out a butterfly 5 run of the input sample.
---------------------------------------------------------------
function butterfly5_dan(input,out_index, fstride, twiddles, m)
	local i0,i1,i2,i3,i4 = out_index,out_index+m,out_index+2*m,out_index+3*m,out_index+4*m
	local scratch = {}
	local ya,yb = twiddles[1+fstride*m],twiddles[1+fstride*2*m]
	for u = 0,m-1 do
		scratch[0] = input[i0]

		scratch[1] = complex_mult(input[i1], twiddles[1+u*fstride])
		scratch[2] = complex_mult(input[i2], twiddles[1+2*u*fstride])
		scratch[3] = complex_mult(input[i3], twiddles[1+3*u*fstride])
		scratch[4] = complex_mult(input[i4], twiddles[1+4*u*fstride])

		scratch[7] = complex_add(scratch[1], scratch[4])
		scratch[8] = complex_add(scratch[2], scratch[3])
		scratch[9] = complex_sub(scratch[2], scratch[3])
		scratch[10] = complex_sub(scratch[1], scratch[4])

		input[i0].real = input[i0].real + scratch[7].real + scratch[8].real
		input[i0].imag = input[i0].imag + scratch[7].imag + scratch[8].imag

		scratch[5] = { real = scratch[0].real + scratch[7].real*ya.real + scratch[8].real*yb.real,
							imag = scratch[0].imag + scratch[7].imag*ya.real + scratch[8].imag*yb.real }

		scratch[6]	=	{ real = scratch[10].imag*ya.imag + scratch[9].imag*yb.imag,
							  imag = -1* scratch[10].real*ya.imag + scratch[9].real*yb.imag }

		input[i1] = complex_sub(scratch[5], scratch[6])
		input[i4] = complex_add(scratch[5], scratch[6])

		scratch[11] =	{ real = scratch[0].real + scratch[7].real*yb.real + scratch[8].real*ya.real,
							  imag = scratch[0].imag + scratch[7].imag*yb.real + scratch[8].imag*ya.real }

		scratch[12] = { real = -1* scratch[10].imag*yb.imag + scratch[9].imag*ya.imag,
							 imag = scratch[10].real*yb.imag - scratch[9].real*ya.imag }

		input[i2] = complex_add(scratch[11], scratch[12])
		input[i3] = complex_sub(scratch[11], scratch[12])

		i0=i0+1
		i1=i1+1
		i2=i2+1
		i3=i3+1
		i4=i4+1

	end

end
function butterfly5(input,out_index, fstride, twiddles, m, inverse)
	local i0,i1,i2,i3,i4 = out_index,out_index+m,out_index+2*m,out_index+3*m,out_index+4*m
	local scratch = {}
	local tw = twiddles
	local ya,yb = tw[1+fstride*m],tw[1+fstride*2*m]
	for u = 0,m-1 do
		scratch[0] = input[i0]

		scratch[1] = input[i1] * tw[1+u*fstride]
		scratch[2] = input[i2] * tw[1+2*u*fstride]
		scratch[3] = input[i3] * tw[1+3*u*fstride]
		scratch[4] = input[i4] * tw[1+4*u*fstride]

		scratch[7] = scratch[1] + scratch[4]
		scratch[8] = scratch[2] + scratch[3]
		scratch[9] = scratch[2] - scratch[3]
		scratch[10] = scratch[1] - scratch[4]

		input[i0][1] = input[i0][1] + scratch[7][1] + scratch[8][1]
		input[i0][2] = input[i0][2] + scratch[7][2] + scratch[8][2]

		scratch[5] = 	complex.new(	scratch[0][1] + scratch[7][1]*ya[1] + scratch[8][1]*yb[1],
										scratch[0][2] + scratch[7][2]*ya[1] + scratch[8][2]*yb[1])

		scratch[6]	=	complex.new(	scratch[10][2]*ya[2] + scratch[9][2]*yb[2],
										-1* scratch[10][1]*ya[2] + scratch[9][1]*yb[2])

		input[i1] = scratch[5] - scratch[6]
		input[i4] = scratch[5] + scratch[6]

		scratch[11] =	complex.new( 	scratch[0][1] + scratch[7][1]*yb[1] + scratch[8][1]*ya[1],
										scratch[0][2] + scratch[7][2]*yb[1] + scratch[8][2]*ya[1])

		scratch[12] =	complex.new( 	-1* scratch[10][2]*yb[2] + scratch[9][2]*ya[2],
										scratch[10][1]*yb[2] - scratch[9][1]*ya[2])

		input[i2] = scratch[11] + scratch[12]
		input[i3] = scratch[11] - scratch[12]

		i0=i0+1
		i1=i1+1
		i2=i2+1
		i3=i3+1
		i4=i4+1

	end

end

---------------------------------------------------------------
--Carries out a generic butterfly run of the input sample.
---------------------------------------------------------------
function butterfly_generic_dan(input,out_index, fstride, twiddles, m, p )
	local norig = #input

	for u = 0,m-1 do
		local k = u
		for q1 = 0,p-1 do
			scratchbuf[q1] = input[out_index+k]
			k = k + m
		end

		k = u

		for q1=0,p-1 do
			local twidx = 0
			input[out_index+k] = scratchbuf[0]
			for q=1,p-1 do
				twidx = twidx + fstride*k
				if twidx >= Norix then twidx = twidx - Norig end
				local t = complex_mult(scratchbuf[q], twiddles[1+twidx])
				input[out_index+k] = complex_add(input[out_index+k], t)
			end
			k = k + m
		end
	end
end

function butterfly_generic(input,out_index, fstride, twiddles, m, p, inverse )
	local norig = #input

	for u = 0,m-1 do
		local k = u
		for q1 = 0,p-1 do
			scratchbuf[q1] = input[out_index+k]
			k = k + m
		end

		k = u

		for q1=0,p-1 do
			local twidx = 0
			input[out_index+k] = scratchbuf[0]
			for q=1,p-1 do
				twidx = twidx + fstride*k
				if twidx >= Norix then twidx = twidx - Norig end
				local t = scratchbuf[q] * twiddles[1+twidx]
				input[out_index+k] = input[out_index+k] + t
			end
			k = k + m
		end
	end
end

function _init()
    test_data = {}
    --for i=0,107 do
    --    add(test_data, 5 * sin(i / 10))
    --end

    --for i=0,53 do
    --for i=0,2047 do
    --    add(test_data, 0.5 + 0.5 * sin(i / 100))
    --end

    --size = 4

    --for i=1,size do
    --     local to_add = {}
    --     to_add.real = -sin((i-1)/(5 * 3.141 * 2))
    --     to_add.imag = 0
    --    add(test_data, to_add)
    --    print(to_add.real)
    --end

    --print(#test_data)

    --local result = luafft.fft(test_data, false)

    ----cls(0)
    --for i=1,(#result/4)+1 do
    --  local c = result[i]
    --    --pset(i / 30, 120 - 0.125 * c:abs(), 7)
    --    local val = complex_abs(c) / (#test_data)
    --    --print(c)
    --    print(val)
    --    pset(i, 120 - val, 7)
    --    --print(c.abs)
    --    --print(c)
    --end

    size = 256

    for i=1,size do
         local to_add = {}
         local val = 0
         --val =  -sin((i-1)/(5 * 3.141 * 2))

         if flr(i/32) % 2 == 0 then
            val =  1
         end

         to_add.real = val
         to_add.imag = 0
        add(test_data, to_add)
    end

    local result = luafft.fft(test_data, false)

    cls(0)
    for i=1,(#result/4)+1 do
    --for i=1,(#result/4)+1 do
      local c = result[i]
        --pset(i / 30, 120 - 0.125 * c:abs(), 7)
        local val = complex_abs(c) / (#test_data)
        --print(c)
        --print(val)
        pset(i, 80 - 80 * val, 7)
        --print(c.abs)
        --print(c)
    end
end

__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

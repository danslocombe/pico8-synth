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

---------------------------------------------------------------
--This is a lua port of the KissFFT Library by Mark Borgerding
--It provides a simple function to carry out a fast fourier transformation (FFT).

local luafft = {}

---------------------------------------------------------------
-- Returns the next possible size for data input
--
--@param n    Size
--
--@return    Next fast size.
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
--@param input        A set of points that will be transformed.
--                    At this point, the input has to be a list of complex numbers,
--                    according to the format in complex.lua.
--@param inverse    Boolean that controls whether a transformation
--                    or inverse transformation will be carried out.
--@return            Returns a list of complex numbers with the same size
--                    as the input list. Contains the fourier transformation of the input.
---------------------------------------------------------------
function luafft.fft(input, inverse)
    --the size of input defines the number of total points
    local num_points = #input

    local nps = next_possible_size(#input)
    print(nps)
    assert(#input == nps, string.format("The size of your input is not correct. For your size=%i, use a table of size=%i with zeros at the end.", #input, nps))

    local twiddles = {}
   -- DAN MOD
   num_dan = num_points / 2
    for i = 0,num_dan - 1 do
        local phase = 1 * i / num_dan
        if inverse then phase = phase * -1 end
      local twiddle_real = cos(phase)
      local twiddle_imaginary = sin(phase)
        twiddles[1+i] = {real = twiddle_real, imag = twiddle_imaginary}
    end

    local factors = calculate_factors(num_dan)
    local output = {}
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
    local last = out_index + p*m
    local beg = out_index

   --print("work m=" .. m)

    if m == 1 then
        repeat
            output[out_index] = input[f]
            f = f + fstride*in_stride
            out_index = out_index +1
        until out_index == last
    else
        repeat
            work(input, output,out_index,  f, factors, factors_index, twiddles, fstride*p, in_stride, inverse)
            f = f + fstride*in_stride
            out_index = out_index + m
        until out_index == last
    end

    out_index = beg

    if p == 2 then             butterfly2_dan(output,out_index, fstride, twiddles, m)
    elseif p == 3 then         butterfly3_dan(output,out_index, fstride, twiddles, m)
    elseif p == 4 then         butterfly4_dan(output,out_index, fstride, twiddles, m)
    elseif p == 5 then     butterfly5_dan(output,out_index, fstride, twiddles, m)
    else                     butterfly_generic_dan(output,out_index, fstride, twiddles, m, p) end
end


---------------------------------------------------------------
---devides a number into a sequence of factors
--
--@param num_points    Number of points that are used.
--
--@return        Returns a list with the factors
---------------------------------------------------------------
function calculate_factors(num_points)

  local buf = {}
  local p = 4

  floor_sqrt = flr( sqrt( num_points) )
  local n = num_points
  repeat
    while n%p > 0 do
      if         p == 4 then p = 2
      elseif     p == 2 then p = 3
      else                     p = p + 2 end

      if p > floor_sqrt then p = n end
    end
    n = n / p
    add(buf, p)
    add(buf, n)
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

    repeat
        scratch[0] = complex_mult(input[i+m],twiddles[ti1])
        scratch[1] = complex_mult(input[i+m2],twiddles[ti2])
        scratch[2] = complex_mult(input[i+m3],twiddles[ti3])

        scratch[5] = complex_sub(input[i],scratch[1])
        input[i] = complex_add(input[i],scratch[1])

        scratch[3] = complex_add(scratch[0],scratch[2])
        scratch[4] = complex_sub(scratch[0],scratch[2])

        input[i+m2] = complex_sub(input[i],scratch[3])

        ti1 = ti1 + fstride
        ti2 = ti2 + fstride*2
        ti3 = ti3 + fstride*3
        input[i] = complex_add(input[i],scratch[3])

      input[i+m].real = (scratch[5].real + scratch[4].imag)
      input[i+m].imag = (scratch[5].imag - scratch[4].real)

      input[i+m3].real = (scratch[5].real - scratch[4].imag)
      input[i+m3].imag = (scratch[5].imag + scratch[4].real)

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

        scratch[6]    =    { real = scratch[10].imag*ya.imag + scratch[9].imag*yb.imag,
                              imag = -1* scratch[10].real*ya.imag + scratch[9].real*yb.imag }

        input[i1] = complex_sub(scratch[5], scratch[6])
        input[i4] = complex_add(scratch[5], scratch[6])

        scratch[11] =    { real = scratch[0].real + scratch[7].real*yb.real + scratch[8].real*ya.real,
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

function _init()
    test_data = {}
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
      local c = result[i]
        local val = complex_abs(c) / (#test_data)
        --print(val)
        pset(i, 80 - 80 * val, 7)
    end
end

__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

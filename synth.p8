pico-8 cartridge // http://www.pico-8.com
version 34
__lua__

t = 0

--      2^i/12
scale = {1.0,1.059,1.122,1.189,1.26,1.335,1.414,1.498,1.587,1.682,1.782,1.888}

pat_a    = "55_::_5_55_333__" .. "33_88_3_33_111__" .. "11_66_1_111_000_" .. "55555555________"
pat_bass = "BB_B__6_DD_D__8_" .. "AA_A__=_:_=_<_:_" .. "<<_<__=_:_=_<_:_" .. "==_=__=___<_A___"
pat_b    = "::_==_:_::_888__" .. "88_<<_8_88_555__" .. "66_::_6_666_333_" .. "55555555________"

octave = 1
wave_harshness_score = 0

bg_col = 4
foreground_col = 15
hover_col = 9
--bg_col = 8
--oreground_col = 13

screenshake_t = 0
screenshake_mag = 2
force_reset = false

function dump_noise(mag)
  local screen_start = 0x6000
  local screen_size = 8000
  for i=1,mag * 30 do
    local len = 50 + rnd(100)
    local pos = rnd(screen_size) + screen_start
    len = min(len, screen_start + screen_size - pos)
    memset(pos, rnd(64), len)
  end
end

function read_pat(pat, t_pat)
  local note_mult = 1
  local pat_val = ord(pat, t_pat + 1) - 48
  if pat_val < 24 then
    if pat_val >= 12 then
      note_mult *= 2
      pat_val -= 12
    end

    note_mult *= scale[pat_val + 1]
    return note_mult
  end

  return -1
end

canvas_xoff = 16
canvas_yoff = 6
canvas_w = 64
canvas_h = canvas_w
in_canvas = false

-- enable cursor
poke(0x5F2D, 1)

canvas = {}

for i = 0,canvas_w do
    add(canvas, 0.75 * sin(i/canvas_w))
end

function clamp(val, a, b)
    return min(max(val, a), b)
end

canvas_prev_mouse_x = -1
canvas_prev_mouse_y = -1

function tick_canvas()
    local mouse_x = (stat(32) - canvas_xoff) / canvas_w
    local mouse_y = (stat(33) - canvas_yoff) / canvas_h
    local mouse_pressed = band(0x1, stat(34)) != 0

    in_canvas = mouse_x > 0 and mouse_x < 1 and mouse_y > 0 and mouse_y < 1

    local mx = canvas_prev_mouse_x
    local my = canvas_prev_mouse_y
    local incr_count = abs(mouse_x - canvas_prev_mouse_x) * canvas_w

    local dmx = 1
    if mouse_x < canvas_prev_mouse_x then
        dmx = -1
    end
    dmx /= canvas_w

    local dmy = (mouse_y - canvas_prev_mouse_y) / incr_count

    if mouse_pressed then
        for i=0,incr_count do
            ic = mx > 0 and mx < 1 and my > 0 and my < 1
            if ic then
                local my_clamped = clamp(my, 0, 1)
                local pos = flr(mx * canvas_w)
                local x = 2*(my_clamped - 0.5)
                canvas[pos] = x
                screenshake_t = flr(sqrt(wave_harshness_score))
                screenshake_mag = 0.35 * sqrt(wave_harshness_score)
            end
            mx += dmx
            my += dmy
        end
    end

    canvas_prev_mouse_x = mouse_x
    canvas_prev_mouse_y = mouse_y

    --print(mouse_x .. " " .. mouse_y, 10, 10, foreground_col)
end

function draw_canvas()
    line(canvas_xoff, canvas_yoff + canvas_h / 2)
    for i,x in pairs(canvas) do
        yoff = sin(0.5 * (time() + i * 0.12356) * 10)
        line(canvas_xoff + i - 1,
             canvas_yoff + yoff + canvas_h / 2 + x * canvas_h / 2, foreground_col)
    end
end


function create_button(text, x, y, click_fn)
    return {
        text = text,
        x = x,
        y = y,
        width = 3 + #text * 4,
        height = 8,
        mouse_in = false,
        click_fn = click_fn,
        tick = function(self)
            local mouse_x = stat(32)
            local mouse_y = stat(33)
            self.mouse_in
                  = mouse_x > self.x
                and mouse_x < (self.x + self.width) 
                and mouse_y > self.y
                and mouse_y < (self.y + self.height)

            if self.mouse_in then
                if left_click_pressed then
                    self:click_fn()
                end
            end
        end,
        draw = function(self)
            local col = foreground_col
            if (self.mouse_in) then
                col = hover_col
            end
            rect(self.x, self.y, self.x+self.width, self.y+self.height, col)
            print(self.text, self.x + 2, self.y + 2, col)
        end,
    }
end

buttons = {}
add(buttons, create_button("o1", 90, 8, function(self) octave = 0.5 screenshake_t = 2 * wave_harshness_score screenshake_mag = 0.4 * wave_harshness_score force_reset = true end))
add(buttons, create_button("o2", 90, 18, function(self) octave = 1 screenshake_t = 2 * wave_harshness_score screenshake_mag = 0.25 * wave_harshness_score force_reset = true end))
add(buttons, create_button("o3", 90, 28, function(self) octave = 2 screenshake_t = 2 * wave_harshness_score screenshake_mag = 0.125 * wave_harshness_score force_reset = true end))
add(buttons, create_button("reset", 90, 40, function(self)
    extcmd("reset")
end))
add(buttons, create_button("record", 90, 50, function(self)
    if not is_recording then
        extcmd("audio_rec")
        is_recording = true
        self.text = "save"
    else
        extcmd("audio_end")
        is_recording = false
        self.text = "record"
        saved_t = 90
    end
end))

left_clicked_previous = false
left_click_pressed = false

is_recording = false
saved_t = 0
spectro_y = 100
spectro_height = 28

function _update60()

    if screenshake_t > 0 then
        --print(screenshake_mag, 7)
        screenshake_t -= 1
        if rnd() < screenshake_mag then
            local theta = rnd()
            camera(screenshake_mag * cos(theta), screenshake_mag * sin(theta))
            if rnd() < 0.1 * screenshake_mag then
                dump_noise(0.1)
            end
        else
            camera(0,0)
        end
    else
        camera(0,0)
    end
    --cls()
    local mouse_pressed = band(0x1, stat(34)) != 0
    left_click_pressed = mouse_pressed and not left_clicked_previous
    left_clicked_previous = mouse_pressed

    tick_canvas()

    for i,o in pairs(buttons) do
        o:tick()
    end

    local to_buffer = 2048 - stat(108)
    local buffersize = 256
    local draw_incr = 512 / (8*buffersize)
    if force_reset or to_buffer > buffersize then
        cls(bg_col)
        samples = {}
        force_reset = false
        for i=0,buffersize do
            t += 0.1
            local t_pat = (t\80) % 64
            local wave = 0

            local note_mult_a = 0.5 -- read_pat(pat_a, t_pat)
            if (note_mult_a > 0) then
                local desired_input = t * 0.5 * #canvas * note_mult_a * octave
                local index = flr(desired_input % #canvas)
                wave += canvas[1 + index]
                --local input_sin = t * 0.5 * note_mult_a
                --wave += (sin(input_sin)) * 0.5
                --wave += (sin(t * 0.5 * note_mult_a)) * 0.5
            end


            if wave > 1 then
            wave = 1
            elseif wave < -1 then
            wave = -1
            end

            if i == 0 then
                line(16, spectro_y, 16, spectro_y, foreground_col)
            elseif i % 8 == 0 then
                line(16+draw_incr * i, spectro_y + wave * spectro_height, foreground_col)
            end
            add(samples, wave)
            poke(0x4300 + i, (0.5 + 0.5*wave)*255)
        end

        serial(0x808, 0x4300, buffersize)

        calculate_draw_fft(samples)
    end

end

function _draw()
    if is_recording then
        circfill(02,11,2, 8)
        print("record", 06, 10, foreground_col)
    end

    local t = time()
    --local scale = 1.5 + 0.0125 * sin(t * 0.123)
    local scale = 1
    local face_x = 95 + 2*sin(t * 0.32)
    local face_y = 72 + 3*sin(t * 0.23)

    if (screenshake_t > 0) then
        if (sin(t) < 0.5) then
            pal({[15]=3})
        else
            pal({[15]=2})
        end
        local a = rnd()
        xoff = cos(a)
        yoff = sin(a)
        sspr(40, 0, 24, 32, face_x + xoff, face_y + yoff, 24 * scale, 32 * scale)
        pal()
    end

    sspr(40, 0, 24, 32, face_x, face_y, 24 * scale, 32 * scale)

    face_id = clamp(flr(wave_harshness_score - 1), 0, 3)

    if (rnd() < 0.5) then
        pal({[15]=3})
    else
        pal({[15]=2})
    end
    local a = rnd()
    xoff = cos(a)
    yoff = sin(a)
    sspr(40 + 32, 8*face_id, 24, 8, xoff + face_x + 5 * scale, yoff + face_y + 23 * scale, 24 * scale, 8 * scale)
    pal()
    sspr(40 + 32, 8*face_id, 24, 8, face_x + 5 * scale, face_y + 23 * scale, 24 * scale, 8 * scale)

    eye_id = 0
    if (t * 10 + wave_harshness_score * 0.2) % 10 + wave_harshness_score * 0.2 < 10 then
        eye_id = flr(t * 0.01) % 3 + 1
    end
    sspr(0, 8 * eye_id, 8, 8, face_x + 14, face_y + 14, 8 * scale, 8 * scale)
    sspr(0, 8 * eye_id, 8, 8, face_x + 2, face_y + 14, 8 * scale, 8 * scale)

    for i,o in pairs(buttons) do
        o:draw()
    end

    sspr(12*8, 0, 32, 8, 88, 110, 32, 8)

    line(canvas_xoff, canvas_yoff, canvas_xoff + canvas_w, canvas_yoff, foreground_col)
    line(canvas_xoff, canvas_yoff + canvas_h, canvas_xoff + canvas_w, canvas_yoff + canvas_h, foreground_col)

    line(canvas_xoff, canvas_yoff, canvas_xoff, canvas_yoff + canvas_h, foreground_col)
    line(canvas_xoff + canvas_w, canvas_yoff, canvas_xoff + canvas_w, canvas_yoff + canvas_h, foreground_col)

    line(canvas_xoff, canvas_yoff + canvas_h/2, canvas_xoff + canvas_w, canvas_yoff + canvas_h/2, foreground_col)
    draw_canvas()
    if in_canvas then
        palt(3, true)
        spr(2, stat(32), stat(33))
        palt(3, false)
    else
        spr(1, stat(32), stat(33))
    end

    if saved_t > 0 then
        rectfill(00, 10, 128, 20, bg_col)
        print("saved audio to desktop", 10, 10, foreground_col)
        saved_t -= 1
    end
end

fft_display_x = 20
fft_display_y = 80
fft_display_height = 50
fft_display_width = 50

central_freq = 0
max_freq_val = 0

function calculate_draw_fft(samples)

    -- heuristic taken from empirical measurements 
    wave_harshness_score = 0

    local buffer_size = 512 
    local buffer = {}
    for i = 1,buffer_size do
        add(buffer, {real = samples[i], imag = 0})
    end
    local result = fft(buffer, false)
    line(fft_display_x, fft_display_y, fft_display_x, fft_display_y, foreground_col)
    local to_draw = #result / 8

    -- set central_freq so we can compute wave_harshness_score
    -- deliberately dont allow central_freq=0, so start i=1
    max_freq_val = 0
    for i=1,to_draw do
        local c = result[i + 1]
        local val = complex_abs(c)
        if (val > max_freq_val) then
            max_freq_val = val
            central_freq = i
        end
    end

    for i=0,to_draw do
        local c = result[i + 1]
        local val = complex_abs(c) / buffer_size

        local dist = abs(i - central_freq)
        wave_harshness_score += dist * val 

        --print(val)
        --pset(i, 80 - 80 * val, 7)
        line(fft_display_x + (i / to_draw) * fft_display_width, fft_display_y - val * fft_display_height, foreground_col)
    end

    --print("approx_time " .. central_freq, 10, 12, 7)
    --print("max_i " .. max_i .. " max_val " .. max_val, 10, 5, 7)
    --print("score " .. wave_harshness_score, 10, 20, 7)
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
function fft(input, inverse)
    --the size of input defines the number of total points
    local num_points = #input

    local nps = next_possible_size(#input)
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

function work(input, output, out_index, f, factors, factors_index, twiddles, fstride, in_stride, inverse)
    local p = factors[factors_index]
    local m = factors[factors_index+1]
    factors_index = factors_index + 2
    local last = out_index + p*m
    local beg = out_index

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

__gfx__
44ffff4490000000ff333333d000000000000000000000000000000000fff0000000000000000000000000000000000000000000000000000000000000000000
4ffffff499000000f9a33333dd00000000000000000000000000000000f0f00000000000000000000000000000000000fff00000000000000000000000000000
ff4444ff99900000399a3333ddd0000000000000000000000000000000fff00000000000000000000000000000000000f0f00000000000000000000f00000000
f444444f999900003399a333dddd0000000000000000000000000000000f00000000000000ffffffffffff0000000000fff00000000000000000000f00f00000
444444449999900033399633ddddd000000000000000000000000000000f000000000000000000000000000000000000f00f0ff0fff0ff0f0f0fff0ff0f00000
0000000099990000333366e3dddd0000000000000000000000000000000f000000000000000000000000000000000000f00f0f00f0f0f00f0f0f0f0f00fff000
000000009099000033333ee3d0dd0000000000000000000000000000000f000000000000000000000000000000000000f00f0ff0fff00f0fff0f0f0f00f0f000
000000000009900033333333000dd000000000000000000000000000000f000000000000000000000000000000000000000000000000ff000f00000000000000
44ffff44400000000000000000000000000000000ffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000
4ffffff440000000000000000000000000000000ff444444444444444444f44f0000000000000000000000000000000000000000000000000000000000000000
4ffffff440000000000000000000000000000000f4f444444444444444444f4f00000000000000ffff0000000000000000000000000000000000000000000000
4ffffff440000000000000000000000000000000f44f444444444444444444ff0000000000fffffffffff0000000000000000000000000000000000000000000
4ffffff440000000000000000000000000000000ff444444444444444444f44f00000000000000ffff0000000000000000000000000000000000000000000000
0ffffff000000000000000000000000000000000f4f444444444444444444f4f0000000000000000000000000000000000000000000000000000000000000000
00fffff000000000000000000000000000000000f44f444444444444444444ff0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000f4444444444444444444444f0000000000000000000000000000000000000000000000000000000000000000
444444440000000000000000f440000000000000ff444444444444444444f44f0000000000000000000000000000000000000000000000000000000000000000
4444444400000000000000004f40000000000000f4f444444444444444444f4f000000000000fffffffff0000000000000000000000000000000000000000000
44444444000000000000000044f0000000000000f44f444444444444444444ff000000000ffffffffffffff00000000000000000000000000000000000000000
ffffffff0000000000000000f440000000000000ff444444444444444444f44f00000000ffffffffffffffff0000000000000000000000000000000000000000
4444444400000000000000004f40000000000000f4f44444444444f444444f4f00000000ffffff0000ffffff0000000000000000000000000000000000000000
00000000000000000000000044f0000000000000f44f44444444444f444444ff00000000ffffffffffffffff0000000000000000000000000000000000000000
0000000000000000000000000000000000000000ff44444444444444f444444f00000000ffffffffffffffff0000000000000000000000000000000000000000
0000000000000000000000000000000000000000f4f44f44444444f44444f44f0000000000000000000000000000000000000000000000000000000000000000
4444444400000000000000000000000000000000f44f44f44f44444f444f444f000000000ffffffffffffff0000000000ffffffffffffff00000000000000000
4fffff4400000000000000000000000000000000ff444f4444f44444f444f4ff00000000ffffffffffffffff00000000ffffffffffffffff0000000000000000
ffffffff00000000000000000000000000000000f4f444f4444f44444f444f4f00000000fff0ff0ff0ff0fff00000000fff0ff0ff0ff0fff0000000000000000
ffffffff00000000000000000000000000000000f44f444f4f44444444ff444f00000000fff0000000000fff00000000fff0000000000fff0000000000000000
4444444400000000000000000000000000000000ff444f4444f44444f444f4ff00000000fff0000000000fff00000000fff0000000000fff0000000000000000
0000000000000000000000000000000000000000f4f444f4444f44444f444f4f00000000fff0ff0ff0ff0fff00000000fff0ff0ff0ff0fff0000000000000000
0000000000000000000000000000000000000000f44f444f4444444444f4444f00000000ffffffffffffffff00000000ffffffffffffffff0000000000000000
00000000000000000000000000000000000000000fffffffffffffffffffffff000000000ffffffffffffff0000000000ffffffffffffff00000000000000000
__label__
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f444ffffffffffff4444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f444f4444444444f4444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f444f44ff4ff444f4444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f444f4f4f44f444f4444444444444444
44444444444444444444444444444444f4444444444444fffff444444444444444444444444444444444444444444444f444f4f4f44f444f4444444444444444
44444444444444444444444444444444f44444444444ff44444ff4444444444444444444444444444444444444444444f444f4f4f44f444f4444444444444444
44444444444444444444444444444444f4444444444f444444444f444444444444444444444444444444444444444444f444f4ff44fff44f4444444444444444
44444444444444444444444444444444f4444444444f4444444444f44444444444444444444444444444444444444444f444f4444444444f4444444444444444
44444444444444444444444444444444f444444444f44444444444f44444444444444444444444444444444444444444f444ffffffffffff4444444444444444
44444444444444444444444444444444f44444444f4444444444444f4444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f44444444f44444444444444f444444444444444444444444444444444444444f444ffffffffffff4444444444444444
44444444444444444444444444444444f4444444f444444444444444f444444444444444444444444444444444444444f444f4444444444f4444444444444444
44444444444444444444444444444444f444444f44444444444444444f44444444444444444444444444444444444444f444f44ff4fff44f4444444444444444
44444444444444444444444444444444f444444f444444444444444444f4444444444444444444444444444444444444f444f4f4f444f44f4444444444444444
44444444444444444444444444444444f44444f4444444444444444444f4444444444444444444444444444444444444f444f4f4f4fff44f4444444444444444
44444444444444444444444444444444f44444f44444444444444444444f444444444444444444444444444444444444f444f4f4f4f4444f4444444444444444
44444444444444444444444444444444f4444f444444444444444444444f444444444444444444444444444444444444f444f4ff44fff44f4444444444444444
44444444444444444444444444444444f4444f4444444444444444444444f44444444444444444444444444444444444f444f4444444444f4444444444444444
44444444444444444444444444444444f444f44444444444444444444444f44444444444444444444444444444444444f444ffffffffffff4444444444444444
44444444444444444444444444444444f444f44444444444444444444444f44444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f44f4444444444444444444444444f4444444444444444444444444444444444f444ffffffffffff4444444444444444
44444444444444444444444444444444f44f4444444444444444444444444f4444444444444444444444444444444444f444f4444444444f4444444444444444
44444444444444444444444444444444f44f44444444444444444444444444f444444444444444444444444444444444f444f44ff4fff44f4444444444444444
44444444444444444444444444444444f4f444444444444444444444444444f444444444444444444444444444444444f444f4f4f444f44f4444444444444444
44444444444444444444444444444444f4f4444444444444444444444444444f44444444444444444444444444444444f444f4f4f44ff44f4444444444444444
44444444444444444444444444444444ff44444444444444444444444444444f44444444444444444444444444444444f444f4f4f444f44f4444444444444444
44444444444444444444444444444444ff44444444444444444444444444444f44444444444444444444444444444444f444f4ff44fff44f4444444444444444
44444444444444444444444444444444f4444444444444444444444444444444f4444444444444444444444444444444f444f4444444444f4444444444444444
44444444444444444444444444444444fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff444ffffffffffff4444444444444444
44444444444444444444444444444444f44444444444444444444444444444444f444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f44444444444444444444444444444444f44444444444444444444444444444ff4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444f4444444444444444444444444444ff4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444f444444444444444444444444444f4f4444444444444444444444444444444
44444444444444444444444444444444f4444444444444444444444444444444444f44444444444444444444444444f4f4444444444444444444444444444444
44444444444444444444444444444444f4444444444444444444444444444444444f4444444444444444444444444f44f4444444444444444444444444444444
44444444444444444444444444444444f4444444444444444444444444444444444f4444444444444444444444444f44f4444444444444444444444444444444
44444444444444444444444444444444f44444444444444444444444444444444444f44444444444444444444444f444f4444444444444444444444444444444
44444444444444444444444444444444f44444444444444444444444444444444444f44444444444444444444444f444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444f4444444444444444444444f444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444f444444444444444444444f4444f4444444444444444444444444444444
44444444444444444444444444444444f4444444444444444444444444444444444444f44444444444444444444f4444f444ffffffffffffffffffffffffffff
44444444444444444444444444444444f4444444444444444444444444444444444444f4444444444444444444f44444f444f44444444444444444444444444f
44444444444444444444444444444444f44444444444444444444444444444444444444f444444444444444444f44444f444f4fff4fff44ff44ff4fff4ff444f
44444444444444444444444444444444f44444444444444444444444444444444444444f44444444444444444f444444f444f4f4f4f444f444f4f4f4f4f4f44f
44444444444444444444444444444444f444444444444444444444444444444444444444f444444444444444f4444444f444f4ff44ff44f444f4f4ff44f4f44f
44444444444444444444444444444444f4444444444444444444444444444444444444444f44444444444444f4444444f444f4f4f4f444f444f4f4f4f4f4f44f
44444444444444444444444444444444f4444444444444444444444444444444444444444f4444444444444f44444444f444f4f4f4fff44ff4ff44f4f4fff44f
44444444444444444444444444444444f44444444444444444444444444444444444444444f44444444444f444444444f444f44444444444444444444444444f
44444444444444444444444444444444f444444444444444444444444444444444444444444f4444444444f444444444f444ffffffffffffffffffffffffffff
44444444444444444444444444444444f444444444444444444444444444444444444444444f444444444f4444444444f4444444444444444444444444444444
44444444444444444444444444444444f4444444444444444444444444444444444444444444ff44444ff44444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444ff4ff4444444444444f4444444444444444444444444444444
44444444444444444444444444444444f44444444444444444444444444444444444444444444444f444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444f444444444444444444444444444444444444444444444444444444444444444f4444444444444444444444444444444
44444444444444444444444444444444fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff4444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444494444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444499444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444499944444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444499994444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444499999444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444499994444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444494994444444444444444444444444444444444444444444444444444444444444444
4444444444444444444444444444444444f444444444f444444444f4444444499444444444f444444444f444444444f444444444444444444444444444444444
4444444444444444444444444444444444f444444444f444444444f444444444f444444444f444444444f444444444f444444444444444444444444444444444
4444444444444444444444444444444444f444444444f444444444f444444444f444444444f444444444f444444444f444444444444444444444444444444444
444444444444444444444444444444444ff44444444ff44444444ff44444444ff44444444ff44444444ff44444444f4444444444444444444444444444444444
444444444444444444444444444444444ff44444444ff44444444ff44444444ff44444444ff44444444ff44444444f4444444444444444444444444444444444
444444444444444444444444444444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4444444444444444444444444444444444
444444444444444444444444444444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4444444444444444444444444444444444
444444444444444444444444444444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4444444444444444444444444444444444
444444444444444444444444444444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4444444444444444444444444444444444
44444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f444444f44444444444444444444444444444444444
44444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f444444f44444444444444444444444444444444444
44444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f444444f44444444444444444444444444444444444
44444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f444444f44444444444444444444444444444444444
44444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f444444f44444444444444444444444444444444444
4444444444444444444444444444444f4444f44444f444f44444f444f44444f444f44444f444f44444f444f44444f44444444444444444444444444444444444
4444444444444444444444444444444f4444f44444f444f44444f444f44444f444f44444f444f44444f444f44444f44444444444444444444444444444444444
4444444444444444444444444444444f4444f44444f444f44444f444f44444f444f44444f444f44444f444f44444f44444444444444444444444444444444444
4444444444444444444444444444444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
4444444444444444444444444444444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
444444444444444444444444444444f44444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
444444444444444444444444444444f44444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
444444444444444444444444444444f44444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
444444444444444444444444444444444444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
444444444444444444444444444444444444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f4444f444444444444444444444444444444444444
4444444444444444444444444444444444444f444f44444f444f44444f444f44444f444f44444f444f44444f444f444444444444444444444444444444444444
4444444444444444444444444444444444444f444f44444f444f44444f444f44444f444f44444f444f44444f444f444444444444444444444444444444444444
4444444444444444444444444444444444444f444f44444f444f44444f444f44444f444f44444f444f44444f444f444444444444444444444444444444444444
4444444444444444444444444444444444444f444f44444f444f44444f444f44444f444f44444f444f44444f444f444444444444444444444444444444444444
4444444444444444444444444444444444444f444f44444f444f44444f444f44444f444f44444f444f44444f444f444444444444444444444444444444444444
4444444444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f4444444444444444444444444444444444444
4444444444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f4444444444444444444444444444444444444
4444444444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f4444444444444444444444444444444444444
4444444444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f4444444444444444444444444444444444444
4444444444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f4444444444444444444444444444444444444
4444444444444444444444444444444444444f44f444444f44f444444f44f444444f44f444444f44f444444f44f4444444444444444444444444444444444444
44444444444444444444444444444444444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444f4f4444444444444444444444444444444444444
44444444444444444444444444444444444444ff44444444ff44444444ff44444444ff44444444ff44444444ff44444444444444444444444444444444444444
44444444444444444444444444444444444444ff44444444ff44444444ff44444444ff44444444ff44444444ff44444444444444444444444444444444444444
44444444444444444444444444444444444444ff44444444ff44444444ff44444444ff44444444ff44444444ff44444444444444444444444444444444444444
44444444444444444444444444444444444444f444444444f444444444f444444444f444444444f444444444f444444444444444444444444444444444444444
44444444444444444444444444444444444444f444444444f444444444f444444444f444444444f444444444f444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444


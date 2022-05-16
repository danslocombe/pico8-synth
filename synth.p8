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

canvas_xoff = 32
canvas_yoff = 6
canvas_w = 128 - 2*canvas_xoff
canvas_h = canvas_w

-- enable cursor
poke(0x5F2D, 1)

canvas = {}

for i = 0,canvas_w do
    add(canvas, 0.75 * sin(i/canvas_w))
end

function clamp(val, a, b)
    return min(max(val, a), b)
end

function tick_canvas()
    local mouse_x = (stat(32) - canvas_xoff) / canvas_w
    local mouse_y = (stat(33) - canvas_yoff) / canvas_h
    local mouse_pressed = band(0x1, stat(34)) != 0

    if (mouse_pressed and mouse_x > 0 and mouse_x < 1) then
        mouse_y = clamp(mouse_y, 0, 1)
        local pos = flr(mouse_x * canvas_w)
        local x = 2*(mouse_y - 0.5)
        canvas[pos] = x
    end

    --print(mouse_x .. " " .. mouse_y, 10, 10, 7)
end

function draw_canvas()
    line(canvas_xoff, canvas_yoff + canvas_h / 2)
    for i,x in pairs(canvas) do
        line(canvas_xoff + i,
             canvas_yoff + canvas_h / 2 + x * canvas_h / 2, 7)
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
            local col = 7
            if (self.mouse_in) then
                col = 6
            end
            rect(self.x, self.y, self.x+self.width, self.y+self.height, col)
            print(self.text, self.x + 2, self.y + 2, col)
        end,
    }
end

buttons = {}
add(buttons, create_button("o1", 100, 10, function(self) octave = 0.5 end))
add(buttons, create_button("o2", 100, 20, function(self) octave = 1 end))
add(buttons, create_button("o3", 100, 30, function(self) octave = 2 end))
add(buttons, create_button("record", 100, 50, function(self)
    if not is_recording then
        extcmd("audio_rec")
        is_recording = true
        self.text = "stop"
    else
        extcmd("audio_end")
        is_recording = false
        self.text = "record"
    end
end))

left_clicked_previous = false
left_click_pressed = false

is_recording = false
saved_t = 0
spectro_y = 100
spectro_height = 28

function _update60()
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
    if to_buffer > buffersize then
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
                --rectfill(30, 84 - 30, 30 + 512/8, 84 + 30, 0)
                cls()
                line(30, spectro_y, 30, spectro_y, 7)
            elseif i % 8 == 0 then
                line(30+draw_incr * i, spectro_y + wave * spectro_height, 7)
            end
            poke(0x4300 + i, (0.5 + 0.5*wave)*255)
        end

        serial(0x808, 0x4300, buffersize)
    end

end

function _draw()
    if is_recording then
        circfill(02,11,2, 8)
        print("record", 06, 10, 7)
    end

    if saved_t > 0 then
        print("saved audio to desktop", 10, 10)
        saved_t -= 1
    end

    for i,o in pairs(buttons) do
        o:draw()
    end

    line(canvas_xoff, canvas_yoff, canvas_xoff + canvas_w, canvas_yoff, 7)
    line(canvas_xoff, canvas_yoff + canvas_h, canvas_xoff + canvas_w, canvas_yoff + canvas_h, 7)

    line(canvas_xoff, canvas_yoff, canvas_xoff, canvas_yoff + canvas_h, 7)
    line(canvas_xoff + canvas_w, canvas_yoff, canvas_xoff + canvas_w, canvas_yoff + canvas_h, 7)

    line(canvas_xoff, canvas_yoff + canvas_h/2, canvas_xoff + canvas_w, canvas_yoff + canvas_h/2, 7)
    draw_canvas()
    spr(1, stat(32), stat(33))
end
__gfx__
00000000d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000dd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000ddd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000dddd00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000ddddd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000dddd00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000d0dd00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000dd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

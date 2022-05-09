pico-8 cartridge // http://www.pico-8.com
version 34
__lua__

t = 0

--      2^i/12
scale = {1.0,1.059,1.122,1.189,1.26,1.335,1.414,1.498,1.587,1.682,1.782,1.888}

pat_a    = "55_::_5_55_333__" .. "33_88_3_33_111__" .. "11_66_1_111_000_" .. "55555555________"
pat_bass = "BB_B__6_DD_D__8_" .. "AA_A__=_:_=_<_:_" .. "<<_<__=_:_=_<_:_" .. "==_=__=___<_A___"
pat_b    = "::_==_:_::_888__" .. "88_<<_8_88_555__" .. "66_::_6_666_333_" .. "55555555________"

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


function _update60()
    --cls()

    tick_canvas()

    local to_buffer = 2048 - stat(108)
    local buffersize = 256
    local draw_incr = 512 / (8*buffersize)
    if to_buffer > buffersize then
        for i=0,buffersize do
            t += 0.1
            local t_pat = (t\80) % 64
            local wave = 0

            local note_mult_a = read_pat(pat_a, t_pat)
            if (note_mult_a > 0) then
                local desired_input = t * 0.5 * #canvas * note_mult_a
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
                line(30, 90, 30, 90, 7)
            elseif i % 8 == 0 then
                line(30+draw_incr * i, 90 + wave * 30, 7)
            end
            poke(0x4300 + i, (0.5 + 0.5*wave)*255)
        end

        serial(0x808, 0x4300, buffersize)
    end

end

function _draw()
    line(canvas_xoff, canvas_yoff, canvas_xoff + canvas_w, canvas_yoff)
    line(canvas_xoff, canvas_yoff + canvas_h, canvas_xoff + canvas_w, canvas_yoff + canvas_h)

    line(canvas_xoff, canvas_yoff, canvas_xoff, canvas_yoff + canvas_h)
    line(canvas_xoff + canvas_w, canvas_yoff, canvas_xoff + canvas_w, canvas_yoff + canvas_h)

    line(canvas_xoff, canvas_yoff + canvas_h/2, canvas_xoff + canvas_w, canvas_yoff + canvas_h/2)
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

local function sortByTime(a, b) return a.Time < b.Time end

-- Mostly ripped off from https://github.com/Fraktality/anim/blob/master/anim.lua
-- Copyright 2017 Parker Stebbins <parker@fractality.io>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software
-- and associated documentation files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all copies or
-- substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
-- BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
local Lerps = {}

-- bool
function Lerps.boolean(v0, v1, t)
    if t < 0.5 then
        return v0
    else
        return v1
    end
end

-- number
function Lerps.number(v0, v1, t) return v0 + (v1 - v0) * t end

do -- Color3
    local C3 = Color3.new
    local black = C3(0, 0, 0)

    function Lerps.Color3(c0, c1, t)
        local u0, v0, u1, v1, l0, l1

        -- Convert from linear RGB to scaled CIELUV (RgbToLuv13)
        local r, g, b = c0.r, c0.g, c0.b
        -- Apply inverse gamma correction

        if r < 0.0404482362771076 then
            r = r / 12.92
        else
            r = 0.87941546140213 * (r + 0.055) ^ 2.4
        end

        if g < 0.0404482362771076 then
            g = g / 12.92
        else
            g = 0.87941546140213 * (g + 0.055) ^ 2.4
        end

        if b < 0.0404482362771076 then
            b = b / 12.92
        else
            b = 0.87941546140213 * (b + 0.055) ^ 2.4
        end

        -- sRGB->XYZ->CIELUV
        local y = 0.2125862307855956 * r + 0.71517030370341085 * g +
                      0.0722004986433362 * b
        local z = 3.6590806972265883 * r + 11.4426895800574232 * g +
                      4.1149915024264843 * b

        if y > 0.008856451679035631 then
            l0 = 116 * y ^ (1 / 3) - 16
        else
            l0 = 903.296296296296 * y
        end

        if z > 1e-15 then
            u0, v0 = l0 *
                         (0.9257063972951867 * r - 0.8333736323779866 * g -
                             0.09209820666085898 * b) / z,
                     l0 * (9 * y / z - 0.46832)
        else
            u0, v0 = -0.19783 * l0, -0.46832 * l0
        end

        -- Convert from linear RGB to scaled CIELUV (RgbToLuv13)
        r, g, b = c1.r, c1.g, c1.b
        -- Apply inverse gamma correction

        if r < 0.0404482362771076 then
            r = r / 12.92
        else
            r = 0.87941546140213 * (r + 0.055) ^ 2.4
        end

        if g < 0.0404482362771076 then
            g = g / 12.92
        else
            g = 0.87941546140213 * (g + 0.055) ^ 2.4
        end

        if b < 0.0404482362771076 then
            b = b / 12.92
        else
            b = 0.87941546140213 * (b + 0.055) ^ 2.4
        end

        -- sRGB->XYZ->CIELUV
        y = 0.2125862307855956 * r + 0.71517030370341085 * g +
                0.0722004986433362 * b
        z = 3.6590806972265883 * r + 11.4426895800574232 * g +
                4.1149915024264843 * b

        if y > 0.008856451679035631 then
            l1 = 116 * y ^ (1 / 3) - 16
        else
            l1 = 903.296296296296 * y
        end

        if z > 1e-15 then
            u1, v1 = l1 *
                         (0.9257063972951867 * r - 0.8333736323779866 * g -
                             0.09209820666085898 * b) / z,
                     l1 * (9 * y / z - 0.46832)
        else
            u1, v1 = -0.19783 * l1, -0.46832 * l1
        end

        local l = (1 - t) * l0 + t * l1
        if l < 0.0197955 then return black end

        local u = ((1 - t) * u0 + t * u1) / l + 0.19783
        local v = ((1 - t) * v0 + t * v1) / l + 0.46832

        local y = (l + 16) / 116

        if y > 0.206896551724137931 then
            y = y * y * y
        else
            y = 0.12841854934601665 * y - 0.01771290335807126
        end

        local x = y * u / v
        local z = y * ((3 - 0.75 * u) / v - 5)

        local r = 7.2914074 * x - 1.5372080 * y - 0.4986286 * z
        local g = -2.1800940 * x + 1.8757561 * y + 0.0415175 * z
        local b = 0.1253477 * x - 0.2040211 * y + 1.0569959 * z

        if r < 0 and r < g and r < b then
            r, g, b = 0, g - r, b - r
        elseif g < 0 and g < b then
            r, g, b = r - g, 0, b - g
        elseif b < 0 then
            r, g, b = r - b, g - b, 0
        end

        if r < 3.1306684425e-3 then
            r = 12.92 * r
        else
            r = 1.055 * r ^ (1 / 2.4) - 0.055
        end

        if g < 3.1306684425e-3 then
            g = 12.92 * g
        else
            g = 1.055 * g ^ (1 / 2.4) - 0.055
        end

        if b < 3.1306684425e-3 then
            b = 12.92 * b
        else
            b = 1.055 * b ^ (1 / 2.4) - 0.055
        end

        if r < 0 then
            r = 0
        elseif r > 1 then
            r = 1
        end

        if g < 0 then
            g = 0
        elseif g > 1 then
            g = 1
        end

        if b < 0 then
            b = 0
        elseif b > 1 then
            b = 1
        end

        return C3(r, g, b)
    end
end

do -- CFrame
    local Slerp = CFrame.new().lerp
    function Lerps.CFrame(v0, v1, t) return Slerp(v0, v1, t) end
end

do -- NumberRange
    local NR = NumberRange.new
    function Lerps.NumberRange(v0, v1, t)
        local min0, max0 = v0.Min, v0.Max
        local dmin, dmax = v1.Min - min0, v1.Max - max0
        return NR(min0 + t * dmin, max0 + t * dmax)
    end
end

do -- NumberSequenceKeypoint
    local NSK = NumberSequenceKeypoint.new
    function Lerps.NumberSequenceKeypoint(v0, v1, t)
        local t0, v0, e0 = v0.Time, v0.Value, v0.Envelope
        local dt, dv, de = v1.Time - t0, v1.Value - v0, v1.Envelope - e0
        return NSK(t0 + t * dt, v0 + t * dv, e0 + t * de)
    end
end

do -- PhysicalProperties
    local PP = PhysicalProperties.new
    function Lerps.PhysicalProperties(v0, v1, t)
        local d0, e0, ew0, f0, fw0 = v0.Density, v0.Elasticity,
                                     v0.ElasticityWeight, v0.Friction,
                                     v0.FrictionWeight
        local dd, de, dew, df, dfw = v1.Density - d0, v1.Elasticity - e0,
                                     v1.ElasticityWeight - ew0,
                                     v1.Friction - f0, v1.FrictionWeight - fw0
        return PP(d0 + t * dd, e0 + t * de, ew0 + t * dew, f0 + t * df,
                  fw0 + t * dfw)
    end
end

do -- Ray
    local R = Ray.new
    local V3 = Vector3.new
    function Lerps.Ray(v0, v1, t)
        local o0, d0, o1, d1 = v0.Origin, v0.Direction, v1.Origin, v1.Direction
        local ox0, oy0, oz0, dx0, dy0, dz0, dx1, dy1, dz1 = o0.x, o0.y, o0.z,
                                                            d0.x, d0.y, d0.z,
                                                            d1.x, d1.y, d1.z
        local dox, doy, doz, ddx, ddy, ddz = o1.x - ox0, o1.y - oy0, o1.z - oz0,
                                             d1.x - dx0, d1.y - dy0, d1.z - dz0
        return R(V3(ox0 + t * dox, oy0 + t * doy, oz0 + t * doz),
                 V3(dx0 + t * ddx, dy0 + t * ddy, dz0 + t * ddz))
    end
end

do
    newRect = Rect.new

    function Lerps.Rect(v0, v1, t)
        local sc, of = v0.Min.X, v0.Min.Y
        local dsc, dof = v1.Min.X - sc, v1.Min.Y - of
        local sc2, of2 = v0.Max.X, v0.Max.Y
        local dsc2, dof2 = v1.Max.X - sc2, v1.Max.Y - of2

        return newRect(sc + t * dsc, of + t * dof, sc2 + t * dsc2,
                       of2 + t * dof2)
    end
end

do -- UDim
    local UD = UDim.new
    function Lerps.UDim(v0, v1, t)
        local sc, of = v0.Scale, v0.Offset
        local dsc, dof = v1.Scale - sc, v1.Offset - of
        return UD(sc + t * dsc, of + t * dof)
    end
end

do -- UDim2
    local Lerp = UDim2.new().Lerp
    function Lerps.UDim2(v0, v1, t) return Lerp(v0, v1, t) end
end

do -- Vector2
    local V2 = Vector2.new
    function Lerps.Vector2(v0, v1, t)
        local x, y = v0.x, v0.y
        local dx, dy = v1.x - x, v1.y - y
        return V2(x + t * dx, y + t * dy)
    end
end

do
	function Lerps.Vector2int16(v0, v1, t)
        local x, y = v0.x, v0.y
        local dx, dy = v1.x - x, v1.y - y
        return Vector2int16.new(x + t * dx, y + t * dy)
	end
end

do -- Vector3
    local V3 = Vector3.new
    function Lerps.Vector3(v0, v1, t)
        local x, y, z = v0.x, v0.y, v0.z
        local dx, dy, dz = v1.x - x, v1.y - y, v1.z - z
        return V3(x + t * dx, y + t * dy, z + t * dz)
    end
end

do -- ColorSequence
    local newColorSequence = ColorSequence.new
    local Color3Lerp = Lerps.Color3

    function Lerps.ColorSequence(start, finish, alpha)
        local l1 = Color3Lerp(start[1], finish[1])
        local l2 = Color3Lerp(start[2], finish[2])

        return newColorSequence(l1(alpha), l2(alpha))
    end
end

do -- Region3
    local newRegion3 = Region3.new
    local newVector3 = Vector3.new

    function Lerps.Region3(start, finish, alpha) -- @author Sharksie
        local start1 = start.CFrame * (-start.Size * 0.5)
        local start2 = start.CFrame * (start.Size * 0.5)

        local finish1 = finish.CFrame * (-finish.Size * 0.5)
        local finish2 = finish.CFrame * (finish.Size * 0.5)

        local change1 = finish1 - start1
        local change2 = finish2 - start2

        local imin = start1 + alpha * change1
        local imax = start2 + alpha * change2

        local iminx = imin.x
        local imaxx = imax.x

        local iminy = imin.y
        local imaxy = imax.y

        local iminz = imin.z
        local imaxz = imax.z

        local x1, x2
        local y1, y2
        local z1, z2

        if iminx < imaxx then
            x1 = iminx
            x2 = imaxx
        else
            x1 = imaxx
            x2 = iminx
        end

        if iminy < imaxy then
            y1 = iminy
            y2 = imaxy
        else
            y1 = imaxy
            y2 = iminy
        end

        if iminz < imaxz then
            z1 = iminz
            z2 = imaxz
        else
            z1 = imaxz
            z2 = iminz
        end

        return newRegion3(newVector3(x1, y1, z1), newVector3(x2, y2, z2))
    end
end

do
    function Lerps.NumberSequence(start, finish, alpha)
        -- @author Sharksie

        -- For each point on each line, find the values of the other sequence at that point in time through interpolation
        -- 	then interpolate between the known value and the learned value
        -- 	then use that value to create a new keypoint at the time
        -- 	then build a new sequence using all the keypoints generated

        local keypoints = {}
        local addedTimes = {}

        for i, ap in next, start.Keypoints do
            local closestAbove, closestBelow

            for i, bp in next, finish.Keypoints do
                if bp.Time == ap.Time then
                    closestAbove, closestBelow = bp, bp
                    break
                elseif bp.Time < ap.Time and
                    (closestBelow == nil or bp.Time > closestBelow.Time) then
                    closestBelow = bp
                elseif bp.Time > ap.Time and
                    (closestAbove == nil or bp.Time < closestAbove.Time) then
                    closestAbove = bp
                end
            end

            local bValue, bEnvelope
            if closestAbove == closestBelow then
                bValue, bEnvelope = closestAbove.Value, closestAbove.Envelope
            else
                local p = (ap.Time - closestBelow.Time) /
                              (closestAbove.Time - closestBelow.Time)
                bValue = (closestAbove.Value - closestBelow.Value) * p +
                             closestBelow.Value
                bEnvelope =
                    (closestAbove.Envelope - closestBelow.Envelope) * p +
                        closestBelow.Envelope
            end
            local interValue = (bValue - ap.Value) * alpha + ap.Value
            local interEnvelope = (bEnvelope - ap.Envelope) * alpha +
                                      ap.Envelope
            local interp = NumberSequenceKeypoint.new(ap.Time, interValue,
                                                      interEnvelope)

            table.insert(keypoints, interp)

            addedTimes[ap.Time] = true
        end

        for i, bp in next, finish.Keypoints do
            if not addedTimes[bp.Time] then
                local closestAbove, closestBelow

                for i, ap in next, start.Keypoints do
                    if ap.Time == bp.Time then
                        closestAbove, closestBelow = ap, ap
                        break
                    elseif ap.Time < bp.Time and
                        (closestBelow == nil or ap.Time > closestBelow.Time) then
                        closestBelow = ap
                    elseif ap.Time > bp.Time and
                        (closestAbove == nil or ap.Time < closestAbove.Time) then
                        closestAbove = ap
                    end
                end

                local aValue, aEnvelope
                if closestAbove == closestBelow then
                    aValue, aEnvelope = closestAbove.Value,
                                        closestAbove.Envelope
                else
                    local p = (bp.Time - closestBelow.Time) /
                                  (closestAbove.Time - closestBelow.Time)
                    aValue = (closestAbove.Value - closestBelow.Value) * p +
                                 closestBelow.Value
                    aEnvelope =
                        (closestAbove.Envelope - closestBelow.Envelope) * p +
                            closestBelow.Envelope
                end
                local interValue = (bp.Value - aValue) * alpha + aValue
                local interEnvelope = (bp.Envelope - aEnvelope) * alpha +
                                          aEnvelope
                local interp = NumberSequenceKeypoint.new(bp.Time, interValue,
                                                          interEnvelope)

                table.insert(keypoints, interp)
            end
        end

        table.sort(keypoints, sortByTime)

        return NumberSequence.new(keypoints)
    end
end

return function(start_value, end_value, alpha)
	local type = typeof(start_value)
	if not Lerps[type] then error("Unable to Lerp type of " .. type) end
	
	return Lerps[type](start_value, end_value, alpha)
end


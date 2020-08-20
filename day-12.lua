-- https://adventofcode.com/2019/day/12 in Lua.

M1 = {}
M2 = {}
pat = "<x=(%-?%d+), y=(%-?%d+), z=(%-?%d+)>"
for x,y,z in io.read("*all"):gmatch(pat) do
  table.insert(M1, {pos={tonumber(x),tonumber(y),tonumber(z)},vel={0,0,0}})
  table.insert(M2, {pos={tonumber(x),tonumber(y),tonumber(z)},vel={0,0,0}})
end

function cmp(x, y)
  if x < y then return -1
  elseif x > y then return 1
  else return 0 end
end

function simulate()
  -- apply gravity
  for i=1,#M do
    for j=i+1,#M do
      for c=1,3 do
        d = cmp(M[i].pos[c], M[j].pos[c])
        M[i].vel[c] = M[i].vel[c] - d
        M[j].vel[c] = M[j].vel[c] + d
      end
    end
  end

  -- apply velocity
  for i=1,#M do
    for c=1,3 do
      M[i].pos[c] = M[i].pos[c] + M[i].vel[c]
    end
  end
end

-- *: Simulate 1000 steps and compute total energy.
M = M1
for step=1,1000 do
  simulate()
end

energy = 0
for i=1,#M do
  pot = 0
  kin = 0
  for c=1,3 do
    pot = pot + math.abs(M[i].pos[c])
    kin = kin + math.abs(M[i].vel[c])
  end
  energy = energy + pot * kin
end

print("*", energy)

-- **: Find how long the system takes to loop.
-- Strategy: find a cycle in each of the three axes,
-- and take the LCM of the cycle lengths.

M = M2
seen = {{},{},{}}
time = 1
cycle = {false,false,false}
repeat
  for c=1,3 do
    if not cycle[c] then
      p={}
      for i=1,#M do table.insert(p, M[i].pos[c]) end
      for i=1,#M do table.insert(p, M[i].vel[c]) end
      key=table.concat(p, ",")
      if seen[c][key] then
        cycle[c] = time - seen[c][key]
      else
        seen[c][key] = time
      end
    end
  end
  simulate()
  time = time + 1
until cycle[1] and cycle[2] and cycle[3]

function gcd(a, b)
  while b ~= 0 do
    a, b = b, a%b
  end
  return a
end
 
function lcm(a, b)
  return a * b / gcd(a, b)
end

-- Lua's numbers are too floaty for this output.
print("**", lcm(cycle[1], lcm(cycle[2], cycle[3])))

-- I'll just print the formula and compute it elsewhere:
print("\t= lcm(" .. cycle[1] .. ", " .. cycle[2] .. ", " .. cycle[3] .. ")")

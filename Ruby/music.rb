def Rest(d)
  {:Type => :Rest, :Duration => d}
end

def Serial(m1, m2)
  {:Type => :Serial, :P1 => m1, :P2 => m2}
end

def Parallel(m1, m2)
  {:Type => :Parallel, :P1 => m1, :P2 => m2}
end

def Note(p,d)
  {:Type => :Note, :Pitch => p, :Duration => d}
end

def Delay(d, m)
  Serial(Rest(0), m)
end


def line(ms)
  res=nil
  for m in ms.reverse
    if res
      res = Serial(m, res)
    else
      res = Serial(m, Rest(0))
    end
  end
  res
end

def chord(ms)
  res=nil
  for m in ms.reverse
    if res
      res = Parallel(m, res)
    else
      res = Parallel(m, Rest(0))
    end
  end
  res
end


def duration(m)
  t = m[:Type]
  d = 0
  case t
  when :Rest
    d = m[:Duration]
  when :Note
    d = m[:Duration]
  when :Parallel
    d1 = duration(m[:P1])
    d2 = duration(m[:P2])
    if d1 > d2
      d = d1
    else
      d = d2
    end
  when :Serial
    d1 = duration(m[:P1])
    d2 = duration(m[:P2])
    d = d1 + d2
  end
  d
end


def parse(m, s, d, o)
  t = m[:Type]
  dur = 0
  case t

  when :Note
    s.times do 
      print "-"
    end
    print d
    print " "
    puts m[:Pitch]
    o[d]=[] if o[d].nil?
    o[d].push(m)
  when :Serial
    p1 = m[:P1]
    pd1 = duration(p1)
    p2 = m[:P2]
    parse(p1, s, d, o)
    parse(p2, s, d + pd1, o)
  when :Parallel
    p1 = m[:P1]
    p2 = m[:P2]
    parse(m[:P1], s, d, o)
    parse(m[:P2], s + 4, d, o)
  end
end

def perform(o)
  pt = 0
  for i in o.keys.sort
    dd = 0
    if i > pt
#      puts "#{i} #{pt} sleep #{i - pt}"
      puts "sleep #{(i * 1.0) - (pt * 1.0)}"
      pt = i
    end
    for j in o[i]
      t = j[:Type]
      p = j[:Pitch]
      d = j[:Duration]
      if d > dd
        dd = d 
      end
#      puts "i=#{i} pt=#{pt} play #{p}, release: #{d}" if t != :Rest
      puts "play :#{p}, release: #{d}" if t != :Rest
    end
  end
end

def nd2l(ns,ds)
  a = []
  ns.zip(ds).each do |n, d|
    a.push Note(n,d)
  end
  line(a)
end


m1 = nd2l([:C3, :D3, :E3, :F3, :G3, :A3, :B3, :C4], [0.3].cycle)
beats = nd2l(([:A3].cycle.take 20), [0.3].cycle)
m3 = nd2l([:G3, :A3, :B3, :C4, :D4, :E4, :F4, :G4], [0.3].cycle)
mus = Parallel(Parallel(m1,beats), m3)
o = {};
parse(mus, 0, 0, o)
perform(o)

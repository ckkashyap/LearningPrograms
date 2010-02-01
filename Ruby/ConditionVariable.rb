require 'thread'
mutex = Mutex.new
cv = ConditionVariable.new

a = Thread.new {
  mutex.synchronize {
    puts "A: I have critical section, but will wait for cv"
    cv.wait(mutex)
    puts "A: I have critical section again! I rule!"
  }
}

b = Thread.new {
  mutex.synchronize {
    puts "B: I have critical section, but will wait for cv"
    cv.wait(mutex)
    puts "B: I have critical section again! I rule!"
  }
}

puts "(Later, back at the ranch...)"

c = Thread.new {
  mutex.synchronize {
    puts "C: Now I am critical, but am done with cv"
    cv.signal
    puts "C: I am still critical, finishing up"
  }
  mutex.synchronize {
    puts "C: Now I am critical, but am done with cv"
    cv.signal
    puts "C: I am still critical, finishing up"
  }
}


a.join
b.join
c.join

DICT={}

def str2arr(str)
	str.unpack("C*").map! {|x| x-97}
end

print "Enter source word: "
source=gets
source.chomp!
print "Enter destination word: "
destination=gets
destination.chomp!

print "Loading dictionary..."
IO.readlines("words").each do |word|
	word.downcase!
	word.chomp!
	next unless /^[a-z]+$/.match(word)
	next if word.length != source.length
	DICT[word]=1
end
puts "done."


queue=[] # queue of entries

class Entry
	def initialize(w,n,p=nil)
		@word=w.clone
		@distance=n
		@parent=p
	end
	def getDistance
		@distance
	end
	def getWord
		@word
	end
	def getParent
		@parent
	end
end

UsedWordHash=Hash.new

def getOneHopList(node,n)
	list=[]
	word=node.getWord
	length=word.length

	length.times do |i|
		c=word[i]
		('a'..'z').each do |j|
			next if j == c
			word[i]=j
			if DICT[word] == 1
				if UsedWordHash[word].nil?
					UsedWordHash[word]=1
					list.push(Entry.new(word,n,node))
				end
			end
		end
		word[i]=c
	end
	return list
end

queue.push(Entry.new(source,0))

while queue.length > 0 do
	x=queue.shift
	l=x.getDistance
	w=x.getWord
	if w==destination
		p=x.getParent
		solution=[]
		loop do
			solution.unshift(w)
			x=p
			w=x.getWord
			p=x.getParent
			break if p.nil?
		end
		solution.unshift(source)
		puts solution.join(" -> ")
		exit
	end
	list=getOneHopList(x,l+1)
	queue=queue+list
end

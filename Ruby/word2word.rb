class Node
	def initialize
		@exists=false
		@children=Array.new(26)
	end
	def getChildren
		return @children
	end
	def wordExists?
		return @exists
	end
	def wordExists=(e)
		@exists=e
	end
	def Node.addWord(node,arr)
		return if arr.nil?
		if arr.length==0
			node.wordExists=true
		else
			c=arr.shift
			children=node.getChildren
			nextNode=children[c]
			nextNode=Node.new if nextNode.nil?
			children[c]=nextNode
			Node.addWord(nextNode,arr)
		end
	end
	def Node.find(node,arr)
		arr.each do |i|
			node=node.getChildren[i]
			return false if node.nil?
		end
		node.wordExists?
	end

	def Node.print(node,str)
		puts str.pack("C*") if node.wordExists?
		
		children=node.getChildren
		26.times do |i|
			n=children[i]
			next if n.nil?
			str.push(i+97)
			Node.print(n,str)
			str.pop
		end
	end
end

tree=Node.new
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
IO.readlines("/usr/share/dict/words").each do |word|
	word.downcase!
	word.chomp!
	next unless /^[a-z]+$/.match(word)
	next if word.length != source.length
#Node.addWord(tree,str2arr(word))
	DICT[word]=1
end
puts "done."

#loop do
#	print "Enter a word: "
#	w=gets
#	w.chomp!
#	
#	ww=str2arr(w)
#	l=w.length
#	b=Time.now.to_f
#	x1=Node.find(tree,ww)
#	a=Time.now.to_f
#	bt=(a-b)
#
#	b=Time.now.to_f
#	x2=dict[w]
#	a=Time.now.to_f
#	ht=(a-b)
#
#	puts "BTree took #{bt} seconds to get #{x1}"
#	puts "Hashtable took #{ht} seconds to get #{x2}"
#	m=(bt>ht)?"Btree took more time":"HT took more time"
#	puts m
#end


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

AnotherHash=Hash.new

def existsInReturnPath(node,word)
	p=node.getParent
	while !p.nil?
		return true if p.getWord == word
		p=p.getParent
	end
	if AnotherHash[word].nil?
		AnotherHash[word]=1
		return false
	else
		return true
	end
end

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
				list.push(Entry.new(word,n,node)) unless existsInReturnPath(node,word)
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



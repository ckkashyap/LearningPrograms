class Node
	
	def initialize
		@exists=false
		@children=[]
		26.times do |i|
			@children.push(nil)
		end
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
			c=c-97
			children=node.getChildren
			nextNode=children[c]
			nextNode=Node.new if nextNode.nil?
			children[c]=nextNode
			Node.addWord(nextNode,arr)
		end
	end
	def Node.find(node,arr)
		return false if arr.nil?
		c=arr.shift
		return node.wordExists? if c.nil?
		c=c-97
		children=node.getChildren
		nextNode=children[c]
		return false if nextNode.nil?
		return Node.find(nextNode,arr)
	end

	def Node.print(node,str)
		puts str.pack("C*") if node.wordExists?
		
		children=node.getChildren
		26.times do |i|
			n=children[i]
			next if n.nil?
			str.push(i+97)
			Node.print(n,str)
			Wstr.pop
		end
	end
end

tree=Node.new

def str2arr(str)
	str.unpack("C*")
end

#IO.readlines("/home/ckkashyap/words").each do |word|
#	word.downcase!
#	word.chomp!
#	next unless /^[a-z]+$/.match(word)
#	Node.addWord(tree,str2arr(word))
#end


print "Enter source word: "
source=gets
print "Enter destination word: "
destination=gets



queue=[]

class Entry
	def initialize(w,n)
		@w=w.clone
		@n=n
	end
	def getDistance
		@n
	end
	def getWord
		@w
	end
end

def getOneHopList(word,n)
	list=[]
	(word.length-1).times do |i|
		c=word[i]
		('a'..'z').each do |j|
			next if j == c
			word[i]=j
			list.push(Entry.new(word,n))
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
	list=getOneHopList(w,l+1)
end


#puts Node.find(tree,str2arr("hello"))

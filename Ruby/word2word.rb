class Node
	
	def initialize
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
			str.pop
		end
	end
end

tree=Node.new

def str2arr(str)
	str.unpack("C*")
end

Node.addWord(tree,str2arr("abc"))
Node.addWord(tree,str2arr("xyz"))
Node.addWord(tree,str2arr("b"))

Node.print(tree,[])


puts Node.find(tree,str2arr("abc"))


queue=[]


class Node
	def initialize(str,no,nc,n)
		@str=str
		@no=no
		@nc=nc
		@max=n
	end

	def getPossibilities
		arr=[]
		if @nc < @no
			n=Node.new(@str+")",@no,@nc+1,@max)
			arr.push(n)
		end

		if @no < @max
			n=Node.new(@str+"(",@no+1,@nc,@max)
			arr.push(n)
		end
		return arr
	end

	def print
		puts @str
	end
	def string
		@str
	end
end


size=ARGV[0]
n=Node.new("",0,0,size.to_i)
size=size.to_i*2
queue.push(n)

while !queue.empty?
	x=queue.shift
	str=x.string
	puts str if str.length == size
	arr=x.getPossibilities
	queue=queue+arr
end

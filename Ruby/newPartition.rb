
class Node
	def Node.set(n)
		@@set=[]
		n.times do |i|
			break if i==n
			next if i==0
			@@set.push i
		end
		@@max=n
	end

	def initialize(a,s)
		@arr=a
		@sum=s
	end

	def sum
		@sum
	end

	def getPossibilities
		arr=[]
		@@set.each do |e|
			if (@sum + e)<=@@max
				n=Node.new(@arr.clone.push(e),@sum+e)
				arr.push(n)
			end
		end
		return arr
	end

	def print
		puts @arr.join("+")
	end

end

N=5
Node.set(N)

x=Node.new([],0)
queue=[]
queue.push(x)

while ! queue.empty?
	x=queue.shift
	x.print if x.sum==N
	l=x.getPossibilities
	queue=queue+l
end

def part(list,sum)
        if sum==0
                puts list.join("+")
                return
        end
        if list.size==0
        n=1
        else
        n=list.size-1
        end
        for i in (n..sum).to_a
                list.push(i)
                part(list,sum-i)
                list.pop
        end
end

part(Array.new,5)

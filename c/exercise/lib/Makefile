help:
	@echo 
	@echo "	You have the following choices:"
	@echo "	    make test_q"
	@echo "	    make test_ll"
	@echo "	    make clean"
	@echo 


all: test_q test_ll

%.o: %.c
	gcc -g3 -c -I . $<  -o $@


test_q : test_q.o queue.o linkedlist.o
	gcc $^ -o $@

	
test_ll: test_ll.o queue.o linkedlist.o
	gcc $^ -o $@

clean:
	rm -f *.o test_ll test_q

TARGETS = test_q test_ll test_bst test_zigzag

help:
	@echo 
	@echo "	You have the following choices:"
	@for i in $(TARGETS); do echo "	make $$i"; done
	@echo "	make clean"
	@echo 


all: $(TARGETS)

%.o: %.c
	gcc -g3 -c -I . $<  -o $@


test_q : test_q.o queue.o linkedlist.o
	gcc $^ -o $@

	
test_ll: test_ll.o queue.o linkedlist.o
	gcc $^ -o $@


test_bst: test_bst.o bst.o
	gcc $^ -o $@

test_zigzag: test_zigzag.o bst.o queue.o linkedlist.o

EXECS = $(patsubst %.c, %, $(wildcard test*c))

clean:
	rm -f $(EXECS)
	rm -f *.o

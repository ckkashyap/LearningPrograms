#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linkedlist.h>

LLNode * ll_insert(LLNode *head, void *val) {
	LLNode *n = (LLNode*)malloc(sizeof(LLNode));
	n -> next = head;
	n -> prev = NULL;
	n -> val = val;
	if (head != NULL) head -> prev = n;
	return n;
}


void ll_delete(LLNode *head) {
	LLNode *t;
	while (head) {
		t=head->next;
		free(head);
		head=t;
	}
}

void ll_print(LLNode *head, void (*f)(void *)) {
	while(head) {
		f(head->val);
		head=head->next;
		if(head) printf (" -> ");
	}
	printf("\n");
}

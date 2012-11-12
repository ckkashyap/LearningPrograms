#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include <linkedlist.h>


void f ( void *p ) {
	int *i = (int *)p;
	printf("%d", *i);
}

int main(int argc, char *argv[]) {
	int v1,v2,v3;
	LLNode *head=NULL;

	v1=1;
	head = ll_insert(head, &v1);
	v2=2;
	head = ll_insert(head, &v2);
	v3=3;
	head = ll_insert(head, &v3);


	ll_print(head, f);
	return 0;
}

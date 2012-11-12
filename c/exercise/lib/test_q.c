#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include <queue.h>

void f ( void *p ) {
	int *i = (int *)p;
	if(i != NULL)
	printf("%d", *i);
	else printf("NULL");
}

int main(int argc, char *argv[]) {
	int v1,v2,v3,v4;
	Queue *queue=NULL;
	int *p;

	v1=1;
	v2=2;
	v3=3;
	v4=4;

	queue = q_enqueue(queue, &v1);
	queue = q_enqueue(queue, &v2);
	queue = q_enqueue(queue, &v3);
	queue = q_enqueue(queue, &v4);

	printf("After enqueing all\n");
	q_print(queue, f);

	p=(int *)q_dequeue(queue);
	printf("%d\n",*p);

	printf("After dequeing\n");
	q_print(queue, f);

	p=(int *)q_dequeue(queue);
	printf("%d\n",*p);

	printf("After dequeing\n");
	q_print(queue, f);

	p=(int *)q_dequeue(queue);
	printf("%d\n",*p);

	printf("After dequeing\n");
	q_print(queue, f);

	p=(int *)q_dequeue(queue);
	if(p == NULL)printf("NULL RETURNED\n");
	
	printf("%d\n",*p);

	printf("After dequeing\n");
	q_print(queue, f);

	return 0;
}

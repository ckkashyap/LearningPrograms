#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <queue.h>

Queue * q_new(){
	Queue *q = (Queue *)malloc(sizeof(Queue));
	q -> head = NULL;
	q -> tail = NULL;
	return q;
}

Queue *q_pushback(Queue *queue, void *val){
	LLNode *t, *n;
	if(queue == NULL)return q_enqueue(queue, val);

	t = queue->tail;
	n = (LLNode*)malloc(sizeof(LLNode));
	n->next = NULL;
	n->prev = t;
	n->val = val;
	t->next = n;
	queue->tail=n;

	return queue;
}

Queue * q_enqueue(Queue *queue, void *val) {
	if(queue == NULL) {
		queue = q_new();
	}
	queue->head = ll_insert(queue->head, val);
	if(queue->tail == NULL) queue->tail=queue->head;
	return queue;
}

void * q_dequeue(Queue *queue) {
	void *v;
	if(queue->tail == NULL) return NULL;
	v = queue->tail->val;
	LLNode *t = queue->tail;
	queue->tail=queue->tail->prev;
	if(queue->tail == NULL)queue->head=NULL;
	else queue->tail->next=NULL;
	if(t!=NULL)free(t);
	return v;
}

void q_print(Queue *q, void (*f) (void *)) {
	printf("%p\n", q->head);
	ll_print(q->head, f);
}

int q_isEmpty(Queue *q) { 
	return (q->tail == NULL);
}


void q_delete(Queue *queue) {
}


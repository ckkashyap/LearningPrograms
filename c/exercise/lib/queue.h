#ifndef __QUEUE_H__
#define __QUEUE_H__

#include "linkedlist.h"

typedef struct _queue {
	LLNode *head;
	LLNode *tail;
} Queue;


Queue * q_new();
Queue * q_enqueue(Queue *queue, void *val);
void * q_dequeue(Queue *queue);
void q_delete(Queue *queue);
void q_print(Queue *q, void (*f) (void *));
int q_isEmpty(Queue *q);
Queue *q_pushback(Queue *queue, void *val);
Queue * q_switchDirection(Queue *queue);
Queue *q_reverse(Queue *q);

#endif

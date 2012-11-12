#ifndef __LINKED_LIST_H__
#define __LINKED_LIST_H__

typedef struct _node {
	void *val;
	struct _node *next;
	struct _node *prev;
} LLNode;


LLNode * ll_insert(LLNode *head, void *val);
void ll_delete(LLNode *head);
void ll_print(LLNode *head, void (*f)(void *));

#endif

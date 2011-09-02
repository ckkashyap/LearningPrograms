#ifndef __LIST_H__
#define __LIST_H__

typedef struct _Node {
  int value;
  struct _Node *next;
} Node;

Node* newNode(int);
void insert(Node *, int);
void print(Node *);
Node *reverse(Node *);
Node *mergeTCO(Node *, Node *);

#endif

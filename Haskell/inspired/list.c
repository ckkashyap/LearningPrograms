#include <stdio.h>
#include <stdlib.h>

typedef struct _node {
  int i;
  int count;
  struct _node *next;
} Node;

Node *Empty = (Node *)NULL;

Node *cons(int i, Node *rest){
  Node *newNode = (Node *)malloc(sizeof(Node));
  newNode->i = i;
  newNode->count=1;
  newNode->next = rest;
  return newNode;
}

Node *concat(Node * l1, Node * l2) {
  if (l1 == Empty) {
    return l2;
  }
  if (l2 == Empty) {
    return l1;
  }
  return (cons (l1->i, concat (l1->next, l2)));
}

void print(Node *l){
  if(l==Empty){printf("\n");return;}
  printf("%d ",l->i);
  print(l->next);
}


int main(){
  Node * l1 = cons (1, (cons (2, Empty)));
  Node * l2 = cons (1, (cons (2, cons (3, Empty))));
  print(l1);
  print(l2);
  print(concat(l1,l2));
}



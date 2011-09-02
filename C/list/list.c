#include "list.h"
#include <stdio.h>
#include <stdlib.h>

Node *newNode(int value) {
  Node *node=(Node *)malloc(sizeof(Node));
  node->value=value;
  node->next=NULL;
  return node;
}

void print(Node *head) {
  while(head) {
    printf("%d ",head->value);
    head=head->next;
  }
  printf("\n");
}

void insert(Node *head, int value) {
  Node *node;
  while(head->next) {
    head=head->next;
  }
  node=newNode(value);
  head->next=node;
}


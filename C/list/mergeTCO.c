#include "list.h"
#include <stdio.h>

int n1BiggerThann2(Node *n1, Node *n2) {
  return (n1->value > n2->value);
}

Node *_mergeTCO(Node *l1, Node *l2, Node *prev, Node *start) {
  if(l1==NULL && l2==NULL)return start;
  if(l1==NULL) {
    prev->next=l2;
    return start;
  }
  if(l2==NULL) {
    prev->next=l1;
    return start;
  }

  if (n1BiggerThann2(l1,l2)) {
    prev->next=l2;
    prev=l2;
    l2=l2->next;
  } else {
    prev->next=l1;
    prev=l1;
    l1=l1->next;
  }

  return _mergeTCO(l1,l2,prev,start);
}

Node *mergeTCO(Node *l1, Node *l2) {
  if(l1==NULL && l2==NULL)return NULL;
  if(l1==NULL) return l2;
  if(l2==NULL) return l1;

  if (n1BiggerThann2(l1,l2))
    return _mergeTCO(l1,l2->next,l2,l2);
  else
    return _mergeTCO(l1->next,l2,l1,l1);
}

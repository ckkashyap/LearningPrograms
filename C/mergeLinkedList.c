#include <stdio.h>

typedef struct _Node {
  int i;
  struct _Node *next;
} Node;

Node* newNode(int i) {
  Node *node=(Node *)malloc(sizeof(Node));
  node->i=i;
  node->next=NULL;
  return node;
}

void insert(Node *head, int i) {
  Node *node;
  while(head->next) {
    head=head->next;
  }
  node=newNode(i);
  head->next=node;
}

void print(Node *head) {
  while(head) {
    printf("%d ",head->i);
    head=head->next;
  }
  printf("\n");
}

Node *merge(Node *l1, Node *l2) {

  Node *l3=NULL;
  if(!l1 && !l2) {
    return NULL;
  }
  if(!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }

  if(l1->i < l2->i) {
    l3=merge(l1->next,l2);
    l1->next=l3;
    return l1;
  } else {
    l3=merge(l1,l2->next);
    l2->next=l3;
    return l2;
  }
}

int main(int argc, char *argv[]) {
  Node *list1,*list2,*l3;

  list1=newNode(1);
  insert(list1,2);
  insert(list1,3);

  list2=newNode(4);
  insert(list2,5);
  insert(list2,6);

  print(list1);
  print(list2);
  
  l3=merge(list1,list2);
  print(l3);
}


    

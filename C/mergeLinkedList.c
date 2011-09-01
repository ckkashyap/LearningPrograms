#include <stdio.h>
#include <stdlib.h>

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

Node *reverse(Node *input, Node *output);  

int reverseCall=0;
int appendCall=0;


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

Node *append(Node *l1, Node *l2) 
{
  Node *ret=l1;
  appendCall++;
  if(!l1)return l2;
  if(!l2)return l1;
  while(l1->next) {
    l1=l1->next;
  }
  l1->next=l2;
  return ret;
}
  

Node *_mergeTailCall(Node *l1, Node *l2, Node *acc) {
  Node *temp;
  if(!l1 && !l2) {
    return NULL;
  }
  if (l2==NULL){
    return append(l1,acc);
  }
  if (l1==NULL){
    return append(l2,acc);
  }
  if (l1->i < l2->i) {
    temp=l1;
    l1=l1->next;
    temp->next=acc;
    return _mergeTailCall(l1, l2, temp);
  } else {
    temp=l2;
    l2=l2->next;
    temp->next=acc;
    return _mergeTailCall(l1, l2, temp);
  }
}

Node *mergeTailCall(Node *l1, Node *l2) {
  return (reverse (_mergeTailCall(l1,l2,NULL),NULL));
}

Node *mergeIter(Node *l1, Node *l2) {
  Node *ret,*temp;
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
    ret=l1;
    l1=l1->next;
  } else {
    ret=l2;
    l2=l2->next;
  }

  temp=ret;
    
  while( l1 && l2) {
    printf("%d %d\n",l1->i,l2->i);
    if(l1->i < l2->i) {
      temp->next=l1;
      l1=l1->next;
    } else {
      temp->next=l2;
      l2=l2->next;
    }
    temp=temp->next;

  }
  printf("----\n");
  return ret;
}


Node *reverse(Node *input, Node *output){
  Node *temp;

  reverseCall++;

  if (input==NULL) {
    return output;
  }

  temp=input->next;
  input->next=output;

  return reverse(temp,input);
}

  



int main(int argc, char *argv[]) {
  Node *list1,*list2,*l3,*l4;
  int start=1,i,N;
  

  N=atoi(argv[1]);
  list1=newNode(start);
  for (i=start+2;i<N;i+=2){
    insert(list1,i);
  }

  list2=newNode(start+1);
  for (i=start+3;i<N;i+=2){
    insert(list2,i);
  }

  if(N<20) {print(list1);
    print(list2);}


  //l3=merge(list1,list2);
      l3=mergeTailCall(list1,list2);
    
  if(N<20) print(l3);
  printf("Reverse called %d times and append called %d times",
	 reverseCall, appendCall);

}


    

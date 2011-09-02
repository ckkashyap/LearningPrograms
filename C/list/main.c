#include "list.h"

Node *getList(int start,int count,int step) {
  int i;
  Node *head=newNode(start);
  start+=step;
  count--;
  for (i=0; i < count ; i++) {
    insert(head,start);
    start+=step;
  }
  return head;
}
    

int main(int argc, char *argv[]) {
  Node *l1,*l2,*l3;
  int N;
  
  N=atoi(argv[1]);
  l1=getList(1,N,2);
  l2=getList(2,N,2);

  if(N<20) {
    print(l1);
    print(l2);
  }

  print(mergeTCO(l1,l2));

}


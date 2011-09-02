#include "list.h"
#include <stdio.h>

Node *_reverse(Node *input, Node *output){
  Node *temp;

  if (input==NULL) {
    return output;
  }

  temp=input->next;
  input->next=output;

  return _reverse(temp,input);
}

Node *reverse(Node *in) {
  return _reverse(in,NULL);
}

#include <stdio.h>
#include <stdlib.h>

typedef struct _Node {
  int d;
  struct _Node *left, *right;
} Node;


Node *newNode(int v){
  Node *n = (Node *)malloc(sizeof(Node));
  n->d=v;
  n->left=n->right=NULL;
  return n;
}


void readTree(FILE *fp, Node **root) {

  char command[5];
  int v;

  fscanf(fp,"%s\b",command);
  printf("command = %s\n", command);

  if(!strcmp(command, "end")){
    return;
  }

  sscanf(command,"%d\n",&v);
  *root=newNode(v);

  readTree(fp, &((*root)->left));
  readTree(fp, &((*root)->right));

}

void print(Node *node){
  if(node==NULL)return;

  print(node->left);
  printf("%d\n", node->d);
  print(node->right);
}


int main(){
  Node *node;
  FILE *fp=fopen("tree.txt","r");
  printf("start\n");
  printf("file opened %p\n", fp);
  readTree(fp, &node);
  printf("Reading done\n");
  print(node);
}

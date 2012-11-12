#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bst.h>


BSTNode *newNode(int val){
	BSTNode *n=(BSTNode *)malloc(sizeof(BSTNode));
	int *v = (int *)malloc(sizeof(int));
	*v=val;
	n->val=v;
	return n;
}

void f(BSTNode *n){
	printf("%d",*((int *)n->val));
}

int main(int argc, char *argv[]) {
	BSTNode *root=NULL;
	root=bst_insert(root, newNode(4));
	root=bst_insert(root, newNode(2));
	root=bst_insert(root, newNode(6));
	root=bst_insert(root, newNode(1));
	root=bst_insert(root, newNode(3));
	root=bst_insert(root, newNode(5));
	root=bst_insert(root, newNode(7));

	bst_dump(root, f);
	return 0;
}

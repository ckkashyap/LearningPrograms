#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bst.h>
#include <queue.h>


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

typedef struct {
	BSTNode *node;
	int level;
} NODE;

void traverse(BSTNode *root) {
	Queue *q=NULL;
	int prevLevel=0;
	NODE *node,*node1;
	node = (NODE*)malloc(sizeof(NODE));
	node->node=root;
	node->level=0;
	

	q=q_enqueue(q,(void *)node);
	while(!q_isEmpty(q)) {
		node = (NODE *)q_dequeue(q);
		if(node->level != prevLevel){
			prevLevel=node->level;
			printf(" | " );
			q_pushback(q, node);
			q=q_reverse(q);
			node = (NODE *)q_dequeue(q);
		}
		if (node->node!=NULL) {
			BSTNode *v1,*v2;

			if (node -> level % 2) {
				v1 = node->node->left;
				v2 = node->node->right;
			} else {
				v2 = node->node->left;
				v1 = node->node->right;
			}


			node1  = (NODE*)malloc(sizeof(NODE));
			node1->node=v1;
			node1->level=node->level+1;
			q=q_enqueue(q, (void *)node1);

			node1  = (NODE*)malloc(sizeof(NODE));
			node1->node=v2;
			node1->level=node->level+1;
			q=q_enqueue(q, (void *)node1);
			f(node->node);
			printf("(%d) ",node->level);
		}
	}
	printf("\n");
	
}

int main(int argc, char *argv[]) {
	BSTNode *root=NULL;





	root = bst_insert(root, newNode(16));
	root = bst_insert(root, newNode(8));
	root = bst_insert(root, newNode(24));
	root = bst_insert(root, newNode(4));
	root = bst_insert(root, newNode(12));
	root = bst_insert(root, newNode(20));
	root = bst_insert(root, newNode(28));
	root = bst_insert(root, newNode(2));
	root = bst_insert(root, newNode(6));
	root = bst_insert(root, newNode(10));
	root = bst_insert(root, newNode(14));
	root = bst_insert(root, newNode(18));
	root = bst_insert(root, newNode(22));
	root = bst_insert(root, newNode(26));
	root = bst_insert(root, newNode(30));
	root = bst_insert(root, newNode(1));
	root = bst_insert(root, newNode(3));
	root = bst_insert(root, newNode(5));
	root = bst_insert(root, newNode(7));
	root = bst_insert(root, newNode(9));
	root = bst_insert(root, newNode(11));
	root = bst_insert(root, newNode(13));
	root = bst_insert(root, newNode(15));
	root = bst_insert(root, newNode(17));
	root = bst_insert(root, newNode(19));
	root = bst_insert(root, newNode(21));
	root = bst_insert(root, newNode(23));
	root = bst_insert(root, newNode(25));
	root = bst_insert(root, newNode(27));
	root = bst_insert(root, newNode(29));
	root = bst_insert(root, newNode(31));
	


	
	/*
	root = bst_insert(root, newNode(8));
	root = bst_insert(root, newNode(4));
	root = bst_insert(root, newNode(12));
	root = bst_insert(root, newNode(2));
	root = bst_insert(root, newNode(6));
	root = bst_insert(root, newNode(10));
	root = bst_insert(root, newNode(14));
	root = bst_insert(root, newNode(1));
	root = bst_insert(root, newNode(3));
	root = bst_insert(root, newNode(5));
	root = bst_insert(root, newNode(7));
	root = bst_insert(root, newNode(9));
	root = bst_insert(root, newNode(11));
	root = bst_insert(root, newNode(13));
	root = bst_insert(root, newNode(15));
	*/



	bst_dump(root, f);
	traverse(root);

	return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bst.h>

BST_ORD cmp (void *v2, void *v1){
	int *p1, *p2;
	p1=(int *)v1;
	p2=(int *)v2;

	if(p1 == NULL && p2 == NULL) return BST_EQ;

	if(p1 == NULL) return BST_GT;
	if(p2 == NULL) return BST_LT;

	if(*p1 == *p2) return BST_EQ;
	if(*p1 < *p2) return BST_LT;
	if(*p1 > *p2) return BST_GT;
}

BSTNode *bst_insert(BSTNode *root, BSTNode *node) {
	BSTNode *ret=NULL;

	if(root == NULL) return node;
	
	switch (cmp (root->val, node->val)) {
		case BST_EQ:
		case BST_LT:
			if(root->left == NULL)
				root->left = node;
			else
				bst_insert(root->left, node);
			break;
		case BST_GT:
			if(root->right == NULL)
				root->right = node;
			else
				bst_insert(root->right, node);
			break;
	}

	return root;
}
			
			
void bst_dump(BSTNode *root, void (*f)(BSTNode *n)) {
	if(root == NULL)return;

	bst_dump(root->left, f);
	f(root);
	printf("\n");
	bst_dump(root->right, f);
}

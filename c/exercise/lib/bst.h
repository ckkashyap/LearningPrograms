#ifndef __BST_H__
#define __BST_H__

typedef enum {
	BST_EQ,
	BST_GT,
	BST_LT
} BST_ORD;

typedef struct _bstNode {
	struct _bstNode *left, *right;
	void *val;
	BST_ORD (*cmp) (void *v1, void *v2);
} BSTNode;

BSTNode *bst_insert(BSTNode *root, BSTNode *val);
void bst_dump(BSTNode *root, void (*f)(BSTNode *n));

#endif

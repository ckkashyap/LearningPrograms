#include <stdio.h>
#include <stdlib.h>

int **ARRAY;

#define N (5)

/*
 *
 *
 * Expects an input file of the format
 * 1
 * 2 3
 * 4 5 6
 * 7 8 9 10
 */
void init(){
	int i,j;
	// Allocate
	ARRAY=(int**)malloc(N*sizeof(int *));
	for(i=0;i<N;i++){
		ARRAY[i]=(int *)malloc(sizeof(int)*(i+1));
	}

	// Read
	for(i=0;i<N;i++){
		for(j=0;j<(i+1);j++){
			scanf("%d",&ARRAY[i][j]);
		}
	}

	// Print
	for(i=0;i<N;i++){
		for(j=0;j<(i+1);j++){
			printf("%d ",ARRAY[i][j]);
		}
		printf("\n");
	}
}

int PATH[N];

int maxSum(int r, int c){
	int n,s1,s2;
	n=ARRAY[r][c];
	if(r==(N-1)){
		return n;
	}
	s1=n+maxSum(r+1,c);
	s2=n+maxSum(r+1,c+1);

	if(s1>s2){
		PATH[r+1]=0;
		return s1;
	}
	else {
		PATH[r+1]=1;
		return s2;
	}
}


int main(){
	int i;
	init();
	printf("%d\n",maxSum(0,0));
	for(i=0;i<N-1;i++)printf("%d ",PATH[i]);
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define N (8) //Number of ID's to be returned

int* GetSomeIDs();
void PrintAndFree(int*);

typedef struct {
	char* alias;   /* '\0'-terminated C string */
	int   specific_id;
} AliasID;


int main(){

	int *p=GetSomeIDs();

	PrintAndFree(p);

	return 0;
}


int GetNumberOfAliases(){
	return 5;
}

AliasID* GetNextAliasID(){
	static int counter;

	AliasID *alias=(AliasID*)malloc(sizeof(AliasID));
	alias->alias=(char*)malloc(20);
	sprintf(alias->alias,"STR %d",counter);
	alias->specific_id = counter;
	counter++;
	return alias;
}

int* GetSomeIDs(){
        int*		buf1;
        AliasID*	buf2;
        AliasID*	alias;
        int		n,i,l;

        buf1 = (int*)malloc(N*sizeof(int)+sizeof(AliasID*)+sizeof(int));

        n=GetNumberOfAliases();
        buf2=(AliasID*)malloc(sizeof(AliasID) * n);

	for(i=0;i<N;i++){
		buf1[i]=100+i;
	}

        for(i=0;i<n;i++){
                alias=GetNextAliasID();
		l=strlen(alias->alias);
		buf2[i].alias=(char*)malloc(l+1);
		strcpy(buf2[i].alias, alias->alias);
		free(alias->alias);
		buf2[i].specific_id = alias->specific_id;
                free(alias);
        }

	buf1[N] = n;
	buf1[N+1] = (int)buf2;

        return buf1;
}

void PrintAndFree(int* p){
	int		numberOfAliases;
	AliasID*	alias;
	int		i;

	for(i=0;i<N;i++){
		printf("%d\n",p[i]);
	}

	numberOfAliases = p[N];
	alias = (AliasID*)p[N+1];
	for(i=0;i<numberOfAliases;i++){
		printf("%s\n",alias[i].alias);
		free(alias[i].alias);
	}
	free(alias);
	free(p);
}

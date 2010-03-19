int fib(int n){
	if(n<=1)return n;

	return fib(n-1) + fib(n-2);
}

int mem[2000];
int newfib(int n){
	if(n<=1)return n;
	if(!mem[n])
		mem[n]=newfib(n-1)+newfib(n-2);

	return mem[n];
}

#define N 50
int main(int argc,char*argv[]){
	int i;
	if(argc>1){
		for(i=0;i<N;i++){
			printf("%d ",fib(i));
		}
	}else{
		for(i=0;i<N;i++){
			printf("%d ",newfib(i));
		}
	}
	printf("\n");
}

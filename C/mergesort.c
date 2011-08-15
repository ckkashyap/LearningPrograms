#include <stdio.h>

int temp[1024];
int input[]={
  1,
  10,
  9,
  6,
  2,
  4,
  3,
  7,
  8,
  5
};

const int LOW = 0;
const int HIGH = (((sizeof(input))/(sizeof(int))) - 1);

void mergeSort(int *list, int low, int high);
void merge(int *list, int low, int mid, int high);

void mergeSort(int *list, int low, int high)
{
  int mid;
  if (low < high) {
    mid = (low + high ) / 2;
    mergeSort(list, low, mid);
    mergeSort(list, mid+1, high);
    merge(list,low,mid,high);
  }
}

void merge(int *list, int low, int mid, int high)
{
  int pos1=low;
  int pos2=mid + 1;
  int counter=low;
  int k;
  while (pos1 <= mid && pos2 <= high) {
    if (list[pos1] <= list[pos2]) {
      temp[counter] = list[pos1];
      pos1++;
    } else {
      temp[counter] = list[pos2];
      pos2++;
    }
    counter++;
  }
  if (pos1 > mid) {
    for (k = pos2; k <= high; k++) {
      temp[counter] = list[k];
      counter++;
    }
  } else {
    for (k = pos1; k <= mid; k++) {
      temp[counter] = list[k];
      counter++;
    }
  }

  for (k = low; k <= high; k++) {
    list[k] = temp[k];
  }
}

void print() {
  int k;
  for (k=0;k<=HIGH;k++) {
    printf ("%d ", input[k]);
  }
  printf ("\n");
}

int main()
{
  printf(" %d %d \n",LOW,HIGH);
  mergeSort(input,LOW,HIGH);
  print();
}

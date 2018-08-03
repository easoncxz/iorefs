
#include <stdio.h>
#include <stdlib.h>


struct Heap {
  int *data;
  int capacity;
  int size;
};

struct MaybeInt {
  int isJust;
  int val;
};

struct Heap initialiseHeap() {
  struct Heap h = {
    .data = (int *) malloc(sizeof(int)),
    .capacity = 1,
    .size = 0
  };
  return h;
}

void freeHeap(struct Heap *h) {
  free(h->data);
  h->data = NULL;
}

int findParent(int pos) {
  return (pos + 1) / 2 - 1;
}

int findLeftChild(int pos) {
  return 2 * (pos + 1) - 1;
}

int findRightChild(int pos) {
  return findLeftChild(pos) + 1;
}

void printArray(int size, int xs[]) {
  printf("[");
  for (int i = 0; i < size; i++) {
    printf("%d", xs[i]);
    if (i < size - 1) {
      printf(", ");
    }
  }
  printf("]");
}

void printHeap(struct Heap h) {
  printf("Heap(capacity=%d, size=%d): ", h.capacity, h.size);
  printArray(h.size, h.data);
  printf("\n");
}

void growHeap(struct Heap *h) {
  int *bigger = (int *) malloc(2 * h->capacity * sizeof(int));
  for (int i = 0; i < h->size; i++) {
    bigger[i] = h->data[i];
  }
  free(h->data);
  h->data = bigger;
  h->capacity *= 2;
}

void siftUpMaxHeap(struct Heap *h, int pos) {
  while (pos > 0) {
    int parent = findParent(pos);
    if (h->data[pos] > h->data[parent]) {
      int temp = h->data[pos];
      h->data[pos] = h->data[parent];
      h->data[parent] = temp;
    }
    pos = parent;
  }
}

void insertMaxHeap(struct Heap *h, int val) {
  int freshIx = h->size;
  h->data[freshIx] = val;
  siftUpMaxHeap(h, freshIx);
  h->size += 1;
  if (h->size == h->capacity) {
    growHeap(h);
  }
}

void siftDownMaxHeap(const struct Heap *h, int pos) {
  if (pos >= 0) {
    while (pos < h->size) {
      int left = findLeftChild(pos);
      int right = findRightChild(pos);
      int largestPos = pos;
      if (right < h->size && h->data[right] > h->data[largestPos]) {
        largestPos = right;
      }
      if (left < h->size && h->data[left] > h->data[largestPos]) {
        largestPos = left;
      }
      if (largestPos == pos) {
        break;
      } else {
        int temp = h->data[pos];
        h->data[pos] = h->data[largestPos];
        h->data[largestPos] = temp;
        pos = largestPos;
      }
    }
  } else {
    printf("Error: sifting down from invalid pos: %d\n", pos);
    int *p = NULL;
    *p = 0;
  }
}

struct MaybeInt removeMaxMaxHeap(struct Heap *h) {
  if (h->size > 0) {
    struct MaybeInt r = {
      .isJust = 1,
      .val = h->data[0]
    };
    h->data[0] = h->data[h->size - 1];
    h->size -= 1;
    siftDownMaxHeap(h, 0);
    return r;
  } else {
    return (struct MaybeInt) {
      .isJust = 0,
      .val = 0
    };
  }
}

void maxHeapify(const struct Heap *h) {
  for (int pos = h->size / 2; pos >= 0; pos--) {
    siftDownMaxHeap(h, pos);
  }
}

void myHeapsort(int size, int *xs) {
  struct Heap h = {
    .data = xs,
    .capacity = size + 1, // pretend we have valid memory at position `xs[size]`
    .size = size
  };
  maxHeapify(&h);
  while (h.size > 0) {
    struct MaybeInt mbLargestX = removeMaxMaxHeap(&h);
    if (mbLargestX.isJust) {
      h.data[h.size] = mbLargestX.val;
    } else {
      printf("Error: heap is known to be non-empty, yet we cannot removeMax?\n");
      int *p = NULL;
      *p = 0;
    }
  }
}

int main() {
  printf("sizeof(int): %lu\n", sizeof(int));
  printf("sizeof(int *): %lu\n", sizeof(int *));

  int xs[] = {-4,18,-7,-15,7,-13,-26,28,-25,30,-23,-5,-1,16,18,7,24,15,-14};
  int xsLength = sizeof(xs) / sizeof(int);
  printArray(xsLength, xs);
  printf("\n");
  myHeapsort(xsLength, xs);
  printArray(xsLength, xs);
  printf("\n");

  // struct Heap h = initialiseHeap();
  // for (int i = 0; i < xsLength; i++) {
  //   insertMaxHeap(&h, xs[i]);
  // }
  // printHeap(h);
  // freeHeap(&h);

  return 0;
}


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
    .data = (int *) malloc(2 * sizeof(int)),
    .capacity = 1,
    .size = 0
  };
  h.data[0] = 0; // never used
  return h;
}

void freeHeap(struct Heap *h) {
  free(h->data);
  h->data = NULL;
}

void printHeap(struct Heap h) {
  printf("Heap(capacity=%d, size=%d): [", h.capacity, h.size);
  for (int i = 1; i < h.size + 1; i++) {
    printf("%d", h.data[i]);
    if (i < h.size) {
      printf(", ");
    }
  }
  printf("]\n");
}

void growHeap(struct Heap *h) {
  int *bigger = (int *) malloc((1 + 2 * h->capacity) * sizeof(int));
  for (int i = 0; i < h->size + 1; i++) {
    bigger[i] = h->data[i];
  }
  free(h->data);
  h->data = bigger;
  h->capacity *= 2;
}

void siftUpMaxHeap(struct Heap *h, int pos) {
  while (pos > 1) {
    int parent = pos / 2;
    if (h->data[pos] > h->data[parent]) {
      int temp = h->data[pos];
      h->data[pos] = h->data[parent];
      h->data[parent] = temp;
    }
    pos = parent;
  }
}

void insertMaxHeap(struct Heap *h, int val) {
  int freshIx = h->size + 1;
  h->data[freshIx] = val;
  siftUpMaxHeap(h, freshIx);
  h->size += 1;
  if (h->size == h->capacity) {
    growHeap(h);
  }
}

void siftDownMaxHeap(struct Heap *h, int pos) {
  if (pos >= 1) {
    int bound = h->size + 1;
    while (pos < bound) {
      // TODO: We need to find the larger child. Current logic is wrong
      int nextPos = 0;  // like `Nothing`
      int left = 2 * pos;
      int right = left + 1;
      if (right < bound && h->data[right] > h->data[pos]) {
        int temp = h->data[pos];
        h->data[pos] = h->data[right];
        h->data[right] = temp;
        nextPos = right;
      }
      if (left < bound && h->data[left] > h->data[pos]) {
        int temp = h->data[pos];
        h->data[pos] = h->data[left];
        h->data[left] = temp;
        nextPos = nextPos == 0 ? left : nextPos;  // like `<|> Just left`
      }
      nextPos = nextPos == 0 ? left : nextPos;  // pos is a leaf node; nextPos points out of bounds
      pos = nextPos;
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
      .val = h->data[1]
    };
    h->data[1] = h->data[h->size];
    h->size -= 1;
    siftDownMaxHeap(h, 1);
    return r;
  } else {
    return (struct MaybeInt) {
      .isJust = 0,
      .val = 0
    };
  }
}

int main() {
  printf("sizeof(int): %lu\n", sizeof(int));
  printf("sizeof(int *): %lu\n", sizeof(int *));
  struct Heap h = initialiseHeap();

  int xs[] = {3,3,4,1,2,7,5,6,3,7};
  int xsLength = sizeof(xs) / sizeof(int);
  for (int i = 0; i < xsLength; i++) {
    insertMaxHeap(&h, xs[i]);
  }
  printHeap(h);
  printf("\n");

  while (h.size > 0) {
    struct MaybeInt m = removeMaxMaxHeap(&h);
    if (m.isJust) {
      printf("Removed max: %d\n", m.val);
    } else {
      printf("Attempted to remove from empty heap.\n");
    }
    printHeap(h);
  }

  freeHeap(&h);
  return 0;
}

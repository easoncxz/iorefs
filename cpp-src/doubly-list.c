
#include <stdio.h>
#include <stdlib.h>

struct DoublyNode {
  int val;
  struct DoublyNode *prev;
  struct DoublyNode *next;
};

struct DoublyList {
  struct DoublyNode *head;
  struct DoublyNode *last;
};

typedef struct DoublyNode Node;

typedef struct DoublyList List;

List insertBack(int val, List l) {
  Node *fresh = (Node *) malloc(sizeof (Node));
  *fresh = (Node) {
    .val = val,
    .prev = l.last,
    .next = NULL
  };
  if (l.last == NULL) {
    return (List) {
      .head = fresh,
      .last = fresh
    };
  } else {
    l.last->next = fresh;
    return (List) {
      .head = l.head,
      .last = fresh
    };
  }
}

List insertFront(int val, List l) {
  Node *fresh = (Node *) malloc(sizeof (Node));
  *fresh = (Node) {
    .val = val,
    .prev = NULL,
    .next = l.head
  };
  if (l.head == NULL) {
    return (List) {
      .head = fresh,
      .last = fresh
    };
  } else {
    l.head->prev = fresh;
    return (List) {
      .head = fresh,
      .last = l.last
    };
  }
}

List reverseInPlace(List l) {
  for (Node *curr = l.head; curr != NULL; ) {
    Node *next = curr->next;
    curr->next = curr->prev;
    curr->prev = next;
    curr = next;
  }
  return (List) {
    .head = l.last,
    .last = l.head
  };
}

List fromArray(int count, int xs[]) {
  List l = {
    .head = NULL,
    .last = NULL
  };
  for (int i = 0; i < count; i++) {
    l = insertFront(xs[i], l);
  }
  return reverseInPlace(l);
}

void printListFromHead(List l) {
  printf("[");
  for (Node *curr = l.head; curr != NULL; curr = curr->next) {
    printf("%d", curr->val);
    if (curr->next != NULL) {
      printf(", ");
    }
  }
  printf("]\n");
}

void freeList(List l) {
  for (Node *curr = l.head; curr != NULL; ) {
    Node *next = curr->next;
    free(curr);
    curr = next;
  }
}

int main(int argc, char *argv[]) {
  printf("sizeof Node: %lu\n", sizeof (Node));
  printf("sizeof List: %lu\n", sizeof (List));
  int xs[] = {1,3,2,4,5};
  List l = fromArray(5, xs);
  printListFromHead(l);
  freeList(l);
  return 0;
}

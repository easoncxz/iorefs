
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

List fromArray(int count, int xs[]) {
  List l = {
    .head = NULL,
    .last = NULL
  };
  for (int i = 0; i < count; i++) {
    l = insertBack(xs[i], l);
  }
  return l;
}

void printList(List l) {
  printf("[");
  for (Node *curr = l.head; curr != NULL; curr = curr->next) {
    printf("%d", curr->val);
    if (curr->next != NULL) {
      printf(", ");
    }
  }
  printf("]\n");
}

int main(int argc, char *argv[]) {
  printf("sizeof Node: %lu\n", sizeof (Node));
  printf("sizeof List: %lu\n", sizeof (List));
  int xs[] = {1,3,2,4,5};
  List l = fromArray(5, xs);
  printList(l);
  return 0;
}

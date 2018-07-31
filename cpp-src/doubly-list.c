
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

int isEmpty(List l) {
  return l.head == NULL && l.last == NULL;
}

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

Node *popBack(List *l) {
  Node *last = l->last;
  if (last == NULL) {
  } else {
    l->last = last->prev;
    if (l->last == NULL) {
      l->head = NULL;
    } else {
      l->last->next = NULL;
    }
  }
  return last;
}

Node *popFront(List *l) {
  Node *head = l->head;
  if (head == NULL) {
  } else {
    l->head = head->next;
    if (l->head == NULL) {
      l->last = NULL;
    } else {
      l->head->prev = NULL;
    }
  }
  return head;
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

List deleteAll(int val, List l) {
  for (Node **pcurr = &l.head; *pcurr != NULL; ) {
    if ((*pcurr)->val == val) {
      Node *next = (*pcurr)->next;
      if (next == NULL) { // last changes
        l.last = (*pcurr)->prev;  // fix backward pointer
      } else { // last preserves
        next->prev = (*pcurr)->prev;  // fix backward pointer
      }
      free(*pcurr);
      *pcurr = next;            // fix forward pointer
    } else {
      pcurr = &(*pcurr)->next;
    }
  }
  return l;
}

List deleteAllNoPP(int val, List l) {
  Node dummy = {
    .val = 0,
    .prev = NULL,
    .next = l.head
  };
  {
    Node *prev = &dummy;
    Node *curr = dummy.next;
    while (curr != NULL) {
      if (curr->val == val) {
        Node *next = curr->next;
        prev->next = next;  // fix forward link
        if (next == NULL) {
          l.last = prev;  // fix backwards link
          free(curr);
          curr = next;
        } else {
          next->prev = prev;  // fix backwards link
          free(curr);
          curr = next;
        }
      } else {
        prev = curr;
        curr = curr->next;
      }
    }
  }
  l.head = dummy.next;
  return l;
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

int readLineOfInts(int xsLength, int xs[]) {
  printf("Input a line of numbers: ");
  int i = 0;
  char sep = '?';
  while (sep != '\n' && i < xsLength) {
    scanf("%d%c", &xs[i], &sep);
    i++;
  }
  printf("Finished reading %d int's\n", i);
  return i;
}

int main(int argc, char *argv[]) {
  int xs[] = {};
  int xsLength = (int) (sizeof (xs)) / (sizeof (int));

  printf("sizeof Node: %lu\n", sizeof (Node));
  printf("sizeof List: %lu\n", sizeof (List));
  printf("sizeof int: %lu\n", sizeof (int));
  printf("sizeof xs: %lu\n", sizeof (xs));

  List l = fromArray(xsLength, xs);
  printListFromHead(l);
  l = deleteAllNoPP(0, l);
  printListFromHead(l);
  freeList(l);

  return 0;
}

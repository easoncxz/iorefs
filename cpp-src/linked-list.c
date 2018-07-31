#include <stdio.h>
#include <stdlib.h>

struct ListNode {
    int val;
    struct ListNode *next;
};

typedef struct ListNode Node;

Node *insertVal(int val, Node *head) {
  Node *fresh = (Node *) malloc(sizeof(Node));
  fresh->val = val;
  fresh->next = head;
  return fresh;
}

void freeList(Node *head) {
  while (head != NULL) {
    Node *next = head->next;
    free(head);
    head = next;
  }
}

Node *reverseList(Node *head) {
  Node *ret = NULL;
  while (head != NULL) {
    Node *next = head->next;
    head->next = ret;
    ret = head;
    head = next;
  }
  return ret;
}

void printList(Node *l) {
  printf("[");
  for (Node *ll = l; ll != NULL; ll = ll->next) {
    printf("%d", ll->val);
    if (ll->next != NULL) {
      printf(", ");
    }
  }
  printf("]\n");
}

Node *fromArray(int xs[], int count) {
  Node *l = NULL;
  for (int i = count - 1; i >= 0; i--) {
    l = insertVal(xs[i], l);
  }
  return l;
}

Node *removeElements(Node *head, int val) {
    for (Node **pcurrent = &head; *pcurrent != NULL; ) {
        if ((*pcurrent)->val == val) {
          Node *next = (*pcurrent)->next;
          free(*pcurrent);
          *pcurrent = next;
        } else {
          pcurrent = &(*pcurrent)->next;
        }
    }
    return head;
}

Node *removeElementsRec(Node *head, int val) {
  if (head == NULL) {
    return NULL;
  } else {
    if (head->val == val) {
      Node *next = head->next;
      free(head);
      return removeElementsRec(next, val);
    } else {
      head->next = removeElementsRec(head->next, val);
      return head;
    }
  }
}

Node *removeElements2(Node *head, int val) {
  Node dummy = {
    .val = 0,
    .next = head
  };
  Node *prev = &dummy;
  Node *curr = head;
  while (curr != NULL) {
    if (curr->val == val) {
      prev->next = curr->next;
      free(curr);
      curr = prev->next;
    } else {
      prev = prev->next;
      curr = curr->next;
    }
  }
  return dummy.next;
}

int main (int argc, char *argv[]) {
  int xs[] = {1, 2, 6, 3, 4, 5, 6};
  Node *l = removeElements2(fromArray(xs, 7), 6);
  printList(l);
  freeList(l);
  printf("sizeof int: %lu\n", sizeof (int));
  printf("sizeof Node*: %lu\n", sizeof (Node *));
  printf("sizeof Node: %lu\n", sizeof (Node));
  return 0;
}

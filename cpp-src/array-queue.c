
#include <stdio.h>
#include <stdlib.h>

struct CircularQueue {
  int head;
  int tail;
  int *data;
  int capacity;
  int size;
};

typedef struct CircularQueue Queue;

struct MaybeInt {
  int isJust;
  int val;
};

Queue initialiseQueue() {
  int capacity = 1;
  int *d = (int *) malloc(capacity * sizeof(int));
  return (Queue) {
    .head = 0,
    .tail = 0,
    .data = d,
    .capacity = capacity,
    .size = 0
  };
}

void freeQueue(Queue *pq) {
  free(pq->data);
  pq->data = NULL;
}

void printQueue(Queue const *pq) {
  printf("Queue(head=%d, tail=%d, capacity=%d) : [", pq->head, pq->tail, pq->capacity);
  for (int i = pq->head; i != pq->tail; i = (1 + i) % pq->capacity) {
    printf("%d", pq->data[i]);
    if ((i + 1) % pq->capacity != pq->tail) {
      printf(", ");
    }
  }
  printf("]\n");
}

void growQueue(Queue *pq) {
  size_t byteCount = sizeof (int) * pq->capacity;
  int *bigger = (int *) malloc(2 * byteCount);
  for (int i = 0; i < pq->capacity; i++) {
    bigger[i] = pq->data[(pq->head + i) % pq->capacity];
  }
  free(pq->data);
  pq->head = 0;
  pq->tail = pq->capacity;
  pq->capacity *= 2;
  pq->data = bigger;
}

void enqueue(int val, Queue *pq) {
  pq->data[pq->tail] = val;
  pq->tail = (1 + pq->tail) % pq->capacity;
  pq->size += 1;
  // The main idea is to never use up all capacity.
  // Grow immediately when it happens.
  // As such, queues with `head == tail` denote empty queues,
  // not "full" queues.
  if (pq->tail == pq->head) {
    growQueue(pq);
  }
}

struct MaybeInt dequeue(Queue *pq) {
  if (pq->head == pq->tail) {
    return (struct MaybeInt) {
      .isJust = 0,
      .val = 0
    };
  } else {
    struct MaybeInt result = {
      .isJust = 1,
      .val = pq->data[pq->head]
    };
    pq->head = (1 + pq->head) % pq->capacity;
    pq->size -= 1;
    return result;
  }
}

// Not really interactive; problematic due to terminal buffering issues.
// Using a script file like ./queue-actions.txt as STDIN would work.
void interact(Queue *pq) {
  char cmd;
  while (scanf("%c", &cmd) > 0) {
    switch (cmd) {
      case 'e':
        {
          int x;
          scanf("%d\n", &x);
          enqueue(x, pq);
          printf("Enqueued: %d\n", x);
          printQueue(pq);
          break;
        }
      case 'd':
        {
          scanf("\n");
          struct MaybeInt mb = dequeue(pq);
          if (mb.isJust) {
            printf("Dequeued: %d\n", mb.val);
          } else {
            printf("Dequeueing from empty queue.\n");
          }
          printQueue(pq);
          break;
        }
      default:
        printQueue(pq);
    }
  }
}

int main(int argc, char *argv[]) {
  Queue q = initialiseQueue();
  interact(&q);
  freeQueue(&q);
  return 0;
}

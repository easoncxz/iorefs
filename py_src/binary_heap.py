
import random

from hypothesis import given
import hypothesis.strategies as st

def parent_0(pos):
    return (pos + 1) // 2 - 1

def left_child_0(pos):
    return 2 * (pos + 1) - 1

def right_child_0(pos):
    return 2 * (pos + 1)

def bubble_up(arr, pos):
    while pos > 0:
        parent_pos = parent_0(pos)
        if arr[pos] > arr[parent_pos]:
            arr[pos], arr[parent_pos] = arr[parent_pos], arr[pos]
            pos = parent_pos
        else:
            break
    return pos

def sink_down(arr, pos):
    while pos < len(arr):
        largest_pos = pos
        left_pos = left_child_0(pos)
        right_pos = right_child_0(pos)
        if left_pos < len(arr) and arr[left_pos] > arr[largest_pos]:
            largest_pos = left_pos
        if right_pos < len(arr) and arr[right_pos] > arr[largest_pos]:
            largest_pos = right_pos
        if largest_pos == pos:
            break
        else:
            arr[pos], arr[largest_pos] = arr[largest_pos], arr[pos]
            pos = largest_pos
    return pos

class MaxHeap:

    def __init__(self, init=None):
        self.data = []
        if init is not None:
            for e in init:
                self.insert(e)

    def __len__(self):
        return len(self.data)

    def __repr__(self):
        return "<MaxHeap(data={})>".format(repr(self.data))

    def is_valid_heap(self):
        n = len(self.data)
        for i in range(parent_0(n - 1), -1, -1):
            l = left_child_0(i)
            r = right_child_0(i)
            if l < n and self.data[l] > self.data[i] or r < n and self.data[r] > self.data[i]:
                return False
        return True

    def insert(self, e):
        self.data.append(e)
        pos = bubble_up(self.data, len(self.data) - 1)

    def peek(self):
        return self.data[0]

    def pop(self):
        small = self.data.pop()
        if self.data:
            biggest, self.data[0] = self.data[0], small
            pos = sink_down(self.data, 0)
            return biggest
        else:
            return small

@given(st.lists(st.integers()))
def test_max_heap(xs):
    h = MaxHeap()
    for x in xs:
        h.insert(x)
        assert h.is_valid_heap()
    out = []
    while h:
        out.append(h.pop())
        assert h.is_valid_heap()
    assert out == sorted(xs, reverse=True), xs

if __name__ == '__main__':
    # To run tests: python3 py_src/binary_heap.py
    test_max_heap()

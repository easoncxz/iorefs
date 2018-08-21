
import random
import unittest

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

def internal_node_indices(arr):
    return range(parent_0(len(arr) - 1), -1, -1)

def make_heap(l):
    for i in internal_node_indices(l):
        sink_down(l, i)
    return l

def is_valid_heap(arr):
    for i in internal_node_indices(arr):
        n = len(arr)
        l = left_child_0(i)
        r = right_child_0(i)
        if l < n and arr[l] > arr[i] or r < n and arr[r] > arr[i]:
            return False
    return True

class MaxHeap:

    def __init__(self, init=None):
        self.data = make_heap([] if init is None else list(init))

    def __len__(self):
        return len(self.data)

    def __repr__(self):
        return "<MaxHeap(data={})>".format(repr(self.data))

    def insert(self, e):
        self.data.append(e)
        bubble_up(self.data, len(self.data) - 1)

    def peek(self):
        return self.data[0] # may raise IndexError

    def pop(self):
        small = self.data.pop()
        if self.data:
            biggest, self.data[0] = self.data[0], small
            pos = sink_down(self.data, 0)
            return biggest
        else:
            return small

    def empty_into(self, l):
        del l[:]
        while self.data:
            l.append(self.pop())

class TestMaxHeap(unittest.TestCase):

    @given(st.lists(st.integers()))
    def test_one_by_one_inserting_and_deleting(self, xs):
        h = MaxHeap()
        for x in xs:
            h.insert(x)
            assert is_valid_heap(h.data)
        out = []
        while h:
            out.append(h.pop())
            assert is_valid_heap(h.data)
        assert out == sorted(xs, reverse=True), xs

    @given(st.lists(st.integers()))
    def test_batched_inserting_and_deleting(self, xs):
        h = MaxHeap(init=xs)
        assert is_valid_heap(h.data)
        out = []
        h.empty_into(out)
        assert out == sorted(xs, reverse=True), xs

if __name__ == '__main__':
    # To run tests: python3 py_src/binary_heap.py
    unittest.main()

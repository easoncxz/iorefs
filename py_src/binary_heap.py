
import operator
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

def identity(x):
    return x

def bubble_up(arr, pos, prevails):
    while pos > 0:
        parent_pos = parent_0(pos)
        if prevails(arr[pos], arr[parent_pos]):
            arr[pos], arr[parent_pos] = arr[parent_pos], arr[pos]
            pos = parent_pos
        else:
            break
    return pos

def sink_down(arr, pos, prevails):
    while pos < len(arr):
        largest_pos = pos
        left_pos = left_child_0(pos)
        right_pos = right_child_0(pos)
        if left_pos < len(arr) and prevails(arr[left_pos], arr[largest_pos]):
            largest_pos = left_pos
        if right_pos < len(arr) and prevails(arr[right_pos], arr[largest_pos]):
            largest_pos = right_pos
        if largest_pos == pos:
            break
        else:
            arr[pos], arr[largest_pos] = arr[largest_pos], arr[pos]
            pos = largest_pos
    return pos

def internal_node_indices(arr):
    return range(parent_0(len(arr) - 1), -1, -1)

def make_heap(l, prevails):
    for i in internal_node_indices(l):
        sink_down(l, i, prevails)
    return l

def is_valid_heap(arr, prevails):
    for i in internal_node_indices(arr):
        n = len(arr)
        l = left_child_0(i)
        r = right_child_0(i)
        if l < n and prevails(arr[l], arr[i]) or (
                r < n and prevails(arr[r], arr[i])):
            return False
    return True

class Heap:

    def __init__(self, init=None, max_heap=True):
        self.is_max_heap = max_heap
        self.prevails = operator.gt if max_heap else operator.lt
        self.data = make_heap(
                [] if init is None else list(init),
                self.prevails)

    def __len__(self):
        return len(self.data)

    def __repr__(self):
        return "<Heap(is_max_heap={}, data={})>".format(
                repr(self.is_max_heap),
                repr(self.data))

    def insert(self, e):
        self.data.append(e)
        bubble_up(self.data, len(self.data) - 1, self.prevails)

    def peek(self):
        return self.data[0] # may raise IndexError

    def pop(self):
        small = self.data.pop()
        if self.data:
            biggest, self.data[0] = self.data[0], small
            pos = sink_down(self.data, 0, self.prevails)
            return biggest
        else:
            return small

    def empty_into(self, l):
        del l[:]
        while self.data:
            l.append(self.pop())

MaxHeap = Heap

class MinHeap(Heap):

    def __init__(self, *args, **kwargs):
        super(MinHeap, self).__init__(*args, max_heap=False, **kwargs)

class TestMaxHeap(unittest.TestCase):

    @given(st.lists(st.integers()))
    def test_one_by_one_inserting_and_deleting(self, xs):
        h = MaxHeap()
        for x in xs:
            h.insert(x)
            assert is_valid_heap(h.data, h.prevails)
        out = []
        while h:
            out.append(h.pop())
            assert is_valid_heap(h.data, h.prevails)
        assert out == sorted(xs, reverse=True), xs

    @given(st.lists(st.integers()))
    def test_batched_inserting_and_deleting(self, xs):
        h = MaxHeap(init=xs)
        assert is_valid_heap(h.data, h.prevails)
        out = []
        h.empty_into(out)
        assert out == sorted(xs, reverse=True), xs

    @given(st.lists(st.integers()))
    def test_max_heap_as_min_heap(self, xs):
        h = MinHeap(init=xs)
        assert is_valid_heap(h.data, h.prevails)
        out = []
        h.empty_into(out)
        assert out == sorted(xs), xs

if __name__ == '__main__':
    # To run tests: python3 py_src/binary_heap.py
    unittest.main()

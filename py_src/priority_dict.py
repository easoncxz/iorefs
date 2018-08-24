
import operator
import unittest

from hypothesis import given, assume
import hypothesis.strategies as st

import binary_heap

def identity(x):
    return x

class Comparable:
    ''' Mixin from regebro's blog from 2010.

    Uses `self._cmpkey()` as comparison key.
    '''

    def _compare(self, other, op):
        try:
            return op(self._cmpkey(), other._cmpkey())
        except (AttributeError, TypeError):
            raise NotImplemented

    def __eq__(self, other):
        return self._compare(other, operator.eq)

    def __ne__(self, other):
        return self._compare(other, operator.ne)

    def __lt__(self, other):
        return self._compare(other, operator.lt)

    def __le__(self, other):
        return self._compare(other, operator.le)

    def __gt__(self, other):
        return self._compare(other, operator.gt)

    def __ge__(self, other):
        return self._compare(other, operator.ge)

class KeyPriority(Comparable):
    ''' A wrapper that gets comparison from the priorities '''

    def __init__(self, k, p):
        self.key = k
        self.priority = p

    def __repr__(self):
        return "<KeyPriority(key={}, priority={})>".format(
                repr(self.key),
                repr(self.priority))

    def __neg__(self):
        return KeyPriority(self.key, -self.priority)

    def _cmpkey(self):
        return self.priority

class PriorityDict:
    ''' A dict ordered by ascending values
    '''

    def __init__(self, init=None):
        self.prevails = operator.lt
        self.data = [] if init is None else (
            binary_heap.make_heap(
                [KeyPriority(k, p) for k, p in init.items()],
                self.prevails))
        self.lookup = {kp.key: i for i, kp in enumerate(self.data)}

    def copy(self):
        pd = PriorityDict()
        pd.prevails = self.prevails
        pd.data = [kp for kp in self.data]
        pd.lookup = {kp.key: i for i, kp in enumerate(self.data)}
        return pd

    def __len__(self):
        return len(self.data)

    def __repr__(self):
        return "<PriorityDict(data={}, lookup={})>".format(
                repr(self.data),
                repr(self.lookup))

    def __setitem__(self, k, p):
        ix = self.lookup.get(k)
        if ix is None:
            self.data.append(KeyPriority(k, p))
            binary_heap.bubble_up(self.data, len(self.data) - 1, self.prevails)
        else:
            old = self.data[ix]
            self.data[ix] = p
            if self.prevails(p, old):
                binary_heap.bubble_up(self.data, ix, self.prevails)
            else:
                binary_heap.sink_down(self.data, ix, self.prevails)

    def __getitem__(self, k):
        return self.data[self.lookup[k]].priority

    def __delitem__(self, k):
        pass    # tricky; first bubble_up then pop

    def __iter__(self):
        # O(n) memory overhead, not smart
        # O(n * log(n)) runtime, like heapsort
        shallow = self.copy()
        while shallow:
            yield shallow.pop().key

    def peek(self):
        return self.data[0]

    def pop(self):
        leaf = self.data.pop()
        if self.data:
            top = self.data[0]
            self.data[0] = leaf
            binary_heap.sink_down(self.data, 0, self.prevails)
            return top
        else:
            return leaf

class TestPriorityDict(unittest.TestCase):

    @given(st.lists(st.integers()))
    def test_one_by_one_inserting_and_deleting(self, xs):
        pd = PriorityDict()
        for x in xs:
            pd[x] = x
            assert binary_heap.is_valid_heap(pd.data, pd.prevails), pd
        out = []
        while pd:
            out.append(pd.pop().key)
            assert binary_heap.is_valid_heap(pd.data, pd.prevails), pd
        assert out == sorted(xs), xs

    @given(st.lists(st.integers()))
    def test_iteration_order(self, xs):
        pd = PriorityDict({x: x for x in xs})
        assert list(pd) == sorted(set(xs)), xs

    @given(st.integers())
    def test_inserting_duplicates(self, x):
        pd = PriorityDict()
        pd[x] = x
        pd[x] = x # again
        assert len(pd) == 1, pd

if __name__ == '__main__':
    unittest.main()


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

def lookup_matches_array(lookup, arr):
    try:
        for k in lookup:
            if arr[lookup[k]].key != k:
                return False
        for i, kp in enumerate(arr):
            if lookup[kp.key] != i:
                return False
    except KeyError:
        return False
    return True

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
        fresh = PriorityDict()
        fresh.prevails = self.prevails
        fresh.data = list(self.data)
        fresh.lookup = dict(self.lookup)
        return fresh

    def _swap_with_lookup_update(self, arr, i, j):
        arr[i], arr[j] = arr[j], arr[i]
        self.lookup[arr[i].key], self.lookup[arr[j].key] = i, j

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
            self.lookup[k] = len(self.data) - 1
            binary_heap.bubble_up(
                    self.data,
                    len(self.data) - 1,
                    self.prevails,
                    swap=self._swap_with_lookup_update)
        else:
            old_kp = self.data[ix]
            new_kp = KeyPriority(k, p)
            self.data[ix] = new_kp
            if self.prevails(new_kp, old_kp):
                propagate = binary_heap.bubble_up
            else:
                propagate = binary_heap.sink_down
            propagate(
                self.data,
                ix,
                self.prevails,
                swap=self._swap_with_lookup_update)

    def __getitem__(self, k):
        return self.data[self.lookup[k]].priority

    def __delitem__(self, k):
        pass    # tricky; first bubble_up then pop

    def __iter__(self):
        ''' Yields keys in priority order '''
        for k, _ in self.items():
            yield k

    def items(self):
        ''' Yields key-priority pairs in priority order; good with `dict` '''
        # O(n) memory overhead, not smart
        # O(n * log(n)) runtime, like heapsort
        shallow = self.copy()
        while shallow:
            kp = shallow.pop()
            yield kp.key, kp.priority

    def peek(self):
        return self.data[0]

    def pop(self):
        leaf = self.data.pop()
        if self.data:
            top = self.data[0]
            self.data[0] = leaf
            del self.lookup[top.key]
            self.lookup[leaf.key] = 0
            binary_heap.sink_down(
                    self.data,
                    0,
                    self.prevails,
                    swap=self._swap_with_lookup_update)
            return top
        else:
            del self.lookup[leaf.key]
            return leaf

class TestPriorityDict(unittest.TestCase):

    def _one_by_one(self, xs, pd_predicate):
        pd = PriorityDict()
        for x in xs:
            pd[x] = x
            pd_predicate(pd)
        out = []
        while pd:
            out.append(pd.pop().key)
            pd_predicate(pd)
        assert out == sorted(out), xs

    @given(st.lists(st.integers()))
    def test_one_by_one_inserting_and_deleting_maintains_heap_propperty(self, xs):
        def has_valid_heap(pd):
            assert binary_heap.is_valid_heap(pd.data, pd.prevails), pd
        self._one_by_one(xs, has_valid_heap)

    @given(st.lists(st.integers()))
    def test_one_by_one_inserting_and_deleting_maintains_correct_lookup_table(self, xs):
        def has_valid_lookup(pd):
            assert lookup_matches_array(pd.lookup, pd.data), pd
        self._one_by_one(xs, has_valid_lookup)

    @given(st.lists(st.integers()))
    def test_iteration_gives_unique_items_in_asc_sorted_order(self, xs):
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

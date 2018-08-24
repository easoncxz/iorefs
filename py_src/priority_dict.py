
import operator
import unittest

from hypothesis import given, assume
import hypothesis.strategies as st

import binary_heap

def identity(x):
    return x

def noop(*args, **kwargs):
    pass

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
        target_ix = self.lookup[k]
        last_ix = len(self.data) - 1
        target = self.data[target_ix]
        last = self.data[last_ix]
        self._swap_with_lookup_update(self.data, target_ix, last_ix)
        del self.lookup[k]
        self.data.pop()
        if self.prevails(last, target):
            propagate = binary_heap.bubble_up
        elif self.prevails(target, last):
            propagate = binary_heap.sink_down
        else:
            propagate = noop
        propagate(
                self.data,
                target_ix,
                self.prevails,
                swap=self._swap_with_lookup_update)

    def __elem__(self, k):
        return k in self.lookup

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
        target_ix = 0
        last_ix = len(self.data) - 1
        self._swap_with_lookup_update(self.data, target_ix, last_ix)
        target = self.data.pop()
        del self.lookup[target.key]
        binary_heap.sink_down(
                self.data,
                target_ix,
                self.prevails,
                swap=self._swap_with_lookup_update)
        return target

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
        assert out == sorted(out), (xs, out)

    def _priority_dict_from_list(self, xs):
        return PriorityDict({x: x for x in xs})

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
        pd = self._priority_dict_from_list(xs)
        assert list(pd) == sorted(set(xs)), xs

    @given(st.lists(st.integers()), st.integers())
    def test_write_invariant(self, xs, x):
        pd = self._priority_dict_from_list(xs)
        pd[x] = x
        assert pd[x] == x, pd

    @given(st.lists(st.integers()), st.integers())
    def test_inserting_duplicates(self, xs, x):
        pd = self._priority_dict_from_list(xs)
        pd[x] = x
        pd[x] = x # again
        uniq = set(xs).union({x})
        assert len(pd) == len(uniq), pd

    @given(st.lists(st.integers()), st.integers())
    def test_inserting_results_in_correct_len(self, xs, x):
        pd = self._priority_dict_from_list(xs)
        pd[x] = x
        uniq = set(xs)
        if x in uniq:
            assert len(pd) == len(uniq), (xs, x, pd)
        else:
            assert len(pd) == len(uniq) + 1, (xs, x, pd)

    @given(st.lists(st.integers()), st.integers())
    def test_delete_invariant(self, xs, x):
        uniq = set(xs)
        pd = self._priority_dict_from_list(xs)
        if x in uniq:
            del pd[x]
        assert x not in pd, (xs, x, pd)

if __name__ == '__main__':
    unittest.main()

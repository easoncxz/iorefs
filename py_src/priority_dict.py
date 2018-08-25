
import unittest
import random

from hypothesis import given, assume
import hypothesis.strategies as st

import binary_heap

def noop(*args, **kwargs):
    pass

def key(kp):
    k, _ = kp
    return k

def priority(kp):
    _, p = kp
    return p

def kp_prevails(kp1, kp2):
    _, p1 = kp1
    _, p2 = kp2
    return p1 < p2

class PriorityDict:
    ''' A dict ordered by ascending values '''

    def __init__(self, init=None):
        self.prevails = kp_prevails
        self.data = [] if init is None else (
            binary_heap.make_heap(
                list(init.items()),
                self.prevails))
        self.lookup = {k: i for i, (k, _) in enumerate(self.data)}

    def __repr__(self):
        return "<PriorityDict(data={}, lookup={})>".format(
                repr(self.data),
                repr(self.lookup))

    def __len__(self):
        return len(self.data)

    def __elem__(self, k):
        return k in self.lookup

    def _swap_with_lookup(self, arr, i, j):
        ''' Swap elements in the given array while maintaining `lookup` to be valid '''
        arr[i], arr[j] = arr[j], arr[i]
        (ki, _), (kj, _) = arr[i], arr[j]
        self.lookup[ki], self.lookup[kj] = i, j

    def _append_with_lookup(self, k, p):
        ''' Append an element at the end of the array while maintianing '''
        self.data.append((k, p))
        self.lookup[k] = len(self.data) - 1

    def _pop_with_lookup(self):
        ''' Pop the last item of the array '''
        k, p = self.data.pop()
        del self.lookup[k]
        return k, p

    def _fixup(self, ix):
        binary_heap.fixup(self.data, ix, self.prevails, swap=self._swap_with_lookup)

    def get(self, k):
        if k not in self:
            return None
        else:
            return self[k]

    def __getitem__(self, k):
        _, p = self.data[self.lookup[k]]
        return p

    def __setitem__(self, k, p):
        ix = self.lookup.get(k)
        if ix is None:
            self._append_with_lookup(k, p)
            ix = len(self.data) - 1
        else:
            self.data[ix] = k, p
        self._fixup(ix)

    def __delitem__(self, k):
        self.pop(key=k)

    def __iter__(self):
        ''' Yields keys in priority order '''
        for k, _ in self.items():
            yield k

    def copy(self):
        fresh = PriorityDict()
        fresh.prevails = self.prevails
        fresh.data = list(self.data)
        fresh.lookup = dict(self.lookup)
        return fresh

    def items(self):
        ''' Yields key-priority pairs in priority order; good with `dict` '''
        # O(n) memory overhead, not smart
        # O(n * log(n)) runtime, like heapsort
        shallow = self.copy()
        while shallow:
            yield shallow.pop()

    def to_dict(self):
        return {k: priority(self.data[ix]) for k, ix in self.lookup.items()}

    def __eq__(self, other):
        if not isinstance(other, PriorityDict):
            raise TypeError("{} is not a PriorityDict".format(repr(other)))
        return self.to_dict() == other.to_dict()

    def peek(self):
        k, p = self.data[0]
        return k, p

    def pop(self, key=None):
        ix = 0 if key is None else self.lookup[key]
        self._swap_with_lookup(self.data, ix, len(self.data) - 1)
        target = self._pop_with_lookup()
        self._fixup(ix)
        return target

def lookup_matches_array(lookup, arr):
    if len(lookup) != len(arr):
        return False
    for k in lookup:
        if key(arr[lookup[k]]) != k:
            return False
    for i, kp in enumerate(arr):
        if lookup[key(kp)] != i:
            return False
    return True

class TestPriorityDict(unittest.TestCase):

    def _priority_dict_from_list(self, xs):
        return PriorityDict({x: x for x in xs})

    def _assert_valid_pd(self, pd):
        assert binary_heap.is_valid_heap(pd.data, pd.prevails), pd
        assert lookup_matches_array(pd.lookup, pd.data), pd

    @given(st.lists(st.integers()))
    def test_one_by_one_inserting_and_deleting(self, xs):
        pd = PriorityDict()
        for x in xs:
            pd[x] = x
            self._assert_valid_pd(pd)
        out = []
        while pd:
            out.append(key(pd.pop()))
            self._assert_valid_pd(pd)
        assert out == sorted(set(xs)), (xs, out)

    @given(st.lists(st.integers()))
    def test_iteration_gives_unique_items_in_asc_sorted_order(self, xs):
        pd = self._priority_dict_from_list(xs)
        assert list(pd) == sorted(set(xs)), xs

    @given(st.lists(st.integers()), st.integers())
    def test_write_invariant(self, xs, x):
        pd = self._priority_dict_from_list(xs)
        pd[x] = x
        assert pd[x] == x, pd
        assert x in pd, (xs, x, pd)
        self._assert_valid_pd(pd)

    @given(st.lists(st.integers()), st.integers())
    def test_inserting_duplicates(self, xs, x):
        pd = self._priority_dict_from_list(xs)
        pd[x] = x
        pd[x] = x # again
        uniq = set(xs).union({x})
        assert len(pd) == len(uniq), pd
        self._assert_valid_pd(pd)

    @given(st.lists(st.integers()), st.integers())
    def test_inserting_results_in_correct_len(self, xs, x):
        pd = self._priority_dict_from_list(xs)
        pd[x] = x
        uniq = set(xs)
        if x in uniq:
            assert len(pd) == len(uniq), (xs, x, pd)
        else:
            assert len(pd) == len(uniq) + 1, (xs, x, pd)
        self._assert_valid_pd(pd)

    @given(st.lists(st.integers()), st.integers())
    def test_delete_invariant(self, xs, x):
        uniq = set(xs)
        pd = self._priority_dict_from_list(xs)
        assert (x in uniq) == (x in pd), (xs, x, pd)
        if x in uniq:
            del pd[x]
        assert x not in pd, (xs, x, pd)
        self._assert_valid_pd(pd)

    @given(st.dictionaries(st.characters(), st.integers()))
    def test_dictionary_roundtrip_identity(self, d):
        pd = PriorityDict(d)
        self._assert_valid_pd(pd)
        from_pd = dict(pd.items())
        back_into_pd = PriorityDict(from_pd)
        assert from_pd == d, (d, pd)
        assert back_into_pd == pd, (back_into_pd, pd, d)

    @given(st.dictionaries(st.integers(), st.integers()), st.integers())
    def test_dictionary_lookup_equivalence(self, d, x):
        pd = PriorityDict(d)
        assert pd.to_dict() == d, (pd, d)
        assert pd.get(x) == d.get(x), (pd, d, x)

    @given(st.dictionaries(st.integers(), st.integers()))
    def test_to_dict_matches_iteration(self, d):
        pd = PriorityDict(d)
        assert pd.to_dict() == dict(d.items()), (d, pd)

    @given(st.lists(st.integers()))
    def test_differently_ordered_priority_dicts_are_equivalent(self, xs):
        different = list(xs)
        random.shuffle(different)
        assume(different != xs)
        one = self._priority_dict_from_list(xs)
        other = self._priority_dict_from_list(different)
        assume(one.data != other.data)
        assume(one.lookup != other.lookup)
        while one and other:
            assert one == other, (one, other, xs, different)
            x = one.pop()
            y = other.pop()
            assert x == y, (x, y, one, other, xs, different)
        assert not one and not other, (one, other, xs, different)

if __name__ == '__main__':
    unittest.main()

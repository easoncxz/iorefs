
import unittest

from hypothesis import given, assume
import hypothesis.strategies as st

from comparable_mixin import Comparable
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

def lookup_matches_array(lookup, arr):
    try:
        for k in lookup:
            if key(arr[lookup[k]]) != k:
                return False
        for i, kp in enumerate(arr):
            if lookup[key(kp)] != i:
                return False
    except KeyError:
        return False
    return True

class PriorityDict:
    ''' A dict ordered by ascending values
    '''

    def __init__(self, init=None):
        self.prevails = kp_prevails
        self.data = [] if init is None else (
            binary_heap.make_heap(
                list(init.items()),
                self.prevails))
        self.lookup = {k: i for i, (k, _) in enumerate(self.data)}

    def copy(self):
        fresh = PriorityDict()
        fresh.prevails = self.prevails
        fresh.data = list(self.data)
        fresh.lookup = dict(self.lookup)
        return fresh

    def _swap_with_lookup_update(self, arr, i, j):
        arr[i], arr[j] = arr[j], arr[i]
        (ki, _), (kj, _) = arr[i], arr[j]
        self.lookup[ki], self.lookup[kj] = i, j

    def __len__(self):
        return len(self.data)

    def __repr__(self):
        return "<PriorityDict(data={}, lookup={})>".format(
                repr(self.data),
                repr(self.lookup))

    def __setitem__(self, k, p):
        ix = self.lookup.get(k)
        if ix is None:
            self.data.append((k, p))
            self.lookup[k] = len(self.data) - 1
            binary_heap.bubble_up(
                    self.data,
                    len(self.data) - 1,
                    self.prevails,
                    swap=self._swap_with_lookup_update)
        else:
            old_kp = self.data[ix]
            new_kp = (k, p)
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
        _, p = self.data[self.lookup[k]]
        return p

    def __delitem__(self, k):
        self.pop(key=k)

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
            yield shallow.pop()

    def peek(self):
        k, p = self.data[0]
        return k, p

    def pop(self, key=None):
        target_ix = 0 if key is None else self.lookup[key]
        last_ix = len(self.data) - 1
        last = self.data[last_ix]
        self._swap_with_lookup_update(self.data, target_ix, last_ix)
        target = self.data.pop()
        k, _ = target
        del self.lookup[k]
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
        return target

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
        if x in uniq:
            del pd[x]
        assert x not in pd, (xs, x, pd)
        self._assert_valid_pd(pd)

if __name__ == '__main__':
    unittest.main()

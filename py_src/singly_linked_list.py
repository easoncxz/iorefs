
from typing import Generic, TypeVar, Optional, List, Sequence, Callable, Iterator
import unittest

from hypothesis import given, assume
from hypothesis import strategies as st

N = TypeVar('N')
X = TypeVar('X')
Z = TypeVar('Z')

class Node(Generic[N]):
    def __init__(self, data: N, succ: 'Optional[Node[N]]') -> None:
        self.data = data
        self.succ = succ

    def __repr__(self):
        return repr(list(self))

    def __iter__(self):
        curr = self
        while curr is not None:
            yield curr.data
            curr = curr.succ

LinkedList = Optional[Node[N]]

def from_list(xs: List[N]) -> LinkedList[N]:
    head: LinkedList[N] = None
    for x in reversed(xs):
        head = Node(x, head)
    return head

def reverse(l: LinkedList[N]) -> LinkedList[N]:
    current = l
    tail: LinkedList[N] = None
    while current is not None:
        tail = Node(current.data, tail)
        current = current.succ
    return tail

def foldr(f: Callable[[X, Z], Z], z: Z, xs: Iterator[X]) -> Z:
    ''' (Not tail-recursive; will blow the call-stack) '''
    try:
        x = next(xs)
        return f(x, foldr(f, z, xs))
    except StopIteration:
        return z


class LinkedListTests(unittest.TestCase):
    @given(st.lists(st.integers()))
    def test_from_list(self, xs: List[int]) -> None:
        ll = from_list(xs)
        if xs:
            assert repr(ll) == repr(xs), repr(xs)
        else:
            assert repr(ll) == 'None'



if __name__ == '__main__':
    unittest.main()

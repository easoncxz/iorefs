from pprint import pprint
from collections import deque
from typing import TypeVar, Iterable, Iterator, Generic, Hashable, Dict, List, Tuple, Callable, Any, Set, Type
from typing_extensions import Protocol
from abc import abstractmethod

N = TypeVar('N', bound=Hashable)
E = TypeVar('E')
Unit = Tuple[()]


class Graph(Generic[N, E]):
    ''' A nested-dict matrix representation of a directed graph

    nodes: iterable of hashables
    edges: iterable of pairs of nodes
    '''

    def __init__(self, nodes: Iterable[N],
                 edges: Iterable[Tuple[N, N, E]]) -> None:
        self.matrix: Dict[N, Dict[N, E]] = {
            from_node: dict()
            for from_node in nodes
        }
        for (f, t, dist) in edges:
            self.matrix[f][t] = dist

    def out_nodes(self, from_node: N) -> Iterable[N]:
        return (
            n for n, dist in self.matrix[from_node].items()
            if dist is not None)


sample_graph = Graph[int, Unit](
    range(9),
    [
        (1, 2, ()),
        (2, 3, ()),
        (3, 4, ()),
        (1, 3, ()),
        (3, 6, ()),
        (1, 5, ()),
        (5, 3, ()),
        (5, 6, ()),
        (6, 7, ()),
        (7, 3, ()),
        (5, 8, ()),
    ],
)

class Todo(Protocol[N]):
    @abstractmethod
    def __init__(self, init: Iterable[N] = None) -> None:
        pass

    @abstractmethod
    def add(self, elem: N) -> None:
        pass

    @abstractmethod
    def remove(self) -> N:
        pass

class TodoStack(Generic[N]):
    def __init__(self, *args):
        self.coll = deque(*args)

    def __len__(self):
        return len(self.coll)

    def add(self, elem: N) -> None:
        self.coll.append(elem)

    def remove(self) -> N:
        return self.coll.pop()


class TodoQueue(Generic[N]):
    def __init__(self, *args):
        self.coll = deque(*args)

    def __len__(self):
        return len(self.coll)

    def add(self, elem: N) -> None:
        self.coll.append(elem)

    def remove(self) -> N:
        return self.coll.popleft()


def search(Todo: Type[Todo[N]], graph, start) -> Iterator[N]:
    seen_nodes: Set[N] = set()
    todo = Todo([start])
    while todo:
        node = todo.remove()
        if node not in seen_nodes:
            seen_nodes.add(node)
            yield node
        for to_node in graph.out_nodes(node):
            if to_node not in seen_nodes:
                todo.add(to_node)


def depth_first_search(graph: Graph[N, E], start: N) -> Iterator[N]:
    '''
    >>> list(depth_first_search(sample_graph, 1))
    [1, 5, 8, 6, 7, 3, 4, 2]
    '''
    return search(TodoStack, graph, start)


def breadth_first_search(graph, start):
    '''
    >>> list(breadth_first_search(sample_graph, 1))
    [1, 2, 3, 5, 4, 6, 8, 7]
    '''
    return search(TodoQueue, graph, start)

from pprint import pprint
from collections import deque


class Graph:
    ''' A nested-dict matrix representation of a directed graph

    nodes: iterable of hashables
    edges: iterable of pairs of nodes
    '''

    def __init__(self, nodes, edges):
        self.matrix = {
            from_node: {to_node: None
                        for to_node in nodes}
            for from_node in nodes
        }
        for (f, t, dist) in edges:
            self.matrix[f][t] = dist

    def out_nodes(self, from_node):
        return (
            n for n, dist in self.matrix[from_node].items()
            if dist is not None)


sample_graph = Graph(
    range(9), [
        (1, 2, True),
        (2, 3, True),
        (3, 4, True),
        (1, 3, True),
        (3, 6, True),
        (1, 5, True),
        (5, 3, True),
        (5, 6, True),
        (6, 7, True),
        (7, 3, True),
        (5, 8, True),
    ])


class TodoStack:
    def __init__(self, *args):
        self.coll = deque(*args)

    def __len__(self):
        return len(self.coll)

    def add(self, elem):
        self.coll.append(elem)

    def remove(self):
        return self.coll.pop()


class TodoQueue:
    def __init__(self, *args):
        self.coll = deque(*args)

    def __len__(self):
        return len(self.coll)

    def add(self, elem):
        self.coll.append(elem)

    def remove(self):
        return self.coll.popleft()


def search(Todo, graph, start):
    seen_nodes = set()
    todo = Todo([start])
    while todo:
        node = todo.remove()
        if node not in seen_nodes:
            seen_nodes.add(node)
            yield node
        for to_node in graph.out_nodes(node):
            if to_node not in seen_nodes:
                todo.add(to_node)


def depth_first_search(graph, start):
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

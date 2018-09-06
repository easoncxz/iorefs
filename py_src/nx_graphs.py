
import math
import random

import networkx as nx
from networkx.classes.graph import Graph
import matplotlib.pyplot as plt

from graph_search import Todo, TodoStack, TodoQueue, sample_graph_edges

def generate_random_graph(node_count: int = 10) -> Graph:
    edge_count = min(node_count ** 2, math.ceil(2 * node_count))
    g = nx.gnm_random_graph(node_count, edge_count)
    for u, v in g.edges:
        g[u][v]['weight'] = random.randrange(20)
    return g

def show_graph(graph):
    nx.draw(graph, with_labels=True, font_weight='bold')
    plt.show()

def search(Todo, graph, start, visit=None):
    if visit is None:
        visit = lambda *args, **kwargs: None
    seen_nodes = set()
    todo = Todo([(start, None)])
    tree = nx.Graph()
    while todo:
        node, maybe_data = todo.pop()
        if node not in seen_nodes:
            seen_nodes.add(node)
            visit(node)
            tree.add_node(node)
            if maybe_data is not None:
                from_node, edge = maybe_data
                tree.add_edge(from_node, node)
                tree[from_node][node]['weight'] = edge
            for neighbour in sorted(graph[node]):
                todo.add(neighbour, (node, graph[node][neighbour].get('weight')))
    return tree

sample_graph = nx.Graph()
sample_graph.add_edges_from(sample_graph_edges)

def depth_first_search(graph, start):
    '''
    >>> journey, tree = depth_first_search(sample_graph, 1)
    >>> journey
    [1, 5, 8, 6, 7, 3, 4, 2]
    '''
    journey = []
    tree = search(TodoStack, graph, start, visit=journey.append)
    return journey, tree

def breadth_first_search(graph, start):
    '''
    >>> journey, tree = breadth_first_search(sample_graph, 1)
    >>> journey
    [1, 2, 3, 5, 4, 6, 7, 8]

    Notice strange quirk about the flipped order of 7 and 8 compared to graph_search.py
    '''
    journey = []
    tree = search(TodoQueue, graph, start, visit=journey.append)
    return journey, tree

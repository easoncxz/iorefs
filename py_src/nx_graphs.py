
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

def search(Todo, graph, start):
    seen_nodes = set()
    todo = Todo([(start, None)])
    while todo:
        node, src_data = todo.pop()
        if node not in seen_nodes:
            seen_nodes.add(node)
            yield node
            for neighbour in sorted(graph[node]):
                todo.add(neighbour, (node, graph[node][neighbour].get('weight')))

sample_graph = nx.Graph()
sample_graph.add_edges_from(sample_graph_edges)

def depth_first_search(graph, start):
    '''
    >>> list(depth_first_search(sample_graph, 1))
    [1, 5, 8, 6, 7, 3, 4, 2]
    '''
    return search(TodoStack, graph, start)

def breadth_first_search(graph, start):
    '''
    >>> list(depth_first_search(sample_graph, 1))
    [1, 5, 8, 6, 7, 3, 4, 2]
    '''
    return search(TodoQueue, graph, start)

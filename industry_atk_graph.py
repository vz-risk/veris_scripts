DESCRIPTION = "Industry attack graph analysis."

import networkx as nx
import copy
import matplotlib.pyplot as plt
from prettytable import PrettyTable

# Download the correct attack graph from https://github.com/vz-risk/VERISAG/tree/v2/static.  The numbers are the first 2 numbers of the NAICS code. Download the gexf version.
GRAPH_FILE = "LOCATION YOU SAVED THE GEXF FILE TO"

# Set locations
LOCATION = "~/Documents/Development/VERISAG/"
FILTERS = "~/Documents/Development/VERISAG/filter.txt"
#DATA_LOCATION = "~/Documents/Data/VCDB/data/json"  # not using since we already have the graph

# Load VERISAG Module
import imp
fp, pathname, description = imp.find_module("V2AG", [LOCATION])
V2AG = imp.load_module("V2AG", fp, pathname, description)
analysis = V2AG.attack_graph_analysis.analyze()

# Create the attack graph object
DBIRAG = V2AG.attack_graph(None, filter_file=FILTERS, build=False)
DBIRAG.g = nx.read_gexf(GRAPH_FILE)

# Ensure edge weights are populated
DBIRAG.g = DBIRAG.normalize_weights(DBIRAG.g)

# Print top 10 attack paths
paths = analysis.helper.shortest_attack_paths(DBIRAG.g, src=None, dst=None)
sorted_paths = list()
for path in paths.keys():
    if len(paths[path]) > 0:
        sorted_paths.append((path, paths[path], analysis.helper.path_length(DBIRAG.g, paths[path])[1]))
sorted_paths.sort(key=lambda tup: tup[2], reverse=False)
for path in [x[1] for x in sorted_paths[:10]]:
    p = path[0]
    for node in path[1:]:
        p = p + " -> " + node
    print p


# Best Nodes to Mitigate and Relative Change (from  blog)
# Initialize Variables
shortest_paths = list()
mitigations = list()
action="start"
attribute = "end"
cutoff = 6
done = False
g_copy = copy.deepcopy(DBIRAG.g)
l = 1

# Run LookupError
try:
    path = nx.dijkstra_path(g_copy,action,attribute,'weight')
except nx.NetworkXNoPath:
    done = True
while not done:
    # since as you remove paths, the paths will get longer, the cutoff needs to be continuously increased
    if len(path) != l:
        l = len(path)
        print(l)

    if len(path) > 4:
        cutoff = len(path) + 2

    # get shortest path and add to distrubion
    _, length = analysis.helper.path_length(g_copy, path)
    shortest_paths.append(length)


    #Mitigate a node
    direct, nodes = analysis.one_graph_one_path(g_copy, action, attribute, cutoff=cutoff, output="return")
    if direct:
        g_copy.remove_edge(action, attribute)
        mitigations.append((action, attribute))
    else:
        node_to_mitigate = nodes.pop()
        g_copy.remove_node(node_to_mitigate)
        mitigations.append(node_to_mitigate)

    # retest
    try:
        path = nx.dijkstra_path(g_copy,action,attribute,'weight')
    except nx.NetworkXNoPath:
        done = True
shortest_paths = [x - 2 for x in shortest_paths]
x = range(len(shortest_paths))

# Print Best Mitigations
tbl = PrettyTable(["Mitigation", "Attacker Cost"])
tbl.align['Mitigation'] = 'l'
tbl.align['Attacker Cost'] = 'l'
tbl.add_row(["No Mitigation", round(shortest_paths[0], 4)])
for i in range(len(shortest_paths)-1):
    tbl.add_row([mitigations[i], round(shortest_paths[i+1], 4)])
print tbl

# Plot the results
plt.figure(facecolor="white")
plt.plot(x, shortest_paths, 'bo', x, shortest_paths, 'k')
plt.title("Relative Shortest Path Length Increase From Mitigation")
plt.xlabel("Mitigation Number")
plt.ylabel("Relative Path Length")
plt.ylim(ymin = 0)
# The labels below may or may not be correct, but you get the idea
for i in range(4):  # if more mitigations make significant changes, increase 4 to 5 or 6
    plt.text(x[i+1] + .2, shortest_paths[i+1]-.02, mitigations[i])
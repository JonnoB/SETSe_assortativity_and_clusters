import os
import networkx as nx
import numpy as np
import pandas as pd
from stellargraph import StellarGraph
from stellargraph import datasets
from stellargraph.data import BiasedRandomWalk
from gensim.models import Word2Vec

dims = 2

plwd = "/home/jonno/setse_1_data"
peels_folder = plwd + "/peels_graphml"
benchmark_folder = plwd + "/peel_benchmark_stellargraph_node2vec" + str(dims)
# Set the working directory to load the data from
os.chdir(peels_folder)
# get the list of files to embed
graphml_file_paths = os.listdir(peels_folder)


for peel_path in graphml_file_paths:
    # print(peel_path)
    os.chdir(peels_folder)
    peel_path = peel_path
    peel_path_csv = peel_path.replace("graphml", "csv")
    peel_path_csv = benchmark_folder + "/" + "node2vec" + "_d_" + "2_" + peel_path_csv

    if os.path.isfile(peel_path_csv):
        print("File already complete proceeding to next file")

    else:
        print(peel_path)
        # read the graph ml file
        G_graphml = nx.read_graphml(peel_path)
        # Convert the networkx graph to a Stellargraph
        G = StellarGraph.from_networkx(G_graphml)

        rw = BiasedRandomWalk(G)

        walks = rw.run(
            nodes=list(G.nodes()),  # root nodes
            length=30,  # maximum length of a random walk
            n=200,  # number of random walks per root node
            p=0.5,  # Defines (unormalised) probability, 1/p, of returning to source node
            q=2.0,  # Defines (unormalised) probability, 1/q, for moving away from source node
        )
        print("Number of random walks: {}".format(len(walks)))

        str_walks = [[str(n) for n in walk] for walk in walks]
        model = Word2Vec(str_walks, size=dims, window=10, min_count=0, sg=1, workers=4, iter=1)

        model.wv.save_word2vec_format(peel_path_csv)


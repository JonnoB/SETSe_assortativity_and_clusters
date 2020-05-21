"""
create the peels embedding for various graph embedding methods
using the GEM embedding
"""

import os
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
from time import time

from gem.utils import graph_util, plot_util
from gem.evaluation import visualize_embedding as viz
from gem.evaluation import evaluate_graph_reconstruction as gr

from gem.embedding.gf import GraphFactorization
from gem.embedding.hope import HOPE
from gem.embedding.lap import LaplacianEigenmaps
from gem.embedding.lle import LocallyLinearEmbedding
from gem.embedding.node2vec import node2vec
from gem.embedding.sdne import SDNE
from argparse import ArgumentParser

plwd = "/home/jonno/setse_1_data"
peels_folder = plwd + "/peels_graphml"
benchmark_folder = plwd + "/peel_benchamrk_embeddings"
# Set the working directory to load the data from
os.chdir(peels_folder)
# get the list of files to embed
graphml_file_paths = os.listdir(peels_folder)

models = []
# Load the models you want to run
models.append(HOPE(d=4, beta=0.01))
models.append(LaplacianEigenmaps(d=2))
models.append(LocallyLinearEmbedding(d=2))
# node2vec can go here if I can get it to work
models.append(SDNE(d=4, beta=5, alpha=1e-5, nu1=1e-6, nu2=1e-6, K=3, n_units=[50, 15, ], rho=0.3, n_iter=50, xeta=0.01,
                   n_batch=100,
                   modelfile=['enc_model.json', 'dec_model.json'],
                   weightfile=['enc_weights.hdf5', 'dec_weights.hdf5']))

#models.append(GraphFactorization(d=2, max_iter=50000, eta=1 * 10 ** -4, regu=1.0, data_set='peels'))

model_names = ["HOPE", "LapEig", "LLE", "SDNE"]
for num in range(len(models)):

    for peel_path in graphml_file_paths:
        # print(peel_path)
        os.chdir(peels_folder)
        peel_path = peel_path
        peel_path_csv = peel_path.replace("graphml", "csv")
        peel_path_csv = benchmark_folder + "/" + model_names[num] + "_d_" + "2_" + peel_path_csv

        if os.path.isfile(peel_path_csv):
            print("File already complete proceeding to next file")

        else:
            # read the graph ml file
            G = nx.read_graphml(peel_path)

            # create an embedding. Outputs the embeddings and possibly time... I am not sure
            os.chdir('/home/jonno/GEM')
            Y, t = models[num].learn_embedding(graph=G, edge_f=None, is_weighted=False, no_python=True)

            os.chdir(peels_folder)

            print(peel_path_csv)
            np.savetxt(peel_path_csv, Y, delimiter=", ")

print("I dids it, I did do it, I did")

"""
Embedd the facebook data for various graph embedding methods
using the GEM embedding
"""

import os
import networkx as nx
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from time import time

from gem.utils import graph_util, plot_util
from gem.evaluation import visualize_embedding as viz
from gem.evaluation import evaluate_graph_reconstruction as gr

from gem.embedding.hope import HOPE
from gem.embedding.lap import LaplacianEigenmaps
from gem.embedding.lle import LocallyLinearEmbedding
from gem.embedding.node2vec import node2vec
from gem.embedding.sdne import SDNE
from argparse import ArgumentParser

# number of dimensions to embed into
# this is only going to be two for this experiment
dims = 2

task_id = int(os. getenv("SGE_TASK_ID"))-1
# Get the working directory and store it so I can save the embedding there
project_folder = os.getcwd()

#HPC jobs that are completed move all files to this folder
scratch_folder = "/home/ucabbou/Scratch/facebookpython"

# The folder the data is stored in
file_folder = "/home/ucabbou/facebook_data/facebook100_graphml"

# load the data csv which has the different uni model combinations to embed
uni_model_df = pd.read_csv("/home/ucabbou/facebook_data/facebook_model_uni_python.csv")

# get the model type to use and the uni to embed
use_model_type = uni_model_df.model[task_id]
uni_name = uni_model_df.unis[task_id]

#before running the embedding a check is done to see if the file is completed
completed_file_path = scratch_folder + "/" + use_model_type + "_" + uni_name + ".csv"

# load path of the university path
load_path = file_folder + "/" + uni_name + ".graphml"
# save path of the embedded data
save_path = project_folder + "/" + use_model_type + "_" + uni_name + ".csv"

# create an empty list of models
models = []
# using and else if statement load the model for this task
# The end result is a list that is 1 long
if use_model_type == "HOPE":
    models.append(HOPE(d=dims*2, beta=0.01))
elif use_model_type == "LapEig":
    models.append(LaplacianEigenmaps(d=dims))
elif use_model_type == "LLE":
    models.append(LocallyLinearEmbedding(d=dims))
elif use_model_type == "node2vec":
    models.append(node2vec(d=2, max_iter=1, walk_len=80, num_walks=10, con_size=10, ret_p=1, inout_p=1))
else:
    # This logically has to be SDNE as there are no other options
    models.append(SDNE(d=dims*2, beta=5, alpha=1e-5, nu1=1e-6, nu2=1e-6, K=3, n_units=[50, 15, ],
                       rho=0.3,
                       n_iter=50,
                       xeta=0.01,
                       n_batch=100,
                       modelfile=['enc_model.json', 'dec_model.json'],
                       weightfile=['enc_weights.hdf5', 'dec_weights.hdf5']))

#check to see if file has already been embedded
if os.path.isfile(completed_file_path):
    print("Embeddings already created. Terminating task")

else:
    # read the graph ml file
    G = nx.read_graphml(load_path)

    # create an embedding. Outputs the embeddings and possibly time... I am not sure
    # The first aka 0 index is taken as the list is only 1 long
    Y, t = models[0].learn_embedding(graph=G, edge_f=None, is_weighted=False, no_python=True)

    # save the embedding using the previously created path
    np.savetxt(save_path, Y, delimiter=", ")

    print("network embedding complete")

# script completed
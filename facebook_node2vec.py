import os
import networkx as nx
import pandas as pd
from argparse import ArgumentParser

from node2vec import Node2Vec

# number of dimensions to embed into
# this is only going to be two for this experiment
dims = 2

task_id = int(os. getenv("SGE_TASK_ID"))-1
# Get the working directory and store it so I can save the embedding there
project_folder = os.getcwd()

# HPC jobs that are completed move all files to this folder
scratch_folder = "/home/ucabbou/Scratch/facebookpython"

# The folder the data is stored in
file_folder = "/home/ucabbou/facebook_data/facebook100_graphml"

# load the data csv which has the different uni model combinations to embed
uni_model_df = pd.read_csv("/home/ucabbou/facebook_data/facebook_model_uni_python.csv")

# get the model type to use and the uni to embed
use_model_type = uni_model_df.model[task_id]
uni_name = uni_model_df.unis[task_id]

# before running the embedding a check is done to see if the file is completed
completed_file_path = scratch_folder + "/" + use_model_type + "_" + uni_name + ".csv"

# load path of the university path
load_path = file_folder + "/" + uni_name + ".graphml"
# save path of the embedded data
save_path = project_folder + "/" + use_model_type + "_" + uni_name + ".csv"

# read the graph ml file
G = nx.read_graphml(load_path)

# Precompute probabilities and generate walks - **ON WINDOWS ONLY WORKS WITH workers=1**
node2vec = Node2Vec(G, dimensions=dims, walk_length=30, num_walks=200, workers=1)  # Use temp_folder for big graphs

# Embed nodes
# Any keywords acceptable by gensim.Word2Vec can be passed, `diemnsions` and `workers`
# are automatically passed (from the Node2Vec constructor)

model = node2vec.fit(window=10, min_count=1, batch_words=4)

model.wv.save_word2vec_format(save_path)

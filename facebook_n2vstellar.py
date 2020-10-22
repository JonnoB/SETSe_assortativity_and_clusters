import os
import networkx as nx
import pandas as pd
from stellargraph import StellarGraph
from stellargraph.data import BiasedRandomWalk
from gensim.models import Word2Vec

# number of dimensions to embed into
# this is only going to be two for this experiment
dims = 2

task_id = int(os. getenv("SGE_TASK_ID"))-1
# Get the working directory and store it so I can save the embedding there
project_folder = os.getcwd()

# HPC jobs that are completed move all files to this folder
scratch_folder = "/home/ucabbou/Scratch/facebookn2vstellargraph"

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

G_graphml = nx.read_graphml(load_path)
# get the node features as a dataframe, these will then be added to the stellar graph.
# This seems to work better than trying to put them in directly
# nodefeatures = pd.DataFrame.from_dict(dict(G_graphml.nodes(data=True)), orient='index')
# print(nodefeatures)
# Convert the networkx graph to a Stellargraph
G = StellarGraph.from_networkx(G_graphml)

rw = BiasedRandomWalk(G)

walks = rw.run(
    nodes=list(G.nodes()),  # root nodes
    length=30,  # maximum length of a random walk
    n=100,  # number of random walks per root node
    p=0.5,  # Defines (unormalised) probability, 1/p, of returning to source node
    q=2.0,  # Defines (unormalised) probability, 1/q, for moving away from source node
)
print("Number of random walks: {}".format(len(walks)))

str_walks = [[str(n) for n in walk] for walk in walks]
model = Word2Vec(str_walks, size=dims, window=10, min_count=0, sg=1, workers=1, iter=1)

model.wv.save_word2vec_format(save_path)

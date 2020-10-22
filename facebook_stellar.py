
from stellargraph.mapper import (
    CorruptedGenerator,
    FullBatchNodeGenerator,
)
from stellargraph import StellarGraph
from stellargraph.layer import GCN, DeepGraphInfomax

import networkx as nx
import os
import pandas as pd

from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import EarlyStopping
import tensorflow as tf
from tensorflow.keras import Model

# number of dimensions to embed into
# this is only going to be two for this experiment
dims = 2

task_id = int(os. getenv("SGE_TASK_ID"))-1
# Get the working directory and store it so I can save the embedding there
project_folder = os.getcwd()

# HPC jobs that are completed move all files to this folder
scratch_folder = "/home/ucabbou/Scratch/facebookstellargraph"

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
nodefeatures = pd.DataFrame.from_dict(dict(G_graphml.nodes(data=True)), orient='index')
# print(nodefeatures)
# Convert the networkx graph to a Stellargraph
G = StellarGraph.from_networkx(G_graphml, node_features=nodefeatures)

# We create and train our DeepGraphInfomax model (docs). Note that the loss used here must always be
# tf.nn.sigmoid_cross_entropy_with_logits.

fullbatch_generator = FullBatchNodeGenerator(G, sparse=False)
gcn_model = GCN(layer_sizes=[2], activations=["relu"], generator=fullbatch_generator)

corrupted_generator = CorruptedGenerator(fullbatch_generator)
gen = corrupted_generator.flow(G.nodes())

infomax = DeepGraphInfomax(gcn_model, corrupted_generator)
x_in, x_out = infomax.in_out_tensors()

model = Model(inputs=x_in, outputs=x_out)
model.compile(loss=tf.nn.sigmoid_cross_entropy_with_logits, optimizer=Adam(lr=1e-3))

epochs = 100

es = EarlyStopping(monitor="loss", min_delta=0, patience=20)
history = model.fit(gen, epochs=epochs, verbose=0, callbacks=[es])
# plot_history(history)

x_emb_in, x_emb_out = gcn_model.in_out_tensors()

# for full batch models, squeeze out the batch dim (which is 1)
x_out = tf.squeeze(x_emb_out, axis=0)
emb_model = Model(inputs=x_emb_in, outputs=x_out)

all_embeddings = emb_model.predict(fullbatch_generator.flow(G.nodes()))

final_embedds = pd.DataFrame(all_embeddings, index=G.nodes())

final_embedds.to_csv(save_path)

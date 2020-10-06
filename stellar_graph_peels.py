# Deep Graph Infomax embedding of Peels Quintet

from stellargraph.mapper import (
    CorruptedGenerator,
    FullBatchNodeGenerator,
)
from stellargraph import StellarGraph
from stellargraph.layer import GCN, DeepGraphInfomax

import networkx as nx
from stellargraph import datasets
from stellargraph.utils import plot_history
import os
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from sklearn import model_selection
from sklearn.linear_model import LogisticRegression
from sklearn.manifold import TSNE
from IPython.display import display, HTML

from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import EarlyStopping
import tensorflow as tf
from tensorflow.keras import Model

# dimensions needed for embedding
dims = 2

plwd = "/home/jonno/setse_1_data"
peels_folder = plwd + "/peels_graphml"
benchmark_folder = plwd + "/peel_benchmark_stellargraph" + str(dims)
# Set the working directory to load the data from
os.chdir(peels_folder)
# get the list of files to embed
graphml_file_paths = os.listdir(peels_folder)

for peel_path in graphml_file_paths:
    # print(peel_path)
    os.chdir(peels_folder)
    peel_path = peel_path
    peel_path_csv = peel_path.replace("graphml", "csv")
    peel_path_csv = benchmark_folder + "/" + "DGI" + "_d_" + "2_" + peel_path_csv

    if os.path.isfile(peel_path_csv):
        print("File already complete proceeding to next file")

    else:
        print(peel_path)
        # read the graph ml file
        G_graphml = nx.read_graphml(peel_path)
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

        final_embedds.to_csv(peel_path_csv)
print("I dids it, I did do it, I did")

import pandas as pd
import json

clusters = pd.read_csv("new-out/weather-regimes.csv", index_col="date").clusters.to_dict()

with open("new-out/timeseries/clusters.json", "w") as w:
    json.dump(clusters, w)

klusters = pd.read_csv("new-out/kmeans-clusters.csv", index_col="date").clusters.to_dict()

with open("new-out/timeseries/klusters.json", "w") as w:
    json.dump(klusters, w)

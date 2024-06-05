import pandas as pd
import numpy as np
from datetime import datetime as dt
from datetime import timedelta as td
import random
import os
import scipy.stats
import multiprocessing


def get_running_df(dataframe, date_col_name="date"):
    clusters = dataframe.clusters
    blocks = (clusters != clusters.shift(1)).cumsum()
    running = dataframe.groupby(blocks).agg({
        "date": "first",
        "clusters": ["first", "count"]
    })
    running.columns = ["start_date", "cluster", "length"]
    running.loc[:, "julian_day"] = (running.start_date.map(
        dt.fromisoformat).map(lambda x: dt.strftime(x, "%j")).map(int) +
                                    running.length/2) % 366
    return running


# def choose_block_probability(row, hist):
#     same_clus = hist[hist.cluster == row.cluster]
#     while True:
#         weights = 
#         choice = np.random.choice(same_clus.index, p=weights/sum(weights))
#         block = same_clus.loc[choice, :]
#         # TODO return dates from this block and make sure to fill the row.length
#     return get_dates(row.length, same_time)


def get_dates(length, start, offset=0):
    for d in range(length):
        yield dt.strftime(
            dt.fromisoformat(
                start) + td(int(d)) + td(int(offset)),
            "%Y-%m-%d")


def get_dates_all(length, choices: pd.DataFrame):
    weights = np.array([calculate_weights(row.length, length)
                   for _, row in choices.iterrows()])
    choice_row_ind = np.random.choice(choices.index, p=weights/(1e-16 + sum(weights)))
    choice_row = choices.loc[choice_row_ind, :]
    # print(choice_row.length, end=", ")

    if length == choice_row.length:
        yield from get_dates(length, choice_row.start_date)
    elif choice_row.length < length:
        # if chosen block is shorter than required
        yield from get_dates(choice_row.length, choice_row.start_date)
        yield from get_dates_all(length - choice_row.length, choices)
    else:
        # if chosen block is longer than required, truncate from one side
        if random.random() > .5:
            # 50% random chance it'll be true
            offset = choice_row.length - length
        else:
            offset = 0
        yield from get_dates(length, choice_row.start_date, offset)


def calculate_weights(l1, l2):
    len_diff = abs(l1 - l2)
    return 1 / ((len_diff) + .5)


def get_blocks(row, hist, threshold=30):
    same_clus = hist[hist.cluster == row.cluster]
    j_max = row.julian_day + threshold
    j_min = row.julian_day - threshold
    if j_min < 0:
        j_min += 365
        same_time = same_clus.loc[[j >= j_min or
                                  j <= j_max for j in same_clus.julian_day],:]
    else:
        same_time = same_clus.loc[[j >= j_min and
                                  j <= j_max for j in same_clus.julian_day],:]
    if same_time.empty:
        return get_blocks(row, hist, threshold+7)
    # print("\n", row.length, end=": ")
    return get_dates_all(row.length, same_time)


def date_sequence(simulated, historical):
    total = len(simulated.index)
    i = 1
    for _, row in simulated.iterrows():
        print(f"{i} out of {total} ({i*100/total:.2f}%)", end="\r")
        i += 1
        yield from get_blocks(row, historical)


def get_simulated_dates(col_name, simulated_df):
    simulation1 = simulated_df.loc[:, ["ref_date", col_name]]
    simulation1.columns = ["date", "clusters"]
    running_simulated = get_running_df(simulation1)

    all_dates = list(date_sequence(running_simulated,
                                   running_historical))
    assert(len(all_dates) == len(simulated_df.index))
    sim_dir = f"new-out/simulations/{col_name}"
    os.makedirs(sim_dir, exist_ok=True)
    with open(os.path.join(sim_dir, "matching-dates.txt"),
              "w") as writer:
        writer.write("\n".join(all_dates))
        writer.write("\n")
    return all_dates


def get_historical(variable="precip", basin="eerste"):
    historical = pd.read_csv(
        f"data/average-{variable}-{basin}.csv",
        index_col="Date").loc[:, "mean"]
    historical.name = variable
    return historical


def save_simulated_variable(dates, historical,
                            col_name="",
                            variable_name="precip"):
    df = pd.DataFrame(dict(date=dates))
    simulated_precip = pd.merge(df, historical, how="left",
                                left_on="date", right_index=True)
    
    sim_dir = f"new-out/simulations/{col_name}"
    simulated_precip.to_csv(os.path.join(sim_dir, f"{variable_name}.csv"))
    return simulated_precip



historical = pd.read_csv(f"new-out/weather-regimes.csv")
# historical = pd.read_csv(f"csvs/simple-kmeans-6.csv")
filter_dates = (historical.date.map(dt.fromisoformat) < dt.fromisoformat("2019-08-31")).to_list()

running_historical = get_running_df(historical.loc[filter_dates, :])
running_historical.to_csv(f"new-out/running-historical.csv")

simulated = pd.read_csv(f"new-out/simulated-wr.csv")

simulated_cols = list(filter(lambda x: x.startswith("sim_"), simulated.columns))

historical_var = get_historical(variable="precip", basin="eerste")


for sim_col in simulated_cols:
    sdf = simulated.loc[:, ["ref_date", sim_col]]
    sdf.columns = ["date", "clusters"]
    rdf = get_running_df(sdf)
    sim_dir = f"new-out/simulations/{sim_col}"
    os.makedirs(sim_dir, exist_ok=True)
    rdf.to_csv(f"new-out/simulations/{sim_col}/running-length.csv")
    print(sim_col)


def one_sim(col):
    print("Start: ", col)
    dates = get_simulated_dates(col, simulated)
    save_simulated_variable(dates, historical_var, col)
    print("Completed: ", col)

# bootstrap
# pool = multiprocessing.Pool(16)
# pool.map(one_sim, simulated_cols)

for c in simulated_cols:
    one_sim(c)


def read_simulated(col_name, variable_name="precip"):
    sim_dir = f"new-out/simulations/{col_name}"
    return pd.read_csv(os.path.join(sim_dir, f"{variable_name}.csv"))


simulation_names = list(map(lambda x: f"sim_{x+1}", range(50)))

stats = pd.DataFrame(index=simulation_names, columns=["mean", "sd", "skew"])

for sim_name in simulation_names:
    sim = read_simulated(sim_name).loc[:, "precip"].dropna().to_numpy()
    stats.loc[sim_name, "mean"] = sim.mean()
    stats.loc[sim_name, "sd"] = sim.std()
    stats.loc[sim_name, "skew"] = scipy.stats.skew(sim)

stats.to_csv(f"new-out/simulations-stats.csv")

stats_mean = pd.DataFrame(columns=["mean", "sd", "skew"])
stats_mean.loc["sim", ["mean", "sd", "skew"]] = stats.mean().to_numpy()
stats_mean.to_csv(f"new-out/simulation-mean.csv")

hist = historical_var.to_numpy()
stats = pd.DataFrame(columns=["mean", "sd", "skew"])
stats.loc["hist", "mean"] = hist.mean()
stats.loc["hist", "sd"] = hist.std()
stats.loc["hist", "skew"] = scipy.stats.skew(hist)
stats.to_csv(f"new-out/historical-stats.csv")

simulation_date = [
    dt.strftime(
        dt.fromisoformat(
            "1979-01-01") + td(d),
        "%Y") for d in simulated.index]


for sim_name in simulation_names:
    annual_mean = read_simulated(sim_name).loc[:, "precip"].groupby(simulation_date).mean()
    annual_mean.to_csv(f"new-out/simulations/{sim_name}/annual_mean.csv")

hist_annual_mean = historical_var.groupby(historical_var.index.map(lambda x: x.split("-")[0])).mean()

hist_annual_mean.to_csv(f"new-out/historical_annual_mean.csv")

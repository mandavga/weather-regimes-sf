TITLE: Weather Regime Analysis (Sea Level)
AUTHOR: Gaurav Atreya, Garima Mandavya

* Objective
Redoing the weather regime analysis on 1000 hPa.

Our Study area range for the weather regimes is:
- lat: -50.0 → -15.0
- lon: 330.0 → 60.0


* Steps
** Download, Crop and Anomaly calculation

NCEP Reanalysis Data download from: https://psl.noaa.gov/data/gridded/data.ncep.reanalysis2.html

[4x Daily Pressure Levels (Geopotential Height) Data](https://psl.noaa.gov/thredds/catalog/Datasets/ncep.reanalysis2/pressure/catalog.html) can be downloaded. Then cropping and concatenating using the [nco](https://nco.sourceforge.net/) tool.

Command to crop:

    ncks -d lat,-50.,-15. -d lon,330.,60. daily-mean-levels/hgt.1979.nc ./4xdaily-cropped/hgt.1979.nc


Command to concatenate:

    ncrcat daily-mean-levels/*.nc 4xdaily-mean.nc

Here the data is 6 hours timestep, we can calculate the daily mean and then the anomaly from python

    import xarray

    level = 1000

    daily4x = xarray.open_dataset("data/4xdaily-500hpa.nc")

    daily4x = daily4x.sel(level=level).drop_vars("level")

    years = daily4x.time.to_series().map(lambda x: x.strftime("%Y-%m-%d")).to_list()
    daily = daily4x.groupby(xarray.IndexVariable('time', years)).mean()

    anomaly = daily.hgt - daily.hgt.mean("time")
    anomaly.name = "anomaly"

    daily["anomaly"] = anomaly

    daily2.attrs["title"] = "daily NCEP/DOE Reanalysis 2"
    daily2.to_netcdf(f"data/daily-{level}hpa.nc")


We also had to save into a csv file to read it from R later

    outfile = f'csvs/daily-{level}hpa-anomaly.csv'
    df = daily2.anomaly.to_dataframe()
    df.to_csv(outfile)

** Clustering

*** PCA Analysis

*** Kmeans Cluster

*** NHMM Weather Regimes

Run the file: `nhmm-regimes.r` to identify the clusters
Run the file: `simulate-wr.r` to simulate weather regimes for future/simulated years

** Block Bootstraping

Run: `block-bootstrap.py`

It takes a long time, we have made it only run first 40 years out of 1000 for now. The multiprocessing library code is commented out because it didn't run in windows.

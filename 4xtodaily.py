import xarray

# daily4x = xarray.open_dataset("data/4xdaily-500hpa.nc")

level = 1000
# no need to do it for 1000 level
# daily4x = xarray.open_dataset("/home/gaurav/ext-hard-disc-2/ncep-reanalysis-2/4xdaily.nc")
# 
# daily4x = daily4x.sel(level=level).drop_vars("level")
# 
# years = daily4x.time.to_series().map(lambda x: x.strftime("%Y-%m-%d")).to_list()
# daily = daily4x.groupby(xarray.IndexVariable('time', years)).mean()
# 
# anomaly = daily.hgt - daily.hgt.mean("time")
# anomaly.name = "anomaly"
# 
# daily["anomaly"] = anomaly
# 
# daily.attrs["title"] = "daily NCEP/DOE Reanalysis 2"
# daily.to_netcdf(f"data/daily-{level}hpa.nc")


daily = xarray.load_dataset(f"data/daily-{level}hpa.nc")


outfile = f'csvs/daily-{level}hpa-anomaly.csv'
df = daily.anomaly.to_dataframe()
df.to_csv(outfile)

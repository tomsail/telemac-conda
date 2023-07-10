import os
import numpy as np
import pandas as pd
import matplotlib.pylab as plt
from datetime import timedelta, datetime

def read_dataframe(file, date_format="%d/%m/%Y %H:%M"):
    """ Read csv data file and return pandas dataframe """
    parser = lambda date: datetime.strptime(date, date_format)
    df = pd.read_csv(file, parse_dates=["Date"], date_parser=parser)
    df = df.set_index("Date")
    return df

def write_mto_file(
        df,
        date_ini="2021-08-18 00:00",
        date_fin="2021-08-19 00:00",
        file_name="mto_file.txt",
        header = ["# Meteo File",
                  "#",
                  "T TAIR CLDC TDEW VISBI SNOW PLUIE WINDS",
                  "s degC octa degree km mm/h mm/h m/s"],
        var_names=['TAIR', 'CLDC', 'TDEW', 'VISBI', 'SNOW', 'PLUIE', 'WINDS'],
        default_value=[None for i in range(7)]):
    """ Write MTO file in the telemac format """
    # initial checks
    if not isinstance(df, pd.DataFrame):
        raise OSError("df is not a pandas dataframe")
    assert len(default_value) == len(var_names)
    # compute total seconds
    total_sec =(pd.Timestamp(date_fin) \
              - pd.Timestamp(date_ini)).total_seconds()
    # compute date in seconds from ini date
    df['diff'] = df.index - pd.Timestamp(date_ini)
    df['seconds'] = df['diff'].dt.total_seconds()
    # open file
    file = open(file_name, "w")
    # write header
    for head_line in header:
        file.write(head_line + "\n")
    # write data
    ndat = len(df.index)
    nvar = len(var_names)
    for i in range(ndat):
        # write time
        line = "{:.1f}".format(df['seconds'].values[i]) + ","
        # write var
        for k in range(nvar):
            if default_value[k] is not None:
                line += "{}".format(default_value[k])
            else:
                line += "{}".format(df[var_names[k]].values[i])
            if k < nvar-1: line += ","
        # only write relevant times
        if df['seconds'].values[i] >= 0. and df['seconds'].values[i] <= total_sec:
            file.write(line + "\n")
    file.close()
    return 0

def write_bnd_file(
        df,
        date_ini="2021-08-18 00:00",
        date_fin="2021-08-19 00:00",
        file_name="bnd_file.txt",
        header = ["# Boundary condition file",
                  "#",
                  "T SL(1) Q(2) TR(1,1) TR(1,2) TR(2,1) TR(2,2)",
                  "s m m3/s degC degC vfract vfract"],
        var_names=['SL1', 'Q2', 'TR11', 'TR12', 'TR21', 'TR22'],
        default_value=[None for i in range(6)]):
    """ Write boundary file in the telemac format """
    # initial checks
    if not isinstance(df, pd.DataFrame):
        raise OSError("df is not a pandas dataframe")
    assert len(default_value) == len(var_names)
    # compute total seconds
    total_sec =(pd.Timestamp(date_fin) \
              - pd.Timestamp(date_ini)).total_seconds()
    # compute date in seconds from ini date
    df['diff'] = df.index - pd.Timestamp(date_ini)
    df['seconds'] = df['diff'].dt.total_seconds()
    # open file
    file = open(file_name, "w")
    # write header
    for head_line in header:
        file.write(head_line + "\n")
    # write data
    ndat = len(df.index)
    nvar = len(var_names)
    for i in range(ndat):
        # write time
        line = "{:.1f}".format(df['seconds'].values[i]) + ","
        # write var
        for k in range(nvar):
            if default_value[k] is not None:
                line += "{}".format(default_value[k])
            else:
                line += "{}".format(df[var_names[k]].values[i])
            if k < nvar-1: line += ","
        # only write relevant times
        if df['seconds'].values[i] >= 0. and df['seconds'].values[i] <= total_sec:
            file.write(line + "\n")
    file.close()
    return 0

if __name__ == "__main__":

    file_q = "USGS_GOV_from_20190105_to_20190205_Q.csv"
    file_h = "CO-OPS_8311030_from_20190105_to_20190205_wl.csv"
    file_mto = "CO-OPS_8311030_from_20190105_to_20190205_met.csv"

    #==========================================================================
    # MTO FILE
    #==========================================================================
    df_mto = read_dataframe(file_mto, "%m/%d/%Y %H:%M")
    df_mto = df_mto.dropna(subset=[' AT'])
    df_mto = df_mto.fillna(0)
    #print(df_mto)
    #df_mto[' AT'].plot()
    #plt.show()

    write_mto_file(
        df_mto,
        date_ini="2019-01-06 00:00",
        date_fin="2019-01-11 00:00",
        file_name="mto_stlawrence_ini.txt",
        var_names=[' AT', '', '', '', '', '', ''],
        default_value=[None, 0., 0., 10., 0., 0., 0.])

    write_mto_file(
        df_mto,
        date_ini="2019-01-11 00:00",
        date_fin="2019-01-12 00:00",
        file_name="mto_stlawrence.txt",
        var_names=[' AT', '', '', '', '', '', ''],
        default_value=[None, 0., 0., 10., 0., 0., 0.])

    #==========================================================================
    # BND FILE
    #==========================================================================
    df_q = read_dataframe(file_q, "%Y-%m-%d")
    df_q = df_q.dropna(subset=['flowrate'])
    #print(df_q)
    #df_q['flowrate'].plot()
    #plt.show()

    write_bnd_file(
        df_q, 
        date_ini="2019-01-06 00:00",
        date_fin="2019-01-11 00:00",
        file_name="bnd_stlawrence_ini.txt",
        var_names=['', 'flowrate', '', '', '', ''],
        default_value=[74.32, None, 0.0, 0.0, 0.05, 0.0])

    write_bnd_file(
        df_q, 
        date_ini="2019-01-11 00:00",
        date_fin="2019-01-12 00:00",
        file_name="bnd_stlawrence.txt",
        var_names=['', 'flowrate', '', '', '', ''],
        default_value=[74.32, None, 0.0, 0.0, 0.05, 0.0])


    #==========================================================================
    # BND FILE FOR THE DYNICE CASE
    #==========================================================================
    df_q = read_dataframe(file_q, "%Y-%m-%d")
    df_q = df_q.dropna(subset=['flowrate'])
    #print(df_q)
    #df_q['flowrate'].plot()
    #plt.show()

    write_bnd_file(
        df_q, 
        date_ini="2019-01-06 00:00",
        date_fin="2019-01-11 00:00",
        file_name="bnd_stlawrence_ini_dynice.txt",
        var_names=['', 'flowrate', '', '', '', '', '', '', '', ''],
        header = ["# Boundary condition file",
                  "#",
                  "T SL(1) Q(2) TR(1,1) TR(1,2) TR(2,1) "+\
                  "TR(2,2) TR(3,1) TR(3,2) TR(4,1) TR(4,2)",
                  "s m m3/s degC vfract vfract degC degC sfract sfract m m"],
        default_value=[74.32, None, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])

    write_bnd_file(
        df_q, 
        date_ini="2019-01-11 00:00",
        date_fin="2019-01-12 00:00",
        file_name="bnd_stlawrence_dynice.txt",
        var_names=['', 'flowrate', '', '', '', '', '', '', '', ''],
        header = ["# Boundary condition file",
                  "#",
                  "T SL(1) Q(2) TR(1,1) TR(1,2) TR(2,1) "+\
                  "TR(2,2) TR(3,1) TR(3,2) TR(4,1) TR(4,2)",
                  "s m m3/s degC vfract vfract degC degC sfract sfract m m"],
        default_value=[74.32, None, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])


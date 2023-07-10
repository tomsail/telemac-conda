config_ch1d = {
    "files": {
        "xcas": "../Input_Data/1d_stretch/ParametresMascaret.xcas",
        "geo": "../Input_Data/1d_stretch/Channel1d.geo",
        "res": "ResultatsOpthyca_ch1d.opt",
        "listing": "ResultatsListing_ch1d.lis",
        "damocle": "listing.damoc",
        "lig": "../Input_Data/1d_stretch/WaterLine_ch1d.lig",
        "loi": [
                "../Input_Data/1d_stretch/1d_up.loi",
                "../Input_Data/1d_stretch/1d_down.loi"
               ]
    }
}

config_ch2d = {
 "files": {
   "config_file" : "${SYSTELCFG}",
   "config_option" : "${USETELCFG}",
   "cas" : "../Input_Data/2d_rect/T2DCAS"
   }
}

config_run = {
    "Run" : {
   "RefDate" : "1/1/2019 00:00:00",
   "StartDate" : "1/1/2019 00:00:00",
   "EndDate"   : "1/1/2019 02:00:00",
   "SingleExecDuration" : "02:00:00",
        "RestartFromFile" : "no"
    },
    "2D" : {
   "ch2d" : {
       "Parallel" : "no",
       "NbProc" : 4
   }
    }
}

coupling_def = {
    "Coupling" : {
        "TimeStep" : 10.0,
        "Method" : "MultiplicativeSchwarz",
        "MaxIter"  : 5,
   "CplStepRestart1D" : "Persist2D",
   "CplStepRestart2D" : "Persist1D"
    },
    "1D" : {
        "ch1d" : {
            "TimeStep" : 10.0,
            "OutputFreq" : 20.0
        }
    },
    "2D" : {
   "ch2d" : {
            "TimeStep" : 1.0,
            "OutputFreq" : 20.0,
            "OutputSites" : [18, 33, 1018, 1024, 1034, 1044, 1054,
              1064, 1074, 1084, 1094, 1104,
              1114, 1124, 1134, 1144, 1154,
              1164, 1174, 1184, 1194, 1204,
              1214, 1224, 1234, 1244, 1254,
              1264, 1274, 1284, 1294, 1304,
              1314, 1324, 1334, 1344, 1354,
              1364, 1374, 1384, 1394, 1404,
              1414, 1424, 1434, 1444, 1454,
              1464, 1474, 1484, 1494, 1504,
              30]
   }
    },
    "Interfaces" : [
        {
            "Id1D" : "ch1d",
            "IdExtr1D" : "downstream",
            "Condition1D" : "Discharge",
       "Id2D" : "ch2d",
            "LiqBdry2D" : 2,
            "1DPosition" : "UpStream",
            "ConvCriteria" : {
                "Height" : 0.01,
                "Velocity" : 0.015
            }
        }
    ]
}

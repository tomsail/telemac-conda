coupling_def = {
    "Coupling": {
        "TimeStep": 10.0,
        "Method": "MultiplicativeSchwarz",
        "MaxIter": 5,
        "CplStepRestart1D": "Persist2D",
        "CplStepRestart2D": "Persist1D"
    },
    "1D": {
        "ch1d": {
            "TimeStep": 10.0,
            "OutputFreq": 10.0
        }
    },
    "2D": {
        "ch2d": {
            "TimeStep": 1.0,
            "OutputFreq": 10.0
        }
    },
    "Interfaces": [
        {
            "Id1D": "ch1d",
            "IdExtr1D": "downstream",
            "Condition1D": "Discharge",
            "Id2D": "ch2d",
            "LiqBdry2D": 2,
            "1DPosition": "UpStream",
            "ConvCriteria": {
                "Height": 0.01,
                "Velocity": 0.015
            }
        }
    ]
}

config_run = {
    "Run": {
        "RefDate": "1/1/2019 00:00:00",
        "StartDate": "1/1/2019 00:00:00",
        "EndDate": "1/1/2019 00:03:20",
        "SingleExecDuration": "00:01:40",
        "RestartFromFile": "yes"
    },
    "2D": {
        "ch2d": {
            "Parallel": "no",
            "NbProc": 4
        }
    }
}

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
        "cas": "../Input_Data/2d_rect/T2DCAS"
    }
}

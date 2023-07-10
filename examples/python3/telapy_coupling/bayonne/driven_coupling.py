import os
from telapy.coupling.long_cpl_driver import LongCplDriver
from configuration.cfg import Config

coupling = {
    "Coupling" : {
        "TimeStep" : 8.0,
        "Method" : "MultiplicativeSchwarz",    
        "MaxIter"  : 5,
        "CplStepRestart1D" : "Persist2D",
        "CplStepRestart2D" : "Persist1D"
    },
    "1D" : {
        "AAM" : {
            "TimeStep" : 8.0,
            "OutputFreq" : 3600.0
        },
        "AAV" : {
            "TimeStep" : 8.0,
            "OutputFreq" : 3600.0
        },
        "NA" : {
            "TimeStep" : 8.0,
            "OutputFreq" : 3600.0
        }
    },
    "2D" : {
	"Bayonne" : {
            "TimeStep" : 4.0,
            "OutputFreq" : 3600.0,
            "OutputSites" : [32939]
        }
    },
    "Interfaces" : [
        {
            "Id1D" : "AAM",
            "IdExtr1D" : "limite2",
            "Condition1D" : "Discharge",
	    "Id2D" : "Bayonne",
            "LiqBdry2D" : 2,
            "1DPosition" : "UpStream",
            "ConvCriteria" : {
                "Height" : 0.01,
                "Velocity" : 0.015
            }
        },
        {
            "Id1D" : "AAV",
            "IdExtr1D" : "limite1",
            "Condition1D" : "WaterLevel",
	    "Id2D" : "Bayonne",
            "LiqBdry2D" : 3,
            "1DPosition" : "DownStream",
            "ConvCriteria" : {
                "Height" : 0.01,
                "Velocity" : 0.015
            }
        },
        {
            "Id1D" : "NA",
            "IdExtr1D" : "limite2",
            "Condition1D" : "Discharge",
	    "Id2D" : "Bayonne",
            "LiqBdry2D" : 1,
            "1DPosition" : "UpStream",
            "ConvCriteria" : {
                "Height" : 0.01,
                "Velocity" : 0.015
            }
        }
    ]
}
 
config_run = {
    "Run" : {
        "RefDate"   : "18/6/2014 00:00:00",
        "StartDate" : "18/6/2014 12:00:00",
        "EndDate"   : "18/6/2014 16:00:00",
        "SingleExecDuration" : "02:00:00",
        "RestartFromFile" : "yes",
        "SaveCheckPoints" : "no"
    },
    "2D" : {
        "Bayonne" : {
	    "Parallel" : "no",
	    "NbProc" : 3
        }
    }
}

CFGS = Config()

python_dir = os.path.dirname(os.path.realpath(__file__))
root_dir = os.environ.get('HOMETEL')
if root_dir == '':
    root_dir = os.path.dirname(os.path.dirname(python_dir))
# Defining user configuration name and file
cfg_name = os.environ.get('USETELCFG')
cfg_file = os.environ.get('SYSTELCFG')
if cfg_file == '':
    cfg_file = os.path.join(root_dir, 'configs', 'systel.cfg')

CFGS.parse_cfg_file(cfg_file, cfg_name, root_dir, python_dir)
if 'mpi' in CFGS.configs[CFGS.cfgname]['options'].split():
    config_run["2D"]["Bayonne"]["Parallel"] = "yes"

models_configs = {
    "config_AAM": {
        "files": {
            "xcas": "../Input_Data/RUN_Adour_amont/DonneesStat/ParametresMascaret.xcas",
            "geo": "../Input_Data/RUN_Adour_amont/DonneesStat/Geometrie.geo",
            "res": "ResultatsOpthyca_AAM.opt",
            "listing": "ResultatsListing_AAM.lis",
            "damocle": "listing.damoc",
            "lig": "../Input_Data/RUN_Adour_amont/DonneesStat/WaterLine.lig",
            "loi": [
                "../Input_Data/RUN_Adour_amont/DonneesDyn/Q3120030.loi",
                "../Input_Data/RUN_Adour_amont/DonneesDyn/Q5421020.loi",
                "../Input_Data/RUN_Adour_amont/DonneesDyn/Q7412910.loi",
                "../Input_Data/RUN_Adour_amont/DonneesDyn/adour_amont.loi"
            ]
        }	
    },

    "config_AAV" : {
        "files": {
            "xcas": "../Input_Data/RUN_Adour_aval/DonneesStat/ParametresMascaret.xcas",
            "geo": "../Input_Data/RUN_Adour_aval/DonneesStat/Geometrie.geo",
            "res": "ResultatsOpthyca_AAV.opt",
            "listing": "ResultatsListing_AAV.lis",
            "damocle": "listing.damoc",
            "lig": "../Input_Data/RUN_Adour_aval/DonneesStat/WaterLine.lig",
            "loi": [
                "../Input_Data/RUN_Adour_aval/DonneesDyn/adour_aval.loi",
                "../Input_Data/RUN_Adour_aval/DonneesDyn/Q935001001.loi"
            ]
        }
    },
 
    "config_NA" : {
        "files": {
            "xcas": "../Input_Data/RUN_Nive_amont/DonneesStat/ParametresMascaret.xcas",
            "geo": "../Input_Data/RUN_Nive_amont/DonneesStat/Geometrie.geo",
            "res": "ResultatsOpthyca_NA.opt",
            "listing": "ResultatsListing_NA.lis",
            "damocle": "listing.damoc",
            "lig": "../Input_Data/RUN_Nive_amont/DonneesStat/WaterLine.lig",
            "loi": [
                "../Input_Data/RUN_Nive_amont/DonneesDyn/Q9312510.loi",
                "../Input_Data/RUN_Nive_amont/DonneesDyn/nive_amont.loi"
            ]
        }
    },
 
    "config_Bayonne" : {
        "files": {
            "config_file" : cfg_file,
            "config_option" : cfg_name,
            "cas" : "../Input_Data/Bayonne/TelemacBayonne.cas"
        }
    }
}

the_coupling = LongCplDriver(coupling_def = coupling,
                             models_configs = models_configs)
the_coupling(config_run)

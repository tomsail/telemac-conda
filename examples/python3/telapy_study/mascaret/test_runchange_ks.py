from telapy.tools.study_masc_driven import MascaretStudy

def main():
    """ Main Programm"""
    # MascaretStudy documentation
    # help(MascaretStudy)

    # Set the settings dict
    settings_dict = {
        "files": {
            "xcas": "ParametresMascaret.xcas",
            "geo": "geometrie.geo",
            "res": "ResultatsOpthyca.opt",
            "listing": "ResultatsListing.lis",
            "damocle": "listing.damoc",
            "lig": "LigneEauInitiale.lig",
            "loi": [
                "Hydro_Tonneins.loi",
                "CT_LaReole.loi"
            ]
        },
        "friction_coefficients": [{"type": "zone", "index": 0, "value": 35},
                                {"type": "zone", "index": 1, "value": 35},
                                {"type": "zone", "index": 2, "value": 30}],
        "output":{"node":220}
    }

    # Create an instance of MascaretStudy.
    ## settings dict contains input and output mascaret files : xcas, geo, lig, loi, lis, opt, damoc
    ## for backward compatibility the settings_dict could be loaded from a json file
    ## from telapy.tools.study_masc_driven import import_study_settings_from_json
    ## settings_dict = import_study_settings_from_json('config_Garonne_changeKs.json')
    ## or from a python file
    ## from telapy.tools.study_masc_driven import import_study_settings_from_py
    ## settings_dict = import_study_settings_from_py('config_Garonne_changeKs')
    ## log_lvl : CRITICAL, DEBUG, INFO
    ## iprint = 1 or 0 : keep .lis and .opt or not
    ## working_directory : where input and output files are writen
    study = MascaretStudy(settings_dict,
                          log_lvl='INFO',
                          iprint=1,
                          working_directory='study_test_runchange_ks')

    # Print information concerning this study: Screen output of directories,
    # model files and model features
    print(study)

    # Run the instance of Mascaret for input data in setting dict
    hydraulic_state = study(flag='all')

    # POST-TREATMENT
    # Extract the curvilinear abscissa and the hydraulic state
    # curv_abs = hydraulic_state['s']
    # water_depth = hydraulic_state['h']
    # water_elevation = hydraulic_state['z']
    # discharge = hydraulic_state['q']

    print("[RESULTS] x = Curvilinear abscissa # z = Water elevation # q = Discharge")
    # for i, _ in enumerate(curv_abs):
    #    print("x = {} # z = {} # q = {}".format(curv_abs[i],
    #                                            water_elevation[i],
    #                                            discharge[i]))
    # Plot the water elevation
    study.plot_water(output="WaterElevation_Ks")
    print("\n[PLOT] You can open the file 'WaterElevation_Ks.pdf' which contains"
          " a plot of the water elevation at final time.")
    # Plot the bathymetry
    study.plot_bathymetry(output="Bathymetry_Ks")
    print("\n[PLOT] You can open the file 'Bathymetry_Ks.pdf' which contains"
          " a plot of the bathymetry at final time.")
    # Save output .txt files for entire network
    # Add  "output": {"node": 220} in settings if output in local_xx.txt needed at a specific location
    study.save(out_name='test_runchangeKs')

    del study


if __name__ == "__main__":
    main()

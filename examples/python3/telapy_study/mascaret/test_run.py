from telapy.tools.study_masc_driven import MascaretStudy


def main():
    """ Main Programm"""
    # MascaretStudy documentation
    # help(MascaretStudy)

    # Create an instance of MascaretStudy from a dictionary.
    ## the dictionary contains input and output mascaret files : xcas, geo, lig, loi, lis, opt, damoc
    ## log_lvl : CRITICAL, DEBUG, INFO
    ## iprint = 1 or 0 : keep .lis and .opt or not
    ## working_directory : where input and output files are writen

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
        "output":{"node":220}
    }

    study = MascaretStudy(settings_dict,
                          log_lvl='INFO',
                          iprint=1,
                          working_directory='study_test_run')

    # Print information concerning this study: Screen output of directories,
    # model files and model features
    print(study)

    # Run the instance of Mascaret for input data in the setting dict.
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
    study.plot_water(output="WaterElevation_fromdict")
    print("\n[PLOT] You can open the file 'WaterElevation_fromdict.pdf' which contains"
          " a plot of the water elevation at final time.")
    # Plot the bathymetry
    study.plot_bathymetry(output="Bathymetry_fromdict")
    print("\n[PLOT] You can open the file 'Bathymetry_fromdict.pdf' which contains"
          " a plot of the bathymetry at final time.")
    # Save output .txt files for entire network
    # Add  "output": {"node": 220} in settings if output in local_xx.txt needed at a specific location
    study.save(out_name='test_run')

    del study


if __name__ == "__main__":
    main()

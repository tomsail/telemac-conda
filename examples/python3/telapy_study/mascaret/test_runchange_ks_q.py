from telapy.tools.study_masc_driven import MascaretStudy

def main():
    """ Main Programm"""
    # MascaretStudy documentation
    # help(MascaretStudy)

    # Create an instance of MascaretStudy
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

    study = MascaretStudy(settings_dict, log_lvl='INFO', iprint=1,
                          working_directory='study_test_runchange_ks_q')

    # Print information concerning this study.
    print(study)

    # Run the instance of Mascaret for python defined input data
    ks_val = [{'type': 'zone', 'index': 2, 'value': 30.0}]
    q_val = [{'name': 'debit_tonneins',\
              'value': [1800.0, 3000.0, 4000.0, 5000.0, 7000.0, 7000.0, 7000.0]}]
    # Other possibilities to change q_val time series :
    # q_val = [{'name': 'debit_tonneins', 'addperturb': 300.0}]
    # q_val = [{'name': 'debit_tonneins', 'multcoeff': [1.05, 1.10]}]
    # q_val = [{'name': 'debit_tonneins', 'shift_chronicle': 32400}]
    # When using keyword "value" : If only one value is given,
    # it replace the entire time series, if several values are given,
    #  it only replaces at many values in the time series and leave the following unchanged
    # q_val = [{'name': 'debit_tonneins', 'value': 3200.0}]
    # q_val = [{'name': 'CT_LaReole',
    # 'value': 4.0, 4., 5., 5., 6., 6., 7., 7., 8., 8., 9., 9., 10., 10., 11.,
    #  11., 12., 12., 13., 13., 14., 14., 14., 15., 15., 16., 16., 17.}]
    print('q_val=', q_val)
    hydraulic_state = study(x_val={'friction_coefficients': ks_val, 'boundary_conditions': q_val})
    print(study)

    # POST-TREATMENT
    # Extract the curvilinear abscissa and the water level
    # curv_abs = hydraulic_state['s']
    # water_depth = hydraulic_state['h']
    # water_elevation = hydraulic_state['z']
    # discharge = hydraulic_state['q']
    #
    print("[RESULTS] x = Curvilinear abscissa # h = Water Depth # q = Discharge")
    # for i, _ in enumerate(curv_abs):
    #    print("x = {} # z = {} # q = {}".format(curv_abs[i],
    #                                            water_elevation[i],
    #                                           discharge[i]))

    # Plot the water elevation
    study.plot_water(output="WaterElevation_from_script")
    print("\n[PLOT] You can open the file 'WaterElevation_from_script.pdf' which"
          " contains a plot of the water elevation at final time.")
    # Plot the bathymetry
    study.plot_bathymetry(output="Bathymetry_from_script")
    print("\n[PLOT] You can open the file 'Bathymetry_from_script.pdf' which contains"
          " a plot of the bathymetry level at final time.")
    # Save output files and del study
    # Add  out_idx = 220 in save arguments output in .txt needed at a specific location
    study.save(out_name='test_runchangeKsQ')
    del study


if __name__ == "__main__":
    main()

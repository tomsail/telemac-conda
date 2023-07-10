from telapy.tools.study_masc_driven import MascaretStudy
import numpy as np

def main():
    """ Main Programm"""
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
                          working_directory='study_test_run_gpsampler')

    my_name = 'debit_tonneins'
    my_bc = study.bc[my_name]

    reference = {'indices': [[t] for t in my_bc.taxis], 'values': my_bc.value}
    print('reference', reference)

    # In this example we use a hardcoded ditribution it can be generated from Python modules

    q_val = [{'name': 'debit_tonneins', 'addperturb': np.array([-96.14 , -78.573,  25.324,  59.583,  39.245, 110.97 ,  77.149])}]

    print('Q=', q_val)
    hydraulic_state = study(x_val={'boundary_conditions': q_val})

    # POST-TREATMENT
    # Extract the curvilinear abscissa and the water level
    # curv_abs = hydraulic_state['s']
    # water_depth = hydraulic_state['h']
    # water_elevation = hydraulic_state['z']
    # discharge = hydraulic_state['q']

    # Plot the water elevation
    study.plot_water(output="WaterElevation_from_script")
    print("\n[PLOT] You can open the file 'WaterElevation_from_script.pdf' which"
          " contains a plot of the water elevation at final time.")
    # Plot the bathymetry
    study.plot_bathymetry(output="Bathymetry_from_script")
    print("\n[PLOT] You can open the file 'Bathymetry_from_script.pdf' which contains"
          " a plot of the bathymetry level at final time.")
    # Save output files and del study
    study.save(out_name='test_run_Gpsampler')
    del study


if __name__ == "__main__":
    main()

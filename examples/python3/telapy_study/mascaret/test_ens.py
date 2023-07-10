import logging
import numpy as np
from telapy.tools.study_masc_driven import MascaretStudy

def main():
    """ This script is used to run N simulations of MASCARET code. """
    logging.basicConfig(level=logging.INFO)
    ###
    # This script is used to run N simulations of MASCARET code.
    ###

    # MascaretStudy Object
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

    study = MascaretStudy(settings_dict, iprint=0, working_directory='study_ens')
    n_learning = 4

    # In this example we use a hardcoded distribution it can be generated from Python modules

    x_learning = np.array([[35.688, 31.431, 34.399, 3505.378],
                          [34.988, 34.98 , 36.291, 4333.945],
                          [38.596, 38.921, 30.285, 4044.484],
                          [31.477, 37.041, 37.964, 4190.755]])

    x_learning_dico = []
    for i in range(n_learning):
        x_learning_dico.append({'friction_coefficients': [{"type": "zone", "index": 0,
                                                           "value": x_learning[i, 0]},
                                                          {"type": "zone", "index": 1,
                                                           "value": x_learning[i, 1]},
                                                          {"type": "zone", "index": 2,
                                                           "value": x_learning[i, 2]}],
                                "boundary_conditions": [{"name": "debit_tonneins",
                                                         "value": x_learning[i, 3]}]})

    # Run Mascaret for members in x_ens to compute z_ens, q_ens, ...
    h_ens = []
    z_ens = []
    q_ens = []
    for k in range(n_learning):
        x_val = x_learning_dico[k]
        print('x_learning_dico[k]=', x_learning_dico[k])
        print("study#" + str(k))
        print('x=', x_val)
        study.initialize_model()
        output = study(x_val, flag='all')
        h_ens.append(output['h'])
        z_ens.append(output['z'])
        q_ens.append(output['q'])
        print('Output', output['h'])
        study.plot_water(output="WaterElevation_member_{}".format(k))
        study.save(out_name='test_ens', out_idx=k)

    del study


if __name__ == "__main__":
    main()

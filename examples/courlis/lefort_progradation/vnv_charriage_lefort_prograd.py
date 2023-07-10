"""
Validation script for bedload transport in Courlis
"""

import numpy as np
import matplotlib.pyplot as plt
import pylab as pl
from vvytel.vnv_study import AbstractVnvStudy
from os import path


class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of bedload transport Courlis in flume
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['courlis']

    def _pre(self):
        """
        Defining the studies
        """
        # Transcritical kernel

        # ==== Transport law : Lefort 2014
        # 1) Progradation in flume
        # 1.0 SARAP (dt = 1s, dx = 10m)
        self.add_study('vnv_1_0',
                       'mascaret',
                       'prograd_lefort_sarap.xcas')
        # 1.1 SARAP with decentrement option (dt = 360s, dx = 50m)
        self.add_study('vnv_1_1',
                       'mascaret',
                       'prograd_lefort_sarap_dec.xcas')
        # 2) progradation in flume
        #self.add_study('vnv_2',
        #               'mascaret',
        #               path.join('progradation','erosion_Lefort.xcas'))

        # TODO : When adding the others laws, do not forget to change the
        # 'what_law_what_case' function

        # ==== Transport law : Engelund
        # 1) Progradation in flume
        # self.add_study('vnv_3',
        #                'mascaret',
        #                'progradation_Engelund.xcas')
        # 2) progradation in flumes
        # self.add_study('vnv_4',
        #                'mascaret',
        #                'progradation_Engelund.xcas')

        # ===== Transport law : Recking
        # 1) Progradation in flume
        # self.add_study('vnv_5',
        #                'mascaret',
        #                'progradation_Recking.xcas')
        # 2) progradation in flume
        # self.add_study('vnv_6',
        #                'mascaret',
        #                'progradation_Recking.xcas')

        # ===== Transport law : Smart
        # 1) Progradation in flume
        # self.add_study('vnv_7',
        #                'mascaret',
        #                'progradation_Smart.xcas')
        # 2) progradation in flume
        # self.add_study('vnv_8',
        #                'mascaret',
        #                'progradation_Smart.xcas')

    def _check_results(self):
        """
        Post-treatment processes
        """
        #laws, N = get_studies()

        #for i in range(N):
        #    TS_case, TS_name, law = what_law_what_case(i, laws)
        #    num = str(i + 1)  # Study number
        #    print(law + 'transport law :' + TS_name)

        #    # Mascaret file of study
        #    masc_file = self.get_study_res(self,
        #                              name='vnv_' + num, module="mascaret")
        #    # Corresponding Cavalcade results
        #    cavalcade_file = 'canal_' + TS_case + '_' + law + '.txt'

        #    # Check
        #    check_errors(masc_file, cavalcade_file)
        pass

    def _post(self):
        """
        Post-treatment processes
        """
        #laws, N = get_studies()
        #colors = pl.cm.Set1(np.linspace(0, 1, 9))

        #for i in range(N):
        #    TS_case, TS_name, law = what_law_what_case(i, laws)
        #    num = str(i + 1)  # Study number
        #    print(law + 'transport law :' + TS_name)

        #    # Mascaret file of study
        #    masc_file = self.get_study_res(self,
        #                              name='vnv_' + num, module="mascaret")
        #    z_init, z_final, z_max, z_min = get_bottom_results(masc_file)
        #    # TODO: Get bedload rates

        #    # Corresponding Cavalcade results
        #    cavalcade_file = 'canal_' + TS_case + '_' + law + '.txt'
        #    ID, x,\
        #        z_init_cav, z_final_cav,\
        #        z_max_cav, z_min_cav,\
        #        qs = get_cavalcade_results(cavalcade_file)
        #    t_qs = qs[0, :]  # Times for bedload outputs in seconds
        #    n_qs = len(t_qs)  # Number of outputs
        #    qs = qs[1:, :]  # Bedload values
        #    x_qs = x[:-1]  # Remove PF0

        #    # Plots
        #    fig = plt.figure()
        #    ax = fig.add_subplot(1, 1, 1)
        #    ax.plot(x, z_init, '-', color=colors[0],
        #            label='Courlis - Initial level')
        #    ax.plot(x, z_final, '-', color=colors[1],
        #            label='Courlis - Final level')
        #    ax.plot(x, z_max, '-', color=colors[2],
        #            label='Courlis - Maximal level')
        #    ax.plot(x, z_min, '-', color=colors[3],
        #            label='Courlis - Minimal level')
        #    ax.plot(x, z_init_cav, ':', color=colors[0],
        #            label='Cavalcade - Initial level')
        #    ax.plot(x, z_final_cav, ':', color=colors[1],
        #            label='Cavalcade - Final level')
        #    ax.plot(x, z_max_cav, ':', color=colors[2],
        #            label='Cavalcade - Maximal level')
        #    ax.plot(x, z_min_cav, ':', color=colors[3],
        #            label='Cavalcade - Minimal level')
        #    # Nice plot
        #    ax.set_xlabel("X [m]")
        #    ax.set_ylabel("Bottom elevation [m]")
        #    ax.set_title(TS_name + '-' + law, fontsize=20)
        #    plt.rcParams.update({'font.size': 15})
        #    # Grid parameters
        #    plt.grid(b=True, which='major', color='#666666', linestyle='-',
        #             alpha=0.4)
        #    plt.minorticks_on()
        #    plt.grid(b=True, which='minor', color='#999999', linestyle='-',
        #             alpha=0.2)
        #    ax.legend(fontsize=15)

        #    fig = plt.figure()
        #    ax = fig.add_subplot(1, 1, 1)
        #    for i in range(n_qs):
        #        qs_cavalcade = qs[:, i]
        #        # TODO : find the qs of Courlis @t_qs[i]
        #        # ax.plot(x, z_init, '-', color=colors[i % 9],
        #        #         label='Courlis - q_s@' + str(t_qs[i]) + 'h')
        #        ax.plot(x_qs, qs_cavalcade, ':', color=colors[i % 9],
        #                label='Cavalcade - q_s@' + str(t_qs[i]) + 'h')

        #    # Nice plot
        #    ax.set_xlabel("X [m]")
        #    ax.set_ylabel("Bedload [m3/s]")
        #    ax.set_title(TS_name + '-' + law, fontsize=20)
        #    plt.rcParams.update({'font.size': 15})
        #    # Grid parameters
        #    plt.grid(b=True, which='major', color='#666666', linestyle='-',
        #             alpha=0.4)
        #    plt.minorticks_on()
        #    plt.grid(b=True, which='minor', color='#999999', linestyle='-',
        #             alpha=0.2)
        #    ax.legend(fontsize=15)
        pass


def get_bottom_results(masc_file):
    """
    Returns the initial, final, maximal and minimal bottom levels
    in 'masc_file'

    Parameters
    ----------
    masc_file : class MascaretFile
        Results file

    Returns
    -------
    z_init, z_final, z_max, z_min: Numpy arrays
        Initial, final, maximal and minimal bottom levels

    """
    varname = 'Cote du fond'
    reach = masc_file.reaches[1]
    n_sect = reach.nsections
    var_pos = masc_file.get_position_var(varname)
    # Initialization
    z_init = np.zeros(n_sect)
    z_final = np.zeros(n_sect)
    z_max = np.zeros(n_sect)
    z_min = np.zeros(n_sect)

    for i_sect in range(n_sect):
        section = reach.sections[i_sect + 1]
        values = masc_file.get_series(1, section.id, [var_pos])
        z_init[i_sect] = values[0]
        z_final[i_sect] = values[-1]
        z_max[i_sect] = max(values)
        z_min[i_sect] = min(values)
    return z_init, z_final, z_max, z_min


def get_cavalcade_results(file):
    """
    Returns the initial, final, maximal and minimal bottom levels
    obtained with Cavalcade and written in 'file'

    Parameters
    ----------
    file : str
        Results file

    Returns
    -------
    ID : Numpy array
        Section names (int)
    x : Numpy array
        Pks for each section
    z_init, z_final, z_max, z_min: Numpy arrays
        Initial, final, maximal and minimal bottom levels
    qs : Numpy array
        Bedload rate at different time steps for each section except the 0

    """
    sol = np.loadtxt(file, skiprows=1)
    ID = sol[:, 0]
    x = sol[:, 1]
    z_init = sol[:, 2]
    z_final = sol[:, 3]
    z_max = sol[:, 4]
    z_min = sol[:, 5]
    qs = sol[:, 5:]
    return x, ID, z_init, z_final, z_max, z_min, qs


def check_errors(masc_file, cavalcade_file):
    """
    Print the errors on bottom elevations and bedload rates between the
    mascaret results and the cavalcade ones

    Parameters
    ----------
    masc_file : class MascaretFile
        Mascaret results
    cavalcade_file : str
        File with Cavalcade results

    Returns
    -------
    None.

    """
    z_init, z_final, z_max, z_min = get_bottom_results(masc_file)
    # TODO:How to get mascaret bedloads ?

    ID, x,\
        z_init_cav, z_final_cav,\
        z_max_cav, z_min_cav,\
        qs = get_cavalcade_results(cavalcade_file)
    error_init = z_init_cav - z_init
    error_final = z_final_cav - z_final
    error_max = z_max_cav - z_max
    error_min = z_min_cav - z_min

    print('Initial bottom level error : {:.2e}'.format(error_init))

    print('Final bottom level error : {:.2e}'.format(error_final))

    print('Maximal bottom level error : {:.2e}'.format(error_max))

    print('Minimal bottom level error : {:.2e}'.format(error_min))


def get_studies():
    """
    Transport laws and study names of this check

    Returns
    -------
    laws : list of str
        Transport laws
    N : int
        Number of studies
    """
    laws = ['Lefort ']
    # TODO : MAJ quand autres lois de transport opérationnelles
    # laws = ['Lefort ', 'Engelund ', 'Recking ', 'Smart ']

    N = 2 * len(laws)  # progradation + Progradation for each

    return laws, N


def what_law_what_case(i, laws):
    """
    Return the law and the case corresponding to study number 'i'

    Parameters
    ----------
    i : int
        Study number
    laws : list of str
        Transport laws names

    Returns
    -------
    law : str
        Law name
    TS_case : {"progradation", "erosion"}
    TS_name : {'Progradation in a flume', 'progradation in a flume'}

    """
    if i % 2 == 0:
        TS_case = "progradation"
        TS_name = 'Progradation in a flume'
        i_law = i / 2
    else:
        TS_case = "progradation"
        TS_name = 'progradation in a flume'
        i_law = i - 1 / 2
    law = laws[i_law]

    return TS_case, TS_name, law


"""
Validation script for thermal_budget
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # full thermal budget (serial)
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_thermic.cas')


        # full thermal budget (parallel)
        cas = TelemacCas('t2d_thermic.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_thermic_par.cas',
                       cas=cas)
        del cas

        # linear thermal budget (serial)
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_thermic_linear.cas')

        # linear thermal budget (parallel)
        cas = TelemacCas('t2d_thermic_linear.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_thermic_linear_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_thermic.slf',
                            eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES',
                            'fce_thermic.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_thermic.slf',
                            eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES',
                            'fce_thermic.slf',
                            eps=[1.E-6])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES',
                            'vnv_2:ICERES',
                            eps=[1.E-6])

    def _post(self):
        """
        Post-treatment processes
        """
        from os import path
        from postel.deco_vnv import decoVNV, decoVNV_1d
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot2d
        from data_manip.computation.datetimes import compute_datetimes
        import matplotlib.pyplot as plt
        import matplotlib.dates as mdates
        import numpy as np

        # Getting res files
        geo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        res_ice_1 = TelemacFile(self.get_study_file('vnv_1:ICERES'))

        res_vnv_3 = TelemacFile(self.get_study_file('vnv_3:T2DRES'))
        res_ice_3 = TelemacFile(self.get_study_file('vnv_3:ICERES'))

        res_ice = [res_ice_1, res_ice_3]

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        #Plotting mesh
        vnv_plot2d(\
            '',
            geo,
            plot_mesh=True,
            annotate_bnd=True,
            fig_size=(7, 7),
            fig_name='img/mesh')

        # PLOT INPUT VARIABLES FROM METEO FILE

        #======================================================================
        # OBSERVATION:
        #
        #Plotting thermal fluxes
        for idx, resi in enumerate(res_ice):
            phis = resi.get_timeseries_on_points('NET SOLRAD', [[0., 0.]])
            phib = resi.get_timeseries_on_points('EFFECTIVE SOLRAD', [[0., 0.]])
            phie = resi.get_timeseries_on_points('EVAPO HEAT FLUX', [[0., 0.]])
            phih = resi.get_timeseries_on_points('CONDUC HEAT FLUX', [[0., 0.]])
            phip = resi.get_timeseries_on_points('PRECIP HEAT FLUX', [[0., 0.]])
            phit = phis + phib + phie + phih + phip
            fig, ax = plt.subplots(1, 1, figsize=(10, 4))
            datetimes = compute_datetimes(resi.times, initial_date=resi.datetime)
            plot1d(ax, datetimes, phis[0, :], plot_label='$\phi_R$')
            plot1d(ax, datetimes, phib[0, :], plot_label='$\phi_B$')
            plot1d(ax, datetimes, phie[0, :], plot_label='$\phi_E$')
            plot1d(ax, datetimes, phih[0, :], plot_label='$\phi_H$')
            plot1d(ax, datetimes, phip[0, :], plot_label='$\phi_P$')
            plot1d(ax, datetimes, phit[0, :], plot_label='$\phi$')
            ax.set_xlabel('$t$ $(h)$')
            ax.set_ylabel('$\phi$ $(W/m^2)$')
            ax.xaxis.set_major_formatter(mdates.DateFormatter("%H"))
            ax.legend()
            print(" "*8+"~> Plotting img/thermal_fluxes")
            plt.savefig('img/thermal_fluxes_{}'.format(idx))
            # Close figure:
            plt.close('all')

        #Plotting comparison with CRISSP2D
        exp = np.loadtxt('data/ref_crissp2d.prn')
        values = res_vnv_1.get_timeseries_on_nodes('TEMPERATURE', [95])
        fig, ax = plt.subplots(1, 1, figsize=(10, 3))
        datetimes_exp = compute_datetimes(exp[:, 0], initial_date=res_ice_1.datetime)
        datetimes = compute_datetimes(res_vnv_1.times, initial_date=res_ice_1.datetime)
        plot1d(ax, datetimes_exp, exp[:, 1], marker='o', plot_label='CRISSP2D', lw=2)
        plot1d(ax, datetimes, values[0, :], plot_label='KHIONE', lw=2)
        ax.set_xlabel('$t$ $(h)$')
        ax.set_ylabel('$T$ $(\degree C)$')
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%H"))
        ax.legend()
        print(" "*8+"~> Plotting img/temperature")
        plt.savefig('img/temperature')
        # Close figure:
        plt.close('all')

        # Closing files
        geo.close()
        res_vnv_1.close()
        res_ice_1.close()

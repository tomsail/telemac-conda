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
        self.tags = ['telemac3d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # full thermal budget (serial)
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_thermic.cas')


        # full thermal budget (parallel)
        cas = TelemacCas('t3d_thermic.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_thermic_par.cas',
                       cas=cas)
        del cas

        # linear thermal budget (serial)
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_thermic_linear.cas')

        # linear thermal budget (parallel)
        cas = TelemacCas('t3d_thermic_linear.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_thermic_linear_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_thermic.slf',
                            eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES',
                            'fce_thermic.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_thermic.slf',
                            eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES',
                            'fce_thermic.slf',
                            eps=[1.E-6])

        # Comparison between serial and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
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
        from data_manip.computation.triangulation import triangulation_from_data
        from postel.plot2d import plot2d_scalar_filled_contour

        # Getting res files
        geo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module='T3D')
        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T3DRES'))
        res_ice_1 = TelemacFile(self.get_study_file('vnv_1:ICERES'))

        res_vnv_3 = TelemacFile(self.get_study_file('vnv_3:T3DRES'))
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

        #Plotting Temperature
        exp = np.loadtxt('data/ref_crissp2d.prn')
        values1 = res_vnv_1.get_timeseries_on_nodes('TEMPERATURE', [288])
        values2 = res_vnv_1.get_timeseries_on_nodes('TEMPERATURE', [95])
        values = np.add(values1,values2)/2.
        fig, ax = plt.subplots(1, 1, figsize=(10, 3))
        datetimes_exp = compute_datetimes(exp[:, 0], initial_date=res_ice_1.datetime)
        datetimes = compute_datetimes(res_vnv_1.times, initial_date=res_ice_1.datetime)
        plot1d(ax, datetimes, values[0, :], plot_label='KHIONE', lw=2)
        ax.set_xlabel('$t$ $(h)$')
        ax.set_ylabel('$T$ $(\degree C)$')
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%H"))
        ax.legend()
        print(" "*8+"~> Plotting img/temperature")
        plt.savefig('img/temperature')
        # Close figure:
        plt.close('all')

        # Plotting vertical slices
        poly_points = [[-2., 0.], [2., 0.]]
        poly_number=[10]
        _, abs_curv, values_poly_Z1=res_vnv_1.get_data_on_vertical_plane('ELEVATION Z', 23, poly_points, poly_number)
        _, _, values_poly_Z2=res_vnv_1.get_data_on_vertical_plane('ELEVATION Z', -1, poly_points, poly_number)
        _, _, T1= res_vnv_1.get_data_on_vertical_plane('TEMPERATURE', 23, poly_points, poly_number)
        _, _, T2= res_vnv_1.get_data_on_vertical_plane('TEMPERATURE', -1, poly_points, poly_number)
        mesh = triangulation_from_data(abs_curv, values_poly_Z1)
        temp1 = T1.flatten()
        temp2 = T2.flatten()
        fig, axes = plt.subplots(figsize=(10, 4))
        plot2d_scalar_filled_contour(fig, axes, mesh, temp1, data_name='Temperature (degC)',x_label="Distance (m)", y_label="Z (m)")
        print(" "*8+"~> Plotting img/temperature_T1")
        plt.savefig('img/temperature_T1')
        plt.close('all')
        fig, axes = plt.subplots(figsize=(10, 4))
        plot2d_scalar_filled_contour(fig, axes, mesh, temp2, data_name='Temperature (degC)',x_label="Distance (m)", y_label="Z (m)")
        print(" "*8+"~> Plotting img/temperature_T2")
        plt.savefig('img/temperature_T2')
        plt.close('all')

        # Closing files
        geo.close()
        res_vnv_1.close()
        res_ice_1.close()

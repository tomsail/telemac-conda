
"""
Validation script for flume_frazil
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes
from matplotlib import rc

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        # Carstens 1 monoclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 600.)
        cas.set('INITIAL VALUES OF TRACERS', [0., 0.])
        cas.set('KHIONE STEERING FILE', 'ice_monoclass_carstens1.cas')
        self.add_study('vnv_1','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

        # Carstens 2 monoclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 800.)
        cas.set('INITIAL VALUES OF TRACERS', [0., 0.])
        cas.set('KHIONE STEERING FILE', 'ice_monoclass_carstens2.cas')
        self.add_study('vnv_2','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

        # Carstens 1 multiclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 600.)
        cas.set('KHIONE STEERING FILE', 'ice_multiclass_carstens1.cas')
        self.add_study('vnv_3','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

        # Carstens 2 multiclass - parallel
        cas = TelemacCas('t2d_racetrack_carstens.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('DURATION', 800.)
        cas.set('KHIONE STEERING FILE', 'ice_multiclass_carstens2.cas')
        self.add_study('vnv_4','telemac2d', 't2d_racetrack_carstens.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Carstens 1 monoclass
        self.check_epsilons('vnv_1:T2DRES', 'f2d_racetrack_carstens_1_mono.slf', eps=[1.E-3])
        self.check_epsilons('vnv_1:ICERES', 'fce_racetrack_carstens_1_mono.slf', eps=[1.E-6])
        # Carstens 2 monoclass
        self.check_epsilons('vnv_2:T2DRES', 'f2d_racetrack_carstens_2_mono.slf', eps=[1.E-3])
        self.check_epsilons('vnv_2:ICERES', 'fce_racetrack_carstens_2_mono.slf', eps=[1.E-6])
        # Carstens 1 multiclass
        self.check_epsilons('vnv_3:T2DRES', 'f2d_racetrack_carstens_1_multi.slf', eps=[1.E-3])
        self.check_epsilons('vnv_3:ICERES', 'fce_racetrack_carstens_1_multi.slf', eps=[1.E-6])
        # Carstens 2 multiclass
        self.check_epsilons('vnv_4:T2DRES', 'f2d_racetrack_carstens_2_multi.slf', eps=[1.E-3])
        self.check_epsilons('vnv_4:ICERES', 'fce_racetrack_carstens_2_multi.slf', eps=[1.E-6])

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        import matplotlib.pyplot as plt
        import matplotlib.dates as mdates
        import matplotlib.patches as patches
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines, vnv_khione_plot1d_TC
        from postel.plot2d import plot2d_triangle_mesh, \
            plot2d_scalar_filled_contour, plot2d_vectors
        import datetime

        geo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('vnv_1:T2DRES'))

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        #Plotting mesh
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            plot_mesh=True,
            annotate_bnd=True,
            filled_contours=False,
            fig_size=(14, 4),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(14, 4),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        fig_size = (6.5, 4.5)
        extract = [2., -0.6]
        ite_to_plot = [0, -1]

        for i in ite_to_plot:

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Custom plot for multiclass publi
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            fig, ax = plt.subplots(1, 1, figsize=(10, 3))
            margin = 0.015
            xlim = [np.min(res.tri.x) - margin, np.max(res.tri.x) + margin]
            ylim = [np.min(res.tri.y) - margin, np.max(res.tri.y) + margin]
            # mesh
            plot2d_triangle_mesh(ax, res.tri, color='k',
                x_label='$x$ $(m)$', y_label='$x$ $(m)$')

            # velocity
            Ux = res.get_data_value('VELOCITY U', i)
            Uy = res.get_data_value('VELOCITY V', i)
            V = np.sqrt(Ux**2 + Uy**2)
            plot2d_scalar_filled_contour(
                fig, ax, res.tri, V, data_name='$|U|$ $(m/s)$',
                vmin=0.4, vmax=0.6, nv=11,
                alpha=1., colorbar=True, x_label='', y_label='')

            plot2d_vectors(
                fig, ax, res.tri, Ux, Uy,
                normalize=True, scale=35, colorbar=False,
                grid_resolution=[25, 25],
                grid_xlim=xlim, grid_ylim=ylim,x_label='', y_label='',
                data_name='velocity', color='k', alpha=0.75)

            # annotate
            ax.plot(extract[0], extract[1], marker='s', markersize=7, color='r')
            propellerx = [0.0,4.,4.,0.0,0.0]
            propellery = [0.5,0.5,0.7,0.7,0.5]
            ax.plot(propellerx, propellery, color='k', lw=1.)
            ax.set_xlim(xlim[0], xlim[1])
            ax.set_ylim(ylim[0], ylim[1])

            # save fig
            fig.savefig("img/racetrack2d_{}".format(i), dpi=300)

            # Close figure:
            fig.clf()
            plt.close()
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

            vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=i,
                filled_contours=True,
                fig_size=(10, 3),
                fig_name='img/H-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$H$ $(m)$')

            vnv_plot2d(\
                'VELOCITY',
                res,
                record=i,
                filled_contours=True,
                vectors=True,
                fig_size=(10, 3),
                fig_name='img/VEL-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$U$ $(m/s)$')

            vnv_plot2d(\
                'VELOCITY U',
                res,
                record=i,
                filled_contours=True,
                fig_size=(10, 3),
                fig_name='img/U-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$U$ $(m/s)$')

            vnv_plot2d(\
                'VELOCITY V',
                res,
                record=i,
                filled_contours=True,
                fig_size=(10, 3),
                fig_name='img/V-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$U$ $(m/s)$')

            vnv_plot2d(\
                'TEMPERATURE',
                res,
                record=-1,
                filled_contours=True,
                fig_size=(10, 3),
                fig_name='img/T-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$T$ $(^\circ {C})$')

        #=====================================================================
        # Carstens 1 monoclass:
        res = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T1 = np.loadtxt(datarep+"case1.csv", delimiter=',', comments='#', skiprows=0)
        T1[:,0] = 60.*T1[:,0]
        # get data values
        T, Ct, _ = khione_get_timeseries_on_points(res, [extract], ncfra=1)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T1,
            ref_T_label="$T - Obs.$",
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens1_monoclass')

        #=====================================================================
        # Carstens 2 monoclass:
        res = TelemacFile(self.get_study_file('vnv_2:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T2 = np.loadtxt(datarep+"case2.csv", delimiter=',', comments='#', skiprows=0)
        T2[:,0] = 60.*T2[:,0]
        # get data values
        T, Ct, _ = khione_get_timeseries_on_points(res, [extract], ncfra=1)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T2,
            ref_T_label="$T - Obs.$",
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens2_monoclass')

        #=====================================================================
        # Carstens 1 multiclass:
        res = TelemacFile(self.get_study_file('vnv_3:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T1 = np.loadtxt(datarep+"case1.csv", delimiter=',', comments='#', skiprows=0)
        T1[:,0] = 60.*T1[:,0]
        # get data values
        T, Ct, C = khione_get_timeseries_on_points(res, [extract], ncfra=10)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=C,
            ref_T_data=T1,
            ref_T_label="$T - Obs.$",
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            fig_size=(10,5),
            fig_name='img/carstens1_multiclass_ck')
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T1,
            ref_T_label="$T - Obs.$",
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens1_multiclass')

        #=====================================================================
        # Carstens 2 multiclass:
        res = TelemacFile(self.get_study_file('vnv_4:T2DRES'))
        # reference data
        datarep = "data/data_carstens_1966/"
        T2 = np.loadtxt(datarep+"case2.csv", delimiter=',', comments='#', skiprows=0)
        T2[:,0] = 60.*T2[:,0]
        # get data values
        T, Ct, C = khione_get_timeseries_on_points(res, [extract], ncfra=10)
        # Plot timeseries on points:
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=C,
            ref_T_data=T2,
            ref_T_label="$T - Obs.$",
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            fig_size=(10,5),
            fig_name='img/carstens2_multiclass_ck')
        vnv_khione_plot1d_TC(
            res.times, T, Ct, C=None,
            ref_T_data=T2,
            ref_T_label="$T - Obs.$",
            ref_T_error=0.01,
            normalize_time=True,
            normalize_reftime=True,
            normalize_Ct=True,
            fig_size=fig_size,
            fig_name='img/carstens2_multiclass')

        #=====================================================================
        # Closing files
        geo.close()
        res.close()

def khione_get_timeseries_on_points(res, points, ncfra=1):
    import numpy as np
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    T = T[0, :]
    C = np.zeros((ncfra, len(T)))
    Ct = np.zeros((len(T)))
    if ncfra>1:
        for i in range(1, ncfra+1):
            Ci = res.get_timeseries_on_points('FRAZIL {}'.format(i), points)
            C[i-1, :] = Ci[0, :]
        for i in range(len(T)):
            Ct[i] = np.sum(C[:, i])
    else:
        Ct = res.get_timeseries_on_points('FRAZIL', points)
        Ct = Ct[0, :]
    return T, Ct, C

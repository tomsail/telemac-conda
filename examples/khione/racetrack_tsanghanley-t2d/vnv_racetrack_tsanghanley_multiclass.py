
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
        self.rank = 4
        self.tags = ['telemac2d', 'khione']
        self.Tn = [-1.728, -1.8, -1.9, -2., -2.1, -2.2, -2.3, -2.4, -2.5]
        self.dura = [800., 600., 600., 400., 400., 400., 300., 300., 300.]

    def _pre(self):
        """
        Defining the studies
        """
        for i in range(1,10):
            cas = TelemacCas('t2d_racetrack_tsanghanley.cas'.format(i),
                get_dico('telemac2d'))
            cas.set('PARALLEL PROCESSORS', 4)
            cas.set('KHIONE STEERING FILE', 'ice_multiclass_tsang1.cas')
            cas.set('DURATION', self.dura[i-1])
            cas.set('INITIAL VALUES OF TRACERS', [self.Tn[i-1], 31.2,
                0.,0.,0.,0.,0.,0.,0.,0.,0.,0.])
            self.add_study('tsang_{}'.format(i),
               'telemac2d', 't2d_multiclass_tsang{}.cas'.format(i), cas=cas)
            del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        for i in range(1,10):
            self.check_epsilons(
                'tsang_{}:T2DRES'.format(i),
                'f2d_racetrack_tsang_{}_multi.slf'.format(i),
                eps=[1.E-3])
            self.check_epsilons(
                'tsang_{}:ICERES'.format(i),
                'fce_racetrack_tsang_{}_multi.slf'.format(i),
                eps=[1.E-6])

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

        geo, _ = self.get_study_res('tsang_1:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('tsang_1:T2DRES'))
        res_list, res_labels = self.get_study_res(module='T2D')

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
            fig_size=(12, 6),
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(12, 6),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        ite_to_plot = [0, -1]

        for i in ite_to_plot:

            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Custom plot for multiclass publi
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            fig, ax = plt.subplots(1, 1, figsize=(8, 4))
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
                vmin=0.1, vmax=0.2, nv=11,
                alpha=1., colorbar=True,x_label='', y_label='')

            plot2d_vectors(
                fig, ax, res.tri, Ux, Uy,
                normalize=True, scale=35, colorbar=False,
                grid_resolution=[25, 25],
                grid_xlim=xlim, grid_ylim=ylim,x_label='', y_label='',
                data_name='velocity', color='k', alpha=0.75)

            # annotate
            extract = [0.325, -0.225]
            ax.plot(extract[0], extract[1], marker='s', markersize=7, color='r')
            propellerx = [0.10,0.55,0.55,0.10,0.10]
            propellery = [0.14,0.14,0.31,0.31,0.14]
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
                fig_size=(14, 6),
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
                fig_size=(14, 6),
                fig_name='img/VEL-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$U$ $(m/s)$')

            vnv_plot2d(\
                'VELOCITY U',
                res,
                record=i,
                filled_contours=True,
                fig_size=(14, 6),
                fig_name='img/U-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$U$ $(m/s)$')

            vnv_plot2d(\
                'VELOCITY V',
                res,
                record=i,
                filled_contours=True,
                fig_size=(14, 6),
                fig_name='img/V-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$U$ $(m/s)$')

            vnv_plot2d(\
                'TEMPERATURE',
                res,
                record=-1,
                filled_contours=True,
                fig_size=(14, 6),
                fig_name='img/T-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$T$ $(^\circ {C})$')

            vnv_plot2d(\
                'SALINITY',
                res,
                record=-1,
                filled_contours=True,
                fig_size=(14, 6),
                fig_name='img/S-2d_scalarmap_{}'.format(i),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$S$ $(ppt)$')

        # plot parameters:
        point = [0.325, -0.225]
        figsize = (5, 4)
        ref_labels = ['0', '092', '168', '254', '337', '423', '552', '637', '742']
        seeding_Tn = [-1.728, -1.8, -1.9, -2., -2.1, -2.2, -2.3, -2.4, -2.5]
        Tmin = [-1.764, -1.812, -1.910, -2.008, -2.102, -2.201, -2.302, -2.402, -2.501]
        tmin = [298., 67., 45.6, 23.7, 8.2, 6., 6.4, 8.44, 3.08]
        tc = [547., 341., 222., 152., 103., 75.2, 67.6, 54.84, 51.]
        Tf = -1.7056
        xmax = [1.5, 1.5, 1.5, 1.5, 2., 2., 2., 2., 2.]

        #=====================================================================
        for idx, res in enumerate(res_list):
            lab = res_labels[idx].lower()

            # load reference data
            datarep = "data/data_tsanghanley_1985/"
            Cref = np.loadtxt(
                datarep+"GroupC_SAS0p"+ref_labels[idx]+".csv",
                delimiter=',', comments='#', skiprows=0)

            ref_T = np.zeros((1, 2))
            ref_T[0, 0] = tmin[idx]
            ref_T[0, 1] = Tmin[idx]

            # get data values:
            T, Ct, _ = khione_get_timeseries_on_points(res, [point], ncfra=10)
            # Plot timeseries on points:
            vnv_khione_plot1d_TC(
                res.times, T, Ct, C=None,
                ref_Tf=Tf,
                ref_C_data=Cref,
                ref_C_label="$C - Obs.$",
                ref_C_error=0.1,
                normalize_time=True,
                normalize_Ct=True,
                fig_size=figsize,
                legend_loc='upper left',
                legend_locb='lower right',
                xlim=[0.,xmax[idx]],
                fig_name='img/tsang{}_multiclass'.format(idx))


            res.close()

        geo.close()
        #=====================================================================


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

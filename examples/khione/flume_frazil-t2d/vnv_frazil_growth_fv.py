
"""
Validation script for flume_frazil
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from data_manip.computation.datetimes import compute_datetimes

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # initial steady state
        self.add_study('vnv_1', 'telemac2d', 't2d_frazil_ini_fv.cas')

        # explicit thermal growth
        self.add_study('vnv_2', 'telemac2d', 't2d_frazil_growth_fv.cas')
        cas = TelemacCas('t2d_frazil_growth_fv.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_3', 'telemac2d', 't2d_frazil_growth_fv.cas', cas=cas)
        del cas

        # thermal growth with salinity
        cas = TelemacCas('t2d_frazil_growth_sal_fv.cas', get_dico('telemac2d'))
        self.add_study('vnv_4', 'telemac2d', 't2d_frazil_growth_fv.cas', cas=cas)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_5', 'telemac2d', 't2d_frazil_growth_fv.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Implicit thermal growth
        self.check_epsilons('vnv_2:T2DRES', 'f2d_growth_explicit_fv.slf', eps=[1.E-6])
        self.check_epsilons('vnv_3:T2DRES', 'f2d_growth_explicit_fv.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T2DRES', 'vnv_3:T2DRES', eps=[1.E-6])

        # Explicit thermal growth
        self.check_epsilons('vnv_4:T2DRES', 'f2d_growth_sal_fv.slf', eps=[1.E-5])
        self.check_epsilons('vnv_5:T2DRES', 'f2d_growth_sal_fv.slf', eps=[1.E-5])
        self.check_epsilons('vnv_4:T2DRES', 'vnv_5:T2DRES', eps=[1.E-5])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot2d import plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines
        import datetime
        import matplotlib.pyplot as plt

        geo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_2:T2DRES'))
        res_vnv_3 = TelemacFile(self.get_study_file('vnv_3:T2DRES'))
        res_vnv_4 = TelemacFile(self.get_study_file('vnv_4:T2DRES'))

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
            fig_size=(14, 2.5),
            fig_name='img/mesh_fv',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/bottom_fv',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # FIRST OBSERVATION:
        #
        vnv_plot2d(\
            'WATER DEPTH',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/H-2d_scalarmap_fv',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$H$ $(m)$')

        vnv_plot2d(\
            'VELOCITY U',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/U-2d_scalarmap_fv',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$U$ $(m/s)$')

        # Plotting frazil map
        vnv_plot2d(\
            'FRAZIL',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/Cf-2d_scalarmap_fv',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$C_f$ (volume fraction)')

        # Plotting temperature map
        vnv_plot2d(\
            'TEMPERATURE',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/T-2d_scalarmap_fv',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$T$ $(^\circ C)$')

        #----------------------------------------------------------------------
        # Plotting difference between scal and parallel run
        fig, ax = plt.subplots(1, 1, figsize=(14, 2.5))

        val_scal = res_vnv_2.get_data_value('FRAZIL', -1)
        val_par = res_vnv_3.get_data_value('FRAZIL', -1)
        diff = val_scal - val_par

        plot2d_scalar_filled_contour(fig, ax, res_vnv_2.tri, diff,
                                     data_name='$C_f$ (volume fraction)')

        print(" "*8+"~> Plotting img/Cf-dif_fv")
        plt.savefig('img/Cf-dif_fv')
        plt.clf()


        #----------------------------------------------------------------------
        res_list = [res_vnv_2, res_vnv_4]
        res_labels = ['_explicit', '_salinity']

        for idx, res in enumerate(res_list):

            # Plot timeseries on points:
            xpos = [1000, 9000]
            for x in xpos:
                plot1d_history_TCf(\
                    res=res,
                    points=[[x, 75.]],
                    xlim=[datetime.datetime(2016, 12, 2, 0, 0, 0),
                          datetime.datetime(2016, 12, 2, 1, 0, 0)],
                    fig_name='img/timeseries_TCf{}_at_xy={}_75_fv'\
                        .format(res_labels[idx], int(x)))

            # Plotting profiles
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res,
                legend_labels='free surface',
                record=-1,
                poly=[[0., 75.], [10000., 75.]],
                poly_number=[50],
                fig_size=(10, 3),
                y_label='$z$ $(m)$',
                x_label='$x$ $(m)$',
                fig_name='img/profile_elevation{}_fv'.format(res_labels[idx]),
                plot_bottom=True)

            tf = res_vnv_2.times[-1]
            times = [tf/2., tf]

            vnv_plot1d_polylines(\
                'TEMPERATURE',
                res,
                legend_labels='',
                time=times,
                poly=[[0., 75.], [10000., 75.]],
                poly_number=[150],
                fig_size=(10, 4),
                y_label='$T$ $(\degree C)$',
                x_label='$x$ $(m)$',
                fig_name='img/profile_T{}_fv'.format(res_labels[idx]),
                plot_bottom=False)

            vnv_plot1d_polylines(\
                'FRAZIL',
                res,
                legend_labels='',
                time=times,
                poly=[[0., 75.], [10000., 75.]],
                poly_number=[150],
                fig_size=(10, 4),
                y_label='$C_f$ (volume fraction)',
                x_label='$x$ $(m)$',
                fig_name='img/profile_Cf{}_fv'.format(res_labels[idx]),
                plot_bottom=False)

            if 'SALINITY' in res.varnames:
                vnv_plot1d_polylines(\
                    'SALINITY',
                    res,
                    legend_labels='',
                    time=times,
                    poly=[[0., 75.], [10000., 75.]],
                    poly_number=[150],
                    fig_size=(10, 4),
                    y_label='$S$ (ppt)',
                    x_label='$x$ $(m)$',
                    fig_name='img/profile_S{}_fv'.format(res_labels[idx]),
                    plot_bottom=False)

        # Closing files
        geo.close()
        res_vnv_1.close()
        res_vnv_2.close()
        res_vnv_3.close()
        res_vnv_4.close()

def plot1d_history_TCf(res, points, xlim=None, fig_name=''):
    """
    Plot 1d timeseries of temperature and frazil
    """
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt
    from postel.deco_vnv import decoVNV, decoVNV_1d
    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    fig, ax = plt.subplots(1, 1, figsize=(10, 4))
    axb = ax.twinx()
    # get data values
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    C = res.get_timeseries_on_points('FRAZIL', points)
    # get datetimes
    datetimes = compute_datetimes(res.times,
                                  initial_date=res.datetime)
    # plot both T and Cf
    ax.plot(datetimes, T[0, :], label='Temperature', color='#002d74')
    axb.grid()
    axb.plot(datetimes, C[0, :], label='Frazil', color='#e85113')
    # set labels
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%M"))
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    ax.set_xlabel('t (min)')
    ax.set_ylabel(r'$T$ $(\degree C)$')
    axb.set_ylabel('$C_f$ (volume fraction)')
    ax.legend(loc=3)
    axb.legend(loc=4)
    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)
    # Close figure:
    fig.clf()
    plt.close()

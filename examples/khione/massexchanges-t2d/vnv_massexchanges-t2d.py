
"""
Validation script for vnv_massexchanges-t2d
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
        self.tags = ['telemac2d', 'khione', 'fv']
        self.checkbounds = False

    def _pre(self):
        """
        Defining the studies
        """
        # initial steady state
        self.add_study('vnv_1', 'telemac2d', 't2d_massexchanges_ini.cas')

        # frazil ice growth and mass-exchanges with ice cover
        self.add_study('vnv_2', 'telemac2d', 't2d_massexchanges_1.cas')
        cas = TelemacCas('t2d_massexchanges_1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_3', 'telemac2d', 't2d_massexchanges_1_par.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Check bounds
        if self.checkbounds:
            res_ice = TelemacFile(self.get_study_file('vnv_2:ICERES'))
            cf_temp = res_ice.get_data_value('FRAZIL', -1)
            ci_temp = res_ice.get_data_value('ICE COVER FRAC.', -1)
            ti_temp = res_ice.get_data_value('ICE COVER THICK.', -1)
            # Minimum frazil ice volume fraction with default thermal
            # growth parameters i.e. with r=1.2e-4, min_nk=1.5e4 and
            # diameter to thickness ratio = 10.
            MIN_CF_DEFAULT = 1.6286016316209485e-08
            print(" MAX VALUES :")
            print(" MAX CF = ", max(cf_temp), "UPPER BOUND CF = ", 1.)
            print(" MAX CI = ", max(ci_temp), "UPPER BOUND CI = ", 1.)
            print(" MAX TI = ", max(ti_temp))
            print(" MIN VALUES :")
            print(" MIN CF = ", min(cf_temp), "LOWER BOUND CF = ", MIN_CF_DEFAULT)
            print(" MIN CI = ", min(ci_temp), "LOWER BOUND CI = ", 0.)
            print(" MIN TI = ", min(ti_temp))

        # check T2D results
        self.check_epsilons('vnv_2:T2DRES', 'f2d_massexchanges.slf', eps=[1.E-6])
        self.check_epsilons('vnv_3:T2DRES', 'f2d_massexchanges.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T2DRES', 'vnv_3:T2DRES', eps=[1.E-6])
        # check ice results
        self.check_epsilons('vnv_2:ICERES', 'fce_massexchanges.slf', eps=[1.E-6])
        self.check_epsilons('vnv_3:ICERES', 'fce_massexchanges.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES', 'vnv_3:ICERES', eps=[1.E-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot2d import plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines
        import datetime
        import matplotlib.pyplot as plt

        geo, _ = self.get_study_res('vnv_2:T2DGEO', load_bnd=True)
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_2:T2DRES'))
        res_ice_vnv_2 = TelemacFile(self.get_study_file('vnv_2:ICERES'))

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
            fig_name='img/mesh',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',)

        # Plotting bottom
        vnv_plot2d(\
            'BOTTOM',
            geo,
            record=0,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/bottom',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='Bottom (m)')

        #======================================================================
        # 2D SCALAR MAPS:
        #
        records = [0, int(len(res_vnv_2.times)/2.), -1]

        for record in records:

            vnv_plot2d(\
                'FRAZIL',
                res_ice_vnv_2,
                record=record,
                filled_contours=True,
                fig_size=(14, 2.5),
                fig_name='img/Cf-2d_scalarmap_{}'.format(record),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$C_f$ (-)')

            vnv_plot2d(\
                'TEMPERATURE',
                res_vnv_2,
                record=record,
                filled_contours=True,
                fig_size=(14, 2.5),
                fig_name='img/T-2d_scalarmap_{}'.format(record),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$T$ $(^\circ {C})$')

            vnv_plot2d(\
                'ICE COVER FRAC.',
                res_ice_vnv_2,
                record=record,
                filled_contours=True,
                fig_size=(14, 2.5),
                fig_name='img/Ci-2d_scalarmap_{}'.format(record),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$C_i$ (-)')

            vnv_plot2d(\
                'ICE COVER THICK.',
                res_ice_vnv_2,
                record=record,
                filled_contours=True,
                fig_size=(14, 2.5),
                fig_name='img/ti-2d_scalarmap_{}'.format(record),
                x_label='$x$ $(m)$',
                y_label='$y$ $(m)$',
                cbar_label='$t_i$ (m)')

        #----------------------------------------------------------------------
        res_list = [res_vnv_2]
        res_ice_list = [res_ice_vnv_2]
        res_labels = ['']

        for idx, res in enumerate(res_list):

            # Plot timeseries on points:
            xpos = [1000, 9000]
            for x in xpos:
                plot1d_history_TCf(\
                    res=res,
                    res_ice=res_ice_list[idx],
                    points=[[x, 75.]],
                    xlim=[datetime.datetime(2016, 12, 2, 0, 0, 0),
                          datetime.datetime(2016, 12, 2, 3, 0, 0)],
                    fig_name='img/timeseries_TCf{}_at_xy={}_75'\
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
                fig_name='img/profile_elevation{}'.format(res_labels[idx]),
                plot_bottom=True)

            tf = res_vnv_2.times[-1]
            times = [tf/4., tf/2., 3.*tf/4., tf]

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
                fig_name='img/profile_T{}'.format(res_labels[idx]),
                plot_bottom=False)

            vnv_plot1d_polylines(\
                'FRAZIL',
                res_ice_list[idx],
                legend_labels='',
                time=times,
                poly=[[0., 75.], [10000., 75.]],
                poly_number=[150],
                fig_size=(10, 4),
                y_label='$C_f$ (-)',
                x_label='$x$ $(m)$',
                fig_name='img/profile_Cf{}'.format(res_labels[idx]),
                plot_bottom=False)

            vnv_plot1d_polylines(\
                'ICE COVER FRAC.',
                res_ice_list[idx],
                legend_labels='',
                time=times,
                poly=[[0., 75.], [10000., 75.]],
                poly_number=[150],
                fig_size=(10, 4),
                y_label='$C_i$ (-)',
                x_label='$x$ $(m)$',
                fig_name='img/profile_Ci{}'.format(res_labels[idx]),
                plot_bottom=False)

            vnv_plot1d_polylines(\
                'ICE COVER THICK.',
                res_ice_list[idx],
                legend_labels='',
                time=times,
                poly=[[0., 75.], [10000., 75.]],
                poly_number=[150],
                fig_size=(10, 4),
                y_label='$t_i$ (m)',
                x_label='$x$ $(m)$',
                fig_name='img/profile_ti{}'.format(res_labels[idx]),
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
                    fig_name='img/profile_S{}'.format(res_labels[idx]),
                    plot_bottom=False)

        # Closing files
        geo.close()
        res_vnv_2.close()

def plot1d_history_TCf(res, res_ice, points, xlim=None, fig_name=''):
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
    C = res_ice.get_timeseries_on_points('FRAZIL', points)
    # get datetimes
    datetimes = compute_datetimes(res.times,
                                  initial_date=res.datetime)
    # plot both T and Cf
    ax.plot(datetimes, T[0, :], label='Temperature', color='#002d74')
    axb.grid()
    axb.plot(datetimes, C[0, :], label='Frazil', color='#e85113')
    # set labels
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%H-%M"))
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
    plt.close('all')

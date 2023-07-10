
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
        self.rank = 2
        self.tags = ['telemac2d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """
        # implicit thermal growth
        self.add_study('vnv_1', 'telemac2d', 't2d_frazil_meltdown.cas')

        # implicit thermal growth (parallel)
        cas = TelemacCas('t2d_frazil_meltdown.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_2', 'telemac2d', 't2d_frazil_meltdown_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Implicit thermal growth
        self.check_epsilons('vnv_1:T2DRES', 'f2d_meltdown.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T2DRES', 'f2d_meltdown.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:T2DRES', 'vnv_2:T2DRES', eps=[1.E-6])
        # Implicit thermal growth
        self.check_epsilons('vnv_1:ICERES', 'fce_meltdown.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES', 'fce_meltdown.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'vnv_2:ICERES', eps=[1.E-6])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines
        import datetime
        import matplotlib.pyplot as plt

        res_vnv_1 = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        res_ice_vnv_1 = TelemacFile(self.get_study_file('vnv_1:ICERES'))
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_2:T2DRES'))

        #----------------------------------------------------------------------
        res_list = [res_vnv_1]
        res_ice_list = [res_ice_vnv_1]
        res_labels = ['_meltdown']

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
                y_label='$C_f$ (volume fraction)',
                x_label='$x$ $(m)$',
                fig_name='img/profile_Cf{}'.format(res_labels[idx]),
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
        res_vnv_1.close()
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
    ax.set_xlabel('t ')
    ax.set_ylabel(r'$T$ $(\degree C)$')
    axb.set_ylabel('$C_f$ (volume fraction)')
    ax.legend(loc=3)
    axb.legend(loc=4)
    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)
    # Close figure:
    plt.close('all')

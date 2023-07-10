
"""
Validation script for muono-class frazil ice model
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
        self.rank = 3
        self.tags = ['telemac3d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # thermal budget under icy conditions (serial)
        self.add_study('vnv_1', 'telemac3d', 't3d_reservoir_monoclass.cas')

        # thermal budget under icy conditions (parallel)
        cas = TelemacCas('t3d_reservoir_monoclass.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_2', 'telemac3d', 't3d_reservoir_monoclass_par.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('vnv_1:T3DRES', 'f3d_reservoir_monoclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T3DRES', 'f3d_reservoir_monoclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:T3DRES', 'vnv_2:T3DRES', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'fce_reservoir_monoclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES', 'fce_reservoir_monoclass.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'vnv_2:ICERES', eps=[1.E-6])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines
        import datetime
        import matplotlib.pyplot as plt

        res = TelemacFile(self.get_study_file('vnv_1:T3DRES'))
        res_ice = TelemacFile(self.get_study_file('vnv_1:ICERES'))

        # Plot timeseries on points:
        plot1d_history_TCf(\
            res=res,
            res_ice=res_ice,
            points=[[0., 0.]],
            fig_name='img/monoclass_TCf')

        # Closing files
        res.close()


def plot1d_history_TCf(res, res_ice, points, xlim=None, fig_name=''):
    """
    Plot 1d timeseries of temperature and frazil
    """
    import numpy as np
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt
    from postel.deco_vnv import decoVNV, decoVNV_1d

    # plot initialization
    plt.style.use('default')
    plt.rcParams.update(decoVNV)
    plt.rcParams.update(decoVNV_1d)
    fig, ax = plt.subplots(1, 1, figsize=(6., 4.))
    axb = ax.twinx()

    # get data values
    T = res_ice.get_timeseries_on_points('TEMPERATURE', points)
    C = res_ice.get_timeseries_on_points('FRAZIL', points)

    # get datetimes
    #datetimes = compute_datetimes(res.times, initial_date=res.datetime)

    # plot both T and Cf
    ax.plot(res.times, T[0, :], label='$T$', color='#002d74')
    axb.grid()
    axb.plot(res.times, C[0, :], label='$C$', color='#e85113')

    # set labels
    #ax.xaxis.set_major_formatter(mdates.DateFormatter("%M"))
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    ax.set_xlabel('t (s)')
    ax.set_ylabel(r'$T$ $(\degree C)$')
    axb.set_ylabel('$C$ (-)')
    ax.legend(loc=3)
    axb.legend(loc=4)

    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)

    # Close figure:
    plt.close('all')

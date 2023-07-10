
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
        self.rank = 3
        self.tags = ['telemac3d', 'khione']

    def _pre(self):
        """
        Defining the studies
        """

        # explicit thermal growth
        self.add_study('vnv_1', 'telemac3d', 't3d_frazil_meltdown.cas')
        cas = TelemacCas('t3d_frazil_meltdown.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_2', 'telemac3d', 't3d_frazil_meltdown.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Explicit thermal growth
        self.check_epsilons('vnv_1:T3DRES', 'f3d_meltdown.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T3DRES', 'f3d_meltdown.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:T3DRES', 'vnv_2:T3DRES', eps=[1.E-6])
        # Explicit thermal growth
        self.check_epsilons('vnv_1:ICERES', 'fce_meltdown.slf', eps=[1.E-3])
        self.check_epsilons('vnv_2:ICERES', 'fce_meltdown.slf', eps=[1.E-3])
        self.check_epsilons('vnv_1:ICERES', 'vnv_2:ICERES', eps=[1.E-3])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot2d import plot2d_scalar_filled_contour
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines
        import datetime
        import matplotlib.pyplot as plt

        geo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module='T3D')
        res_vnv_2 = TelemacFile(self.get_study_file('vnv_1:T3DRES'))
        res_ice_vnv_2 = TelemacFile(self.get_study_file('vnv_1:ICERES'))
        res_vnv_3 = TelemacFile(self.get_study_file('vnv_2:T3DRES'))
        res_ice_vnv_3 = TelemacFile(self.get_study_file('vnv_2:ICERES'))

        #======================================================================
        # FIRST OBSERVATION:
        #
        vnv_plot2d(\
            'VELOCITY U',
            res_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/U-2d_scalarmap_melt',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$U$ $(m/s)$',
            plane=4)

        # Plotting frazil map
        vnv_plot2d(\
            'FRAZIL',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/Cf-2d_scalarmap_melt',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$C_f$ (volume fraction)',
            plane=4)

        # Plotting temperature map
        vnv_plot2d(\
            'TEMPERATURE',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/T-2d_scalarmap_melt',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$T$ $(^\circ {C})$',
            plane=4)

        # Plotting frazil map
        vnv_plot2d(\
            'FRAZIL S',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/Cf-surface_scalarmap_melt',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$C_f$ (volume fraction)',
            plane=4)

        # Plotting temperature map
        vnv_plot2d(\
            'TEMPERATURE S',
            res_ice_vnv_2,
            record=-1,
            filled_contours=True,
            fig_size=(14, 2.5),
            fig_name='img/T-surface_scalarmap_melt',
            x_label='$x$ $(m)$',
            y_label='$y$ $(m)$',
            cbar_label='$T$ $(^\circ {C})$',
            plane=4)

        # Closing files
        geo.close()
        res_vnv_2.close()
        res_vnv_3.close()

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
    plt.close('all')

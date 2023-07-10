
"""
Validation script for ice cover 
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

        #=======================================================================
        # undercover deposition (monoclass, serial)
        self.add_study('vnv_1', 'telemac2d', 't2d_reservoir_icover.cas')

        # undercover deposition (monoclass, parallel)
        cas = TelemacCas('t2d_reservoir_icover.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_2', 'telemac2d', 't2d_reservoir_icover_par.cas', cas=cas)
        del cas

        #=======================================================================
        # undercover deposition (multiclass, serial)
        self.add_study('vnv_3', 'telemac2d', 't2d_reservoir_icover_m.cas')

        # undercover deposition (multiclass, parallel)
        cas = TelemacCas('t2d_reservoir_icover_m.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_4', 'telemac2d', 't2d_reservoir_icover_m_par.cas', cas=cas)
        del cas

        #=======================================================================
        # thermal expansion of ice cover
        self.add_study('vnv_5', 'telemac2d', 't2d_reservoir_icover_t1.cas')

        # thermal expansion of ice cover
        cas = TelemacCas('t2d_reservoir_icover_t1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_6', 'telemac2d', 't2d_reservoir_icover_t1_par.cas', cas=cas)
        del cas

        #=======================================================================
        # thermal decay of ice cover
        self.add_study('vnv_7', 'telemac2d', 't2d_reservoir_icover_t2.cas')

        # thermal decay of ice cover
        cas = TelemacCas('t2d_reservoir_icover_t2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_8', 'telemac2d', 't2d_reservoir_icover_t2_par.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('vnv_1:T2DRES', 'f2d_reservoir_icover.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:T2DRES', 'f2d_reservoir_icover.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:T2DRES', 'vnv_2:T2DRES', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'fce_reservoir_icover.slf', eps=[1.E-6])
        self.check_epsilons('vnv_2:ICERES', 'fce_reservoir_icover.slf', eps=[1.E-6])
        self.check_epsilons('vnv_1:ICERES', 'vnv_2:ICERES', eps=[1.E-6])

        self.check_epsilons('vnv_3:T2DRES', 'f2d_reservoir_icover_m.slf', eps=[1.E-6])
        self.check_epsilons('vnv_4:T2DRES', 'f2d_reservoir_icover_m.slf', eps=[1.E-6])
        self.check_epsilons('vnv_3:T2DRES', 'vnv_4:T2DRES', eps=[1.E-6])
        self.check_epsilons('vnv_3:ICERES', 'fce_reservoir_icover_m.slf', eps=[1.E-6])
        self.check_epsilons('vnv_4:ICERES', 'fce_reservoir_icover_m.slf', eps=[1.E-6])
        self.check_epsilons('vnv_3:ICERES', 'vnv_4:ICERES', eps=[1.E-6])

        self.check_epsilons('vnv_5:ICERES', 'fce_reservoir_icover_t1.slf', eps=[1.E-6])
        self.check_epsilons('vnv_6:ICERES', 'fce_reservoir_icover_t1.slf', eps=[1.E-6])
        self.check_epsilons('vnv_5:ICERES', 'vnv_6:ICERES', eps=[1.E-6])

        self.check_epsilons('vnv_7:ICERES', 'fce_reservoir_icover_t2.slf', eps=[1.E-6])
        self.check_epsilons('vnv_8:ICERES', 'fce_reservoir_icover_t2.slf', eps=[1.E-6])
        self.check_epsilons('vnv_7:ICERES', 'vnv_8:ICERES', eps=[1.E-6])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d,\
            vnv_plot1d_polylines, vnv_plot1d_history
        import datetime
        import matplotlib.pyplot as plt

        #=======================================================================
        # monoclass
        geo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('vnv_1:T2DRES'))
        res_ice = TelemacFile(self.get_study_file('vnv_1:ICERES'))

        plot1d_history_TCf(\
            res=res,
            res_ice=res_ice,
            points=[[0., 0.]],
            fig_name='img/icover_monoclass_TCf')

        tf = res.times[-1]
        times = [tf/4., tf/2., 3.*tf/4., tf]

        vnv_plot1d_history(\
            'FRAZIL',
            res_ice,
            legend_labels='',
            points=[[0., 0.]],
            points_labels=['[0., 0.]'],
            fig_size=(10, 4),
            y_label='$C_f$ (-)',
            fig_name='img/icover_monoclass_Cf')

        vnv_plot1d_history(\
            'ICE COVER FRAC.',
            res_ice,
            legend_labels='',
            points=[[0., 0.]],
            points_labels=['[0., 0.]'],
            fig_size=(10, 4),
            y_label='$C_i$ (-)',
            fig_name='img/icover_monoclass_Ci')

        vnv_plot1d_history(\
            'ICE COVER THICK.',
            res_ice,
            legend_labels='',
            points_labels=['[0., 0.]'],
            points=[[0., 0.]],
            fig_size=(10, 4),
            y_label='$t_i$ (m)',
            fig_name='img/icover_monoclass_ti')

        geo.close()
        res.close()
        res_ice.close()

        #=======================================================================
        # multiclass
        geo, _ = self.get_study_res('vnv_3:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('vnv_3:T2DRES'))
        res_ice = TelemacFile(self.get_study_file('vnv_3:ICERES'))

        plot1d_history_TCf_m(\
            res=res_ice,
            points=[[0., 0.]],
            fig_name='img/icover_multiclass_TCf')

        tf = res.times[-1]
        times = [tf/4., tf/2., 3.*tf/4., tf]

        vnv_plot1d_history(\
            'ICE COVER FRAC.',
            res_ice,
            legend_labels='',
            points=[[0., 0.]],
            points_labels=['[0., 0.]'],
            fig_size=(10, 4),
            y_label='$C_i$ (-)',
            fig_name='img/icover_multiclass_Ci')

        vnv_plot1d_history(\
            'ICE COVER THICK.',
            res_ice,
            legend_labels='',
            points_labels=['[0., 0.]'],
            points=[[0., 0.]],
            fig_size=(10, 4),
            y_label='$t_i$ (m)',
            fig_name='img/icover_multiclass_ti')

        geo.close()
        res.close()
        res_ice.close()

        #=======================================================================
        # thermal growth
        geo, _ = self.get_study_res('vnv_5:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('vnv_5:T2DRES'))
        res_ice = TelemacFile(self.get_study_file('vnv_5:ICERES'))

        plot1d_history_TCf(\
            res=res,
            res_ice=res_ice,
            points=[[0., 0.]],
            fig_name='img/icover_thermal_growth_TCf')

        tf = res.times[-1]
        times = [tf/4., tf/2., 3.*tf/4., tf]

        vnv_plot1d_history(\
            'ICE COVER FRAC.',
            res_ice,
            legend_labels='',
            points=[[0., 0.]],
            points_labels=['[0., 0.]'],
            fig_size=(10, 4),
            y_label='$C_i$ (-)',
            fig_name='img/icover_thermal_growth_Ci')

        vnv_plot1d_history(\
            'ICE COVER THICK.',
            res_ice,
            legend_labels='',
            points_labels=['[0., 0.]'],
            points=[[0., 0.]],
            fig_size=(10, 4),
            y_label='$t_i$ (m)',
            fig_name='img/icover_thermal_growth_ti')

        geo.close()
        res.close()
        res_ice.close()

        #=======================================================================
        # thermal melting
        geo, _ = self.get_study_res('vnv_7:T2DGEO', load_bnd=True)
        res = TelemacFile(self.get_study_file('vnv_7:T2DRES'))
        res_ice = TelemacFile(self.get_study_file('vnv_7:ICERES'))

        plot1d_history_TCf(\
            res=res,
            res_ice=res_ice,
            points=[[0., 0.]],
            fig_name='img/icover_thermal_melting_TCf')

        tf = res.times[-1]
        times = [tf/4., tf/2., 3.*tf/4., tf]

        vnv_plot1d_history(\
            'ICE COVER FRAC.',
            res_ice,
            legend_labels='',
            points=[[0., 0.]],
            points_labels=['[0., 0.]'],
            fig_size=(10, 4),
            y_label='$C_i$ (-)',
            fig_name='img/icover_thermal_melting_Ci')

        vnv_plot1d_history(\
            'ICE COVER THICK.',
            res_ice,
            legend_labels='',
            points_labels=['[0., 0.]'],
            points=[[0., 0.]],
            fig_size=(10, 4),
            y_label='$t_i$ (m)',
            fig_name='img/icover_thermal_melting_ti')

        geo.close()
        res.close()
        res_ice.close()


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
    fig, ax = plt.subplots(1, 1, figsize=(10, 4))
    axb = ax.twinx()

    # get data values
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    C = res_ice.get_timeseries_on_points('FRAZIL', points)

    # get datetimes
    #datetimes = compute_datetimes(res.times, initial_date=res.datetime)

    # plot both T and Cf
    ax.plot(res.times, T[0, :], label='Temperature', color='#002d74')
    axb.grid()
    axb.plot(res.times, C[0, :], label='Frazil', color='#e85113')

    # set labels
    #ax.xaxis.set_major_formatter(mdates.DateFormatter("%M"))
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    ax.set_xlabel('t (s)')
    ax.set_ylabel(r'$T$ $(\degree C)$')
    axb.set_ylabel('$C_f$ (volume fraction)')
    ax.legend(loc=3)
    axb.legend(loc=4)

    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)

    # Close figure:
    plt.close('all')


def plot1d_history_TCf_m(res, points,  ncfra=4, xlim=None, fig_name=''):
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
    fig, ax = plt.subplots(1, 1, figsize=(10, 4))
    axb = ax.twinx()

    # get data values
    T = res.get_timeseries_on_points('TEMPERATURE', points)
    Ci = []
    for i in range(1, ncfra+1):
        C = res.get_timeseries_on_points('FRAZIL CLASS {}'.format(i), points)
        Ci.append(C[0, :])

    C = res.get_timeseries_on_points('TOTAL CONCENTRAT', points)

    # get datetimes
    #datetimes = compute_datetimes(res.times, initial_date=res.datetime)

    # plot both T and Cf
    ax.plot(res.times, T[0, :], label='Temperature', color='#002d74')
    axb.grid()
    for i in range(ncfra):
        axb.plot(res.times, Ci[i], label='Frazil class {}'.format(i+1))
    axb.plot(res.times, C[0, :], label='Frazil total')

    # set labels
    #ax.xaxis.set_major_formatter(mdates.DateFormatter("%M"))
    if xlim is not None:
        ax.set_xlim(xlim[0], xlim[1])
    ax.set_xlabel('t (s)')
    ax.set_ylabel(r'$T$ $(\degree C)$')
    axb.set_ylabel('$C_f$ (volume fraction)')
    ax.legend(loc=3)
    axb.legend(loc=4)

    # save figure:
    print(" "*8+"~> Plotting {}".format(fig_name))
    fig.savefig(fig_name)

    # Close figure:
    plt.close('all')

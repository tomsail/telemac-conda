
"""
Validation script for waq2d_micropol
"""
import math
import numpy as np
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
        self.rank = 1
        self.tags = ['telemac2d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # water quality- dissolved biomas process
        self.add_study('vnv_sed_sorp_seq',
                       'telemac2d',
                       't2d_waq2d_micropol.cas')

        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_sed_sorp_par',
                       'telemac2d',
                       't2d_waq2d_micropol_par.cas',
                       cas=cas)

        del cas

        # sedimentation and desorption
        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        self.add_study('vnv_sed_desorp_seq',
                       'telemac2d',
                       't2d_waq2d_micropol.cas',
                       cas=cas)

        del cas

        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        self.add_study('vnv_sed_desorp_par',
                       'telemac2d',
                       't2d_waq2d_micropol_par.cas',
                       cas=cas)

        del cas

        # sedimentation in a lake at rest
        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 0., 0.])
        self.add_study('vnv_sed_seq',
                       'telemac2d',
                       't2d_waq2d_micropol.cas',
                       cas=cas)

        del cas

        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 0., 0.])
        self.add_study('vnv_sed_par',
                       'telemac2d',
                       't2d_waq2d_micropol_par.cas',
                       cas=cas)

        del cas

        #  sorption
        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_seq',
                       'telemac2d',
                       't2d_waq2d_micropol.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_par',
                       'telemac2d',
                       't2d_waq2d_micropol_par.cas',
                       cas=cas)

        del cas

        # sedimentation and desorption with decay
        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        sed_cas = TelemacCas('waq_micropol_steer.cas', get_dico('waqtel'))
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 1.13E-7)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sed_desorp_decay_seq',
                       'telemac2d',
                       't2d_waq2d_micropol.cas',
                       cas=cas)

        del cas

        cas = TelemacCas('t2d_waq2d_micropol.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        sed_cas = TelemacCas('waq_micropol_steer.cas', get_dico('waqtel'))
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 1.13E-7)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sed_desorp_decay_par',
                       'telemac2d',
                       't2d_waq2d_micropol_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_sorp_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_sorp.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_sorp_seq:T2DRES',
                            'vnv_sed_sorp_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_desorp_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_desorp.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_desorp_seq:T2DRES',
                            'vnv_sed_desorp_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_seq:T2DRES',
                            'f2d_waq2d_micropol_sed.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_seq:T2DRES',
                            'vnv_sed_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sorp_seq:T2DRES',
                            'f2d_waq2d_micropol_sorp.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sorp_seq:T2DRES',
                            'vnv_sorp_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_desorp_decay_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_desorp_decay.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_desorp_decay_seq:T2DRES',
                            'vnv_sed_desorp_decay_par:T2DRES',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        from postel.plot1d import plot1d

        # Getting geometry file
        geo, _ = self.get_study_res('vnv_sed_sorp_seq:T2DGEO', load_bnd=True)

        # Plotting mesh
        vnv_plot2d('',
                   geo,
                   plot_mesh=True,
                   fig_size=(7., 7.),
                   fig_name='img/pol/res_mesh')

        # Getting files
        for res_name, name in [('vnv_sed_sorp_seq:T2DRES', '1_0_0_1_0'),
                               ('vnv_sed_desorp_seq:T2DRES', '1_0_1_0_0'),
                               ('vnv_sed_seq:T2DRES', '1_0_0_0_0'),
                               ('vnv_sorp_seq:T2DRES', '1_0_1_0_0_w_0'),
                               ('vnv_sed_desorp_decay_seq:T2DRES', '1_0_0_1_0_decay')]:
            res = TelemacFile(self.get_study_file(res_name))

            #
#            Plotting final condition for Traceur 1 (at -1)
           # vnv_plot2d('Traceur 1',
           #            res,
           #            poly=[[10, 110], [210, 110]],
           #            record=-1,
           #            filled_contours=True,
           #            x_label='$x$ (m)', y_label='$z$ (m)',
           #            cbar_label='Tracer 1',
           #            fig_size=(20, 5),
           #            fig_name='img/pol/res_ta1')

            #----------------------------------------------------------------------
            # Comparison of tracers (1D slice):

            # Getting array of time values from file
            times = res.times/86400.

            # List of points we want to display
            points = [[0., 0.]]

            # Getting tracer values over time for each point of extraction
            data1 = res.get_timeseries_on_points('SUSPENDED LOAD', points)
            data2 = res.get_timeseries_on_points('BED SEDIMENTS', points)
            data3 = res.get_timeseries_on_points('MICRO POLLUTANT', points)
            data4 = res.get_timeseries_on_points('ABS. SUSP. LOAD.', points)
            data5 = res.get_timeseries_on_points('ABSORB. BED SED.', points)

            # Initialising figure
            fig, ax = plt.subplots(figsize=(10, 5))

            # for each plot adding a history plot with a label node_(law)
            plot1d(ax, times, data1[0, :],
                   x_label='$t$ (days)',
                   y_label='tracer concentration',
                   plot_label='SPM ($SS$)')

            # for each plot adding a history plot with a label node_(law)
            plot1d(ax, times, data2[0, :],
                   x_label='$t$ (days)',
                   y_label='tracer concentration',
                   plot_label='Bed sediment ($SF$)')

            # for each plot adding a history plot with a label node_(law)
            plot1d(ax, times, data3[0, :],
                   x_label='$t$ (days)',
                   y_label='tracer concentration',
                   plot_label='Dissolved micropollutant ($C$)')

            # for each plot adding a history plot with a label node_(law)
            plot1d(ax, times, data4[0, :],
                   x_label='$t$ (days)',
                   y_label='tracer concentration',
                   plot_label='Adsorbed suspended load ($C_{SS}$)')
#           ax.plot(x, data, label=plot_label, **kwargs)

            # for each plot adding a history plot with a label node_(law)
            plot1d(ax, times, data5[0, :],
                   x_label='$t$ (days)',
                   y_label='tracer concentration',
                   plot_label='Adsorbed bed sediment ($C_{ff}$)')
            if name != "1_0_1_0_0_w_0":
                xl = np.linspace(0, max(times), 30)
                yl = [math.exp(-4*10**(-7)*np.array(x*3600*24)) for x in xl]
                ax.plot(xl, yl, "o", label="$SS$ analytical")
                xl = np.linspace(0, max(times), 30)
                yl = [1-math.exp(-4*10**(-7)*np.array(x*3600*24)) for x in xl]
                ax.plot(xl, yl, "o", label="$SF$ analytical", color='r')
            if name == "1_0_1_0_0_w_0":
                xl = np.linspace(0, max(times), 30)
                yl = [0.5-0.5*math.exp(-5*10**(-7)*np.array(x*3600*24)) for x in xl]
                ax.plot(xl, yl, "o", label="$C_{SS}$ analytical")
                xl = np.linspace(0, max(times), 30)
                yl = [0.5+0.5*math.exp(-5*10**(-7)*np.array(x*3600*24)) for x in xl]
                ax.plot(xl, yl, "o", label="$C$ analytical", color='r')

            # Displaying legend
            ax.legend()

            fig_name = 'img/pol/res_tracers_{}'.format(name)
            print(" "*8+'~> Plotting '+fig_name)
            plt.savefig(fig_name)
            plt.close('all')

            res.close()

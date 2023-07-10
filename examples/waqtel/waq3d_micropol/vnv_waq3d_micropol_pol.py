
"""
Validation script for sedimentation in waqtel 3D
"""
import math
import numpy as np
import matplotlib.pyplot as plt
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot_vnv import vnv_plot2d
from postel.plot1d import plot1d

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac3d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # simple sorption cas file
        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_pol.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sorp.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_pol.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sorp_par.cas',
                       cas=cas)
        del cas

        # sorption and decay
        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_pol.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 8.3E-3)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_decay_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sorp_decay.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_pol.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 8.3E-3)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_decay_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sorp_decay_par.cas',
                       cas=cas)
        del cas

        # sorption and sedimentation
        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        self.add_study('vnv_sed_sorp_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_sorp.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        self.add_study('vnv_sed_sorp_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_sorp_par.cas',
                       cas=cas)
        del cas

        # simple sedimentation
        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 0., 0.])
        self.add_study('vnv_sed_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sed.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 0., 0.])
        self.add_study('vnv_sed_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_par.cas',
                       cas=cas)
        del cas

        # sedimentation and desorption
        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        self.add_study('vnv_sed_desorp_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_desorp.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        self.add_study('vnv_sed_desorp_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_desorp_par.cas',
                       cas=cas)
        del cas

        # desorption and decay
        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_pol.cas', get_dico('waqtel'))
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 8.3E-3)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sed_desorp_decay_seq',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_desorp_decay.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t3d_waq3d_micropol_pol.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_pol.cas', get_dico('waqtel'))
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 8.3E-3)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sed_desorp_decay_par',
                       'telemac3d',
                       't3d_waq3d_micropol_sed_desorp_decay_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_sorp_seq:T3DRES',
                            'f3d_waq3d_micropol_pol_sed_sorp.slf',
                            eps=[1.E-13])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_sorp_seq:T3DRES',
                            'vnv_sed_sorp_par:T3DRES',
                            eps=[1.E-13])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sorp_seq:T3DRES',
                            'f3d_waq3d_micropol_pol_sorp.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sorp_seq:T3DRES',
                            'vnv_sorp_par:T3DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sorp_decay_seq:T3DRES',
                            'f3d_waq3d_micropol_pol_sorp_decay.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sorp_decay_seq:T3DRES',
                            'vnv_sorp_decay_par:T3DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_seq:T3DRES',
                            'f3d_waq3d_micropol_pol_sed.slf',
                            eps=[1.E-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_seq:T3DRES',
                            'vnv_sed_par:T3DRES',
                            eps=[1.E-12])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_desorp_decay_seq:T3DRES',
                            'f3d_waq3d_micropol_pol_sed_desorp_decay.slf',
                            eps=[1.E-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_desorp_decay_seq:T3DRES',
                            'vnv_sed_desorp_decay_par:T3DRES',
                            eps=[1.E-12])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_desorp_seq:T3DRES',
                            'f3d_waq3d_micropol_pol_sed_desorp.slf',
                            eps=[1.E-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_desorp_seq:T3DRES',
                            'vnv_sed_desorp_par:T3DRES',
                            eps=[1.E-12])

    def _post(self):
        """
        Post-treatment processes
        """

        for res_name, name in [('vnv_sorp_seq:T3DRES', 'sorp'),
                               ('vnv_sed_seq:T3DRES', 'sed'),
                               ('vnv_sed_sorp_seq:T3DRES', 'sed_sorp'),
                               ('vnv_sed_desorp_seq:T3DRES', 'sed_desorp'),
                               ('vnv_sorp_decay_seq:T3DRES', 'sorp_decay'),
                               ('vnv_sed_desorp_decay_seq:T3DRES', 'sed_desorp_decay')]:


            res = TelemacFile(self.get_study_file(res_name))

            if name == 'sorp':
                vnv_plot2d('ELEVATION Z',
                           res,
                           plot_mesh=True,
                           fig_size=(20, 5),
                           fig_name='img/pol/res_mesh')

                vnv_plot2d('ELEVATION Z',
                           res,
                           poly=[[0., 0.0], [50., 0.]],
                           record=-1,
                           plot_mesh=True,
                           x_label='$x$ (m)', y_label='$z$ (m)',
                           fig_size=(20, 5),
                           fig_name='img/pol/res_mesh_section')

            # Getting array of time values from file
            times = res.times

            # List of points we want to display
            points = [[25., 0., 0]]

            data1 = res.get_timeseries_on_points('SUSPENDED LOAD', points)
            data2 = res.get_timeseries_on_points('BED SEDIMENTS', points)
            data3 = res.get_timeseries_on_points('MICRO POLLUTANT', points)
            data4 = res.get_timeseries_on_points('ABS. SUSP. LOAD.', points)
            data5 = res.get_timeseries_on_points('ABSORB. BED SED.', points)

            fig, ax = plt.subplots(figsize=(10, 5))

            #for each plot adding a history plot with a label node_(law)

            plot1d(ax, times, data1[0, :],
                   x_label='$t$ (s)',
                   y_label='tracer concentration',
                   plot_label='SPM ($SS$) ($z$ = 0 m)')

            plot1d(ax, times, data2[0, :],
                   linewidth=2,
                   x_label='$t$ (s)',
                   y_label='tracer concentration',
                   plot_label='Bed sediment ($SF$) ($z$ = 0 m)')

            plot1d(ax, times, data3[0, :],
                   x_label='$t$ (s)',
                   y_label='tracer concentration',
                   plot_label='C ($z$ = 0 m)')

            plot1d(ax, times, data4[0, :],
                   linewidth=2,
                   x_label='$t$ (s)',
                   y_label='tracer concentration',
                   plot_label='$C_{SS}$ ($z$ = 0 m)')

            plot1d(ax, times, data5[0, :],
                   linewidth=2,
                   x_label='$t$ (s)',
                   y_label='tracer concentration',
                   plot_label='$C_{ff}$ ($z$ = 0 m)')

            if name == 'sorp':
                xl = np.linspace(0, max(times), 30)
                yl = [0.5-0.5*math.exp(-0.02*np.array(x)) for x in xl]
                ax.plot(xl, yl, "o", label="$C_{SS}$ analytical")
                xl = np.linspace(0, max(times), 30)
                yl = [0.5+0.5*math.exp(-0.02*np.array(x)) for x in xl]
                ax.plot(xl, yl, "o", label="$C$ analytical", color='r')

            ax.legend()
            fig_name = 'img/pol/res_{}'.format(name)
            print(" "*8+'~> Plotting '+fig_name)
            plt.savefig(fig_name)
            plt.close('all')
            res.close()

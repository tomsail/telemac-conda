
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
        self.rank = 3
        self.tags = ['telemac2d', 'waqtel']

    def _pre(self):
        """
        Defining the studies
        """

        # water quality- dissolved biomas process
        self.add_study('vnv_sed_sorp_k2_seq',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas')

        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_sed_sorp_k2_par',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2_par.cas',
                       cas=cas)

        del cas

        # sedimentation and desorption
        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        self.add_study('vnv_sed_desorp_k2_seq',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas

        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        self.add_study('vnv_sed_desorp_k2_par',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas

        # sedimentation in a lake at rest
        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 0., 0.])
        self.add_study('vnv_sed_k2_seq',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas

        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 0., 0.])
        self.add_study('vnv_sed_k2_par',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas

        #  sorption
        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_kin2.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_k2_seq',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)
        del cas

        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 1., 0., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_kin2.cas', get_dico('waqtel'))
        sed_cas.set('SEDIMENT SETTLING VELOCITY', 0.)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sorp_k2_par',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas

        # sedimentation and desorption with decay
        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_kin2.cas', get_dico('waqtel'))
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 1.13E-2)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sed_desorp_decay_k2_seq',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas

        cas = TelemacCas('t2d_waq2d_micropol_kin2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('INITIAL VALUES OF TRACERS', [1., 0., 0., 1., 0.])
        sed_cas = TelemacCas('waq_micropol_steer_kin2.cas', get_dico('waqtel'))
        sed_cas.set('EXPONENTIAL DESINTEGRATION CONSTANT', 1.13E-2)
        sed_cas.write('waq_micropol_tmp.cas')
        cas.set('WAQTEL STEERING FILE', 'waq_micropol_tmp.cas')
        self.add_study('vnv_sed_desorp_decay_k2_par',
                       'telemac2d',
                       't2d_waq2d_micropol_kin2.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_sorp_k2_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_sorp_k2.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_sorp_k2_seq:T2DRES',
                            'vnv_sed_sorp_k2_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_desorp_k2_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_desorp_k2.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_desorp_k2_seq:T2DRES',
                            'vnv_sed_desorp_k2_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_k2_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_k2.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_k2_seq:T2DRES',
                            'vnv_sed_k2_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sorp_k2_seq:T2DRES',
                            'f2d_waq2d_micropol_sorp_k2.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sorp_k2_seq:T2DRES',
                            'vnv_sorp_k2_par:T2DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_sed_desorp_decay_k2_seq:T2DRES',
                            'f2d_waq2d_micropol_sed_desorp_decay_k2.slf',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_sed_desorp_decay_k2_seq:T2DRES',
                            'vnv_sed_desorp_decay_k2_par:T2DRES',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        from postel.plot1d import plot1d

        # Getting geometry file
        geo, _ = self.get_study_res('vnv_sed_sorp_k2_seq:T2DGEO', load_bnd=True)

        # Plotting mesh
        vnv_plot2d('',
                   geo,
                   plot_mesh=True,
                   fig_size=(7., 7.),
                   fig_name='img/kin2/res_mesh')

        # Getting files
        for res_name, name in [('vnv_sed_sorp_k2_seq:T2DRES', '1_0_0_1_0'),
                               ('vnv_sed_desorp_k2_seq:T2DRES', '1_0_1_0_0'),
                               ('vnv_sed_k2_seq:T2DRES', '1_0_0_0_0'),
                               ('vnv_sorp_k2_seq:T2DRES', '1_0_1_0_0_w_0'),
                               ('vnv_sed_desorp_decay_k2_seq:T2DRES', '1_0_0_1_0_decay')]:
            res = TelemacFile(self.get_study_file(res_name))

            # Comparison of tracers (1D slice):

            # Getting array of time values from file
            times = res.times

            # List of points we want to display
            points = [[0., 0.]]

            # Getting tracer values over time for each point of extraction
            data1 = res.get_timeseries_on_points('SUSPENDED LOAD', points)
            data2 = res.get_timeseries_on_points('BED SEDIMENTS', points)
            data3 = res.get_timeseries_on_points('MICRO POLLUTANT', points)
            data4 = res.get_timeseries_on_points('ABS. SUSP. LOAD.', points)
            data5 = res.get_timeseries_on_points('ABSORB. BED SED.', points)
            data6 = res.get_timeseries_on_points('ABS. SUSP. LOAD2', points)
            data7 = res.get_timeseries_on_points('ABSORB. BED SED2', points)

            # Initialising figure
            if name == "1_0_1_0_0_w_0" or name == "1_0_0_0_0":
                fig, ax = plt.subplots(figsize=(10, 5))
                # for each plot adding a history plot with a label node_(law)
                plot1d(ax, times, data1[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='SPM ($SS$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax, times, data2[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Bed sediment ($SF$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax, times, data3[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Dissolved micropollutant ($C$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax, times, data4[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Adsorbed suspended load ($C_{SS1}$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax, times, data6[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Adsorbed suspended load 2 ($C_{SS2}$)')
                if name == "1_0_1_0_0_w_0":
                    plt.xlim([0, 30])
                    xl = np.linspace(0, max(times), 200)
                    yl = [(1/7)*(1+(3+2**(1/2))*math.exp((-3+2**(1/2))/10*np.array(x))
                                 +(3-2**(1/2))*math.exp(-(3+2**(1/2))/10*np.array(x)))
                          for x in xl]
                    ax.plot(xl, yl, "o", label="$C$ analytical", color='green')

                    yl = [(1/7)*(2-(1-2*2**(1/2))*math.exp((-3+2**(1/2))/10*np.array(
                        x))-(1+2*2**(1/2))*math.exp(-(3+2**(1/2))/10*np.array(x)))
                          for x in xl]
                    ax.plot(xl, yl, "o", label="$C_{SS1}$ analytical", color='olive')

                    yl = [(1/7)*(4-(2+3*2**(1/2))*math.exp((-3+2**(1/2))/10*np.array(
                        x))+(-2+3*2**(1/2))*math.exp(-(3+2**(1/2))/10*np.array(x)))
                          for x in xl]
                    ax.plot(xl, yl, "o", label="$C_{SS2}$ analytical", color='orange')


                # Displaying legend
                ax.legend(loc='upper right')

                fig_name = 'img/kin2/res_tracers_{}'.format(name)
                print(" "*8+'~> Plotting '+fig_name)
                plt.savefig(fig_name)
                plt.close('all')
                res.close()

            else:
                fig, ax = plt.subplots(2,figsize=(10, 10))

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[0], times, data1[0, :],
                       x_label='$t$ (s)',
                       y_label='surfacic tracer concentration',
                       plot_label='SPM ($SS$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[0], times, data2[0, :],
                       x_label='$t$ (s)',
                       y_label='surfacic tracer concentration',
                       plot_label='Bed sediment ($SF$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[0], times, data5[0, :],
                       x_label='$t$ (s)',
                       y_label='surfacic tracer concentration',
                       plot_label='Adsorbed bed sediment ($C_{ff1}$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[0], times, data7[0, :],
                       x_label='$t$ (s)',
                       y_label='surfacic tracer concentration',
                       plot_label='Adsorbed bed sediment 2 ($C_{ff2}$)')

                # Displaying legend
                ax[0].legend()

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[1], times, data1[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='SPM ($SS$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[1], times, data2[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Bed sediment ($SF$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[1], times, data3[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Dissolved micropollutant ($C$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[1], times, data4[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Adsorbed suspended load ($C_{SS1}$)')

                # for each plot adding a history plot with a label node_(law)
                plot1d(ax[1], times, data6[0, :],
                       x_label='$t$ (s)',
                       y_label='volumic tracer concentration',
                       plot_label='Adsorbed suspended load 2 ($C_{SS2}$)')

                # Displaying legend
                ax[1].legend()

                fig_name = 'img/kin2/res_tracers_{}'.format(name)
                print(" "*8+'~> Plotting '+fig_name)
                plt.savefig(fig_name)
                plt.close('all')
                res.close()


"""
Validation script for canal
"""
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # canal scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_canal.cas')

        # canal parallel mode
        cas = TelemacCas('t2d_canal.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_canal_par.cas',
                       cas=cas)

        del cas


        # canal scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_canal_Z_Q_1.cas')

        # canal parallel mode
        cas = TelemacCas('t2d_canal_Z_Q_1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_canal_Z_Q_1_par.cas',
                       cas=cas)

        del cas


        # canal scalar mode
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_canal_Q_Z_2.cas')

        # canal parallel mode
        cas = TelemacCas('t2d_canal_Q_Z_2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_canal_Q_Z_2_par.cas',
                       cas=cas)

        del cas


        # canal restart scalar mode
        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_canal_restart.cas')

        # canal restart parallel mode
        cas = TelemacCas('t2d_canal_restart.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_canal_restart_par.cas',
                       cas=cas)

        del cas


        # canal from restart scalar mode
        self.add_study('vnv_9',
                       'telemac2d',
                       't2d_canal_from_restart.cas')

        # canal from restart parallel mode
        cas = TelemacCas('t2d_canal_from_restart.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('PREVIOUS COMPUTATION FILE', 'restart_canal_par.slf')

        self.add_study('vnv_10',
                       'telemac2d',
                       't2d_canal_from_restart_par.cas',
                       cas=cas)

        del cas


        # canal IORDRH 2 scalar mode
        cas = TelemacCas('t2d_canal.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR H', 2)
        self.add_study('vnv_11',
                       'telemac2d',
                       't2d_canal_IORDRH_2.cas',
                       cas=cas)

        # canal IORDRH 2 parallel mode
        cas = TelemacCas('t2d_canal.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR H', 2)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_12',
                       'telemac2d',
                       't2d_canal_par.cas',
                       cas=cas)

        del cas


        # canal IORDRH 2 restart scalar mode
        cas = TelemacCas('t2d_canal_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR H', 2)
        self.add_study('vnv_13',
                       'telemac2d',
                       't2d_canal_IORDRH_2_restart.cas',
                       cas=cas)

        del cas

        # canal IORDRH 2 restart parallel mode
        cas = TelemacCas('t2d_canal_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR H', 2)
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_14',
                       'telemac2d',
                       't2d_canal_restart_par.cas',
                       cas=cas)

        del cas


        # canal IORDRH 2 from restart scalar mode
        cas = TelemacCas('t2d_canal_from_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR H', 2)
        cas.set('PREVIOUS COMPUTATION FILE', 'restart_canal_IORDRH_2.slf')
        self.add_study('vnv_15',
                       'telemac2d',
                       't2d_canal_IORDRH_2_from_restart.cas',
                       cas=cas)

        del cas

        # canal IORDRH 2 from restart parallel mode
        cas = TelemacCas('t2d_canal_from_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR H', 2)
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('PREVIOUS COMPUTATION FILE', 'restart_canal_IORDRH_2_par.slf')

        self.add_study('vnv_16',
                       'telemac2d',
                       't2d_canal_from_restart_par.cas',
                       cas=cas)

        del cas


        # canal IORDRU 2 scalar mode
        cas = TelemacCas('t2d_canal.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR U', 2)
        self.add_study('vnv_17',
                       'telemac2d',
                       't2d_canal_IORDRU_2.cas',
                       cas=cas)

        # canal IORDRU 2 parallel mode
        cas = TelemacCas('t2d_canal.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR U', 2)
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('vnv_18',
                       'telemac2d',
                       't2d_canal_par.cas',
                       cas=cas)

        del cas


        # canal IORDRU 2 restart scalar mode
        cas = TelemacCas('t2d_canal_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR U', 2)
        self.add_study('vnv_19',
                       'telemac2d',
                       't2d_canal_IORDRU_2_restart.cas',
                       cas=cas)

        del cas

        # canal IORDRU 2 restart parallel mode
        cas = TelemacCas('t2d_canal_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR U', 2)
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_20',
                       'telemac2d',
                       't2d_canal_restart_par.cas',
                       cas=cas)

        del cas


        # canal IORDRU 2 from restart scalar mode
        cas = TelemacCas('t2d_canal_from_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR U', 2)
        cas.set('PREVIOUS COMPUTATION FILE', 'restart_canal_IORDRU_2.slf')
        self.add_study('vnv_21',
                       'telemac2d',
                       't2d_canal_IORDRU_2_from_restart.cas',
                       cas=cas)

        del cas

        # canal IORDRU 2 from restart parallel mode
        cas = TelemacCas('t2d_canal_from_restart.cas', get_dico('telemac2d'))
        cas.set('INITIAL GUESS FOR U', 2)
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('PREVIOUS COMPUTATION FILE', 'restart_canal_IORDRU_2_par.slf')

        self.add_study('vnv_22',
                       'telemac2d',
                       't2d_canal_from_restart_par.cas',
                       cas=cas)

        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_canal.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_canal.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_canal_Z_Q_1.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_canal_Z_Q_1.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_canal_Q_Z_2.slf',
                            eps=[1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'f2d_canal_Q_Z_2.slf',
                            eps=[1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[2.E-5])

        # Comparison with the restart file used to continue.
        self.check_epsilons('vnv_7:T2DRST',
                            'restart_canal.slf',
                            eps=[1.E-9])

        # Comparison with the restart file used to continue.
        self.check_epsilons('vnv_8:T2DRST',
                            'restart_canal.slf',
                            eps=[1.E-9])

        # Comparison between one way and intermediate step, sequential run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_9:T2DRES',
                            eps=[1.E-8])

        # Comparison between one way and intermediate step, parallel run.
        self.check_epsilons('vnv_2:T2DRES',
                            'vnv_10:T2DRES',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_11:T2DRES',
                            'vnv_12:T2DRES',
                            eps=[1.E-9])

        # Comparison with the restart file used to continue.
        self.check_epsilons('vnv_13:T2DRST',
                            'restart_canal_IORDRH_2.slf',
                            eps=[1.E-9])

        # Comparison with the restart file used to continue.
        self.check_epsilons('vnv_14:T2DRST',
                            'restart_canal_IORDRH_2_par.slf',
                            eps=[1.E-9])

        # Comparison between one way and intermediate step, sequential run.
        self.check_epsilons('vnv_11:T2DRES',
                            'vnv_15:T2DRES',
                            eps=[1.E-9])

        # Comparison between one way and intermediate step, parallel run.
        self.check_epsilons('vnv_12:T2DRES',
                            'vnv_16:T2DRES',
                            eps=[1.E-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_17:T2DRES',
                            'vnv_18:T2DRES',
                            eps=[1.E-8])

        # Comparison with the restart file used to continue.
        self.check_epsilons('vnv_19:T2DRST',
                            'restart_canal_IORDRU_2.slf',
                            eps=[1.E-9])

        # Comparison with the restart file used to continue.
        self.check_epsilons('vnv_20:T2DRST',
                            'restart_canal_IORDRU_2_par.slf',
                            eps=[1.E-9])

        # Comparison between one way and intermediate step, sequential run.
        self.check_epsilons('vnv_17:T2DRES',
                            'vnv_21:T2DRES',
                            eps=[1.E-8])

        # Comparison between one way and intermediate step, parallel run.
        self.check_epsilons('vnv_18:T2DRES',
                            'vnv_22:T2DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
        # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_3_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv_3_t2dres)
        vnv_5_t2dres = self.get_study_file('vnv_5:T2DRES')
        res_vnv_5_t2dres = TelemacFile(vnv_5_t2dres)

        fig, ax = plt.subplots(1, 1, figsize=(12, 9))

        poly = [[0, 50], [500, 50]]

        # Telemac 2d data
        _, abs_curv, data2d = res_vnv_1_t2dres.get_timeseries_on_polyline(\
                'FREE SURFACE', poly)

        plot1d(ax, abs_curv, data2d[:, -1],
               plot_label='Classical fluvial conditions')

        # Telemac 2d with stage-discharge curve Z = f(Q)
        _, abs_curv, data2d = res_vnv_3_t2dres.get_timeseries_on_polyline(\
                'FREE SURFACE', poly)

        plot1d(ax, abs_curv, data2d[:, -1],
               plot_label='Stage-discharge curve Z = f(Q) at the exit')

        # Telemac 2d with stage-discharge curve Q = f(Z)
        _, abs_curv, data2d = res_vnv_5_t2dres.get_timeseries_on_polyline(\
                'FREE SURFACE', poly)

        plot1d(ax, abs_curv, data2d[:, -1],
               plot_label='Stage-discharge curve Q = f(Z) at the entrance')

        ax.legend()
        ax.set_xlim(0.,500.)
        ax.set_ylim(0.5,0.75)
        ax.set_xlabel('x (m)', fontsize=14)
        ax.set_ylabel('Surface elevation (m)', fontsize=14)

        fig_name = "img/free_surface"
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')


        # Plotting horizontal split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/veloH')

        # Plotting horizontal split
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/freeSurfac')

        #Plotting Horizontal Mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   plot_mesh=True,
                   fig_size=(10, 3),
                   fig_name='img/Mesh')

        res_vnv_1_t2dres.close()
        res_vnv_3_t2dres.close()
        res_vnv_5_t2dres.close()

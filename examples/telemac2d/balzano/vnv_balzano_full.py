
"""
Validation script for balzano
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of balzano
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # charac run
        self.add_study('char_seq', 'telemac2d', 't2d_balzano-charac.cas')
        # charac run in parallel
        cas = TelemacCas('t2d_balzano-charac.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', 't2d_balzano-charac_par.cas', cas=cas)
        del cas

        #======================================================================
        # Nerd run
        self.add_study('nerd_seq', 'telemac2d', 't2d_balzano-nerd.cas')
        # Nerd run in parallel
        cas = TelemacCas('t2d_balzano-nerd.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', 't2d_balzano-nerd_par.cas', cas=cas)
        del cas

        #======================================================================
        # eria run
        self.add_study('eria_seq', 'telemac2d', 't2d_balzano-eria.cas')
        # eria run in parallel
        cas = TelemacCas('t2d_balzano-eria.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', 't2d_balzano-eria_par.cas', cas=cas)
        del cas

        #======================================================================
        # kin1 run
        self.add_study('kin1_seq', 'telemac2d', 't2d_balzano-cin1.cas')
        # kin1 run in parallel
        cas = TelemacCas('t2d_balzano-cin1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_balzano-cin1_par.cas', cas=cas)
        del cas

        #======================================================================
        # hllc run
        self.add_study('hllc_seq', 'telemac2d', 't2d_balzano-hllc.cas')
        # hllc run in parallel
        cas = TelemacCas('t2d_balzano-hllc.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc_par', 'telemac2d', 't2d_balzano-hllc_par.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        #======================================================================
        # charac check
        self.check_epsilons('char_seq:T2DRES', 'f2d_balzano-charac.slf',
                            eps=[0.53, 0.078, 0.009, 0.009, 1.E-15, 0.048, 0.53, 1.E-15])
        self.check_epsilons('char_par:T2DRES', 'f2d_balzano-charac.slf',
                            eps=[0.53, 0.13, 0.042, 0.042, 1.E-15, 0.34, 0.53, 1.E-15])
        self.check_epsilons('char_seq:T2DRES', 'char_par:T2DRES',
                            eps=[0.54, 0.13, 0.042, 0.042, 1.E-15, 0.34, 0.54, 1.E-15])

        #======================================================================
        # NERD check
        self.check_epsilons('nerd_seq:T2DRES', 'f2d_balzano-nerd.slf',
                            eps=[0.65, 0.085, 0.013, 0.013, 1.E-15, 0.048, 0.65, 1.E-15])
        self.check_epsilons('nerd_par:T2DRES', 'f2d_balzano-nerd.slf',
                            eps=[0.96, 0.37, 0.13, 0.13, 1.E-15, 2.9, 0.96, 1.E-15])
        self.check_epsilons('nerd_seq:T2DRES', 'nerd_par:T2DRES',
                            eps=[0.96, 0.37, 0.13, 0.13, 1.E-15, 2.9, 0.96, 1.E-15])

        #======================================================================
        # ERIA check
        self.check_epsilons('eria_seq:T2DRES', 'f2d_balzano-eria.slf',
                            eps=[0.74, 0.23, 0.015, 0.015, 1.E-15, 0.51, 0.74, 1.E-15])
        self.check_epsilons('eria_par:T2DRES', 'f2d_balzano-eria.slf',
                            eps=[0.81, 0.53, 0.24, 0.24, 1.E-15, 1.6, 0.82, 1.E-15])
        self.check_epsilons('eria_seq:T2DRES', 'eria_par:T2DRES',
                            eps=[0.81, 0.53, 0.24, 0.24, 1.E-15, 1.6, 0.82, 1.E-15])

        #======================================================================
        # kin1 check
        self.check_epsilons('kin1_seq:T2DRES', 'f2d_balzano-cin1.slf',
                            eps=[1.E-8])
        self.check_epsilons('kin1_par:T2DRES', 'f2d_balzano-cin1.slf',
                            eps=[1.E-12])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_par:T2DRES',
                            eps=[1.E-12])

        #======================================================================
        # hllc check
        self.check_epsilons('hllc_seq:T2DRES', 'f2d_balzano-hllc.slf',
                            eps=[1.E-8])
        self.check_epsilons('hllc_par:T2DRES', 'f2d_balzano-hllc.slf',
                            eps=[1.E-6])
        self.check_epsilons('hllc_seq:T2DRES', 'hllc_par:T2DRES',
                            eps=[1.E-6])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot3d, vnv_plotbar_cpu_times,  \
                 vnv_plot1d_polylines, vnv_plot1d_history, vnv_plot1d, vnv_plotbar
        import numpy as np
        from os import path

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plot 2d mesh and boundaries:
        geom_res, _ = self.get_study_res('nerd_seq:T2DGEO', load_bnd=True)
        nerd_res, _ = self.get_study_res('nerd_seq:T2DRES')
        kin1_res, _ = self.get_study_res('kin1_seq:T2DRES')

        vnv_plot2d(\
            'BOTTOM',
            geom_res,
            record=0,
            fig_size=(12, 2),
            fig_name="img/t2d_balzano_mesh",
            annotate_bnd=True,
            plot_mesh=True)

        # Plot 3d bathymetry:
        vnv_plot3d(\
            'BOTTOM', nerd_res,
            record=0,
            fig_size=(8, 6),
            fig_name="img/t2d_balzano_bathy")

        # Plot initial condition in slice plane:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            nerd_res,
            'initial elevation',
            fig_size=(6, 5),
            record=0,
            fig_name='img/bathymetry1D',
            plot_bottom=True)

        #======================================================================
        # First observation:
        #
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            kin1_res,
            legend_labels='KIN1',
            time=[0, 2500, 5000, 10000, 20000, 70000],
            fig_size=(6, 5),
            ref_file="DATA/analytical.txt",
            ref_label='analytic $t_\\infty$',
            y_label='z (m)',
            x_label='x (m)',
            fig_name='img/SL_firstobs',
            plot_bottom=True)

        vnv_plot1d_polylines(\
            'VELOCITY U',
            kin1_res,
            legend_labels='KIN1',
            time=[0, 2500, 5000, 10000, 20000, 70000],
            fig_size=(6, 5),
            y_label='U (m/s)',
            x_label='x (m)',
            fig_name='img/U_firstobs',
            plot_bottom=False)

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
             self.action_time,
             fig_size=(7, 3),
             fig_name='img/t2d_balzano_cpu_times')

        #----------------------------------------------------------------------
        # get a list of all sequential results:
        res_list = []
        res_labels = []
        for name, study in self.studies.items():
            # Keep only sequential results:
            if 'seq' in name:
                file = self.get_study_file(name + ':T2DRES')
                res_list.append(TelemacFile(file))
                res_labels.append(name.split('_')[0].upper())

        #----------------------------------------------------------------------
        # Maximum principle and positivity:
        #
        max_vars = []
        min_vars = []

        for idx, res in enumerate(res_list):

            # computing min and max for all time steps:
            min_dat = 10.
            max_dat = 0.

            for rec in range(res.ntimestep):
                data = res.get_data_value('WATER DEPTH', rec)

                aux1 = np.max(data)
                aux2 = np.min(data)

                if aux1 > max_dat:
                    max_dat = aux1
                if aux2 < min_dat:
                    min_dat = aux2

            max_vars.append(max_dat)
            min_vars.append(min_dat)

        vnv_plotbar(\
            [min_vars],
            fig_size=(10, 3),
            legend_labels=['min(T)'],
            x_labels=res_labels,
            y_scale='linear',
            fig_title='Minimum values of water depth at $t=T$',
            fig_name="img/t2d_balzano_minT",
            bar_width=.5,
            annotate=True,
            annotate_threshold=-1.e-12)

        vnv_plotbar(\
            [max_vars],
            fig_size=(10, 3),
            legend_labels=['max(T)'],
            x_labels=res_labels,
            y_scale='linear',
            fig_title='Maximum values of water depth at $t=T$',
            fig_name="img/t2d_balzano_maxT",
            bar_width=.5,
            annotate=True)

        #----------------------------------------------------------------------
        # Accuracy of free surface and velocity (1D slice):
        records = [i for i in range(10)]
        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                legend_labels=res_labels,
                record=record,
                fig_size=(6, 5),
                ref_file="DATA/analytical.txt",
                ref_label='analytic $t_\\infty$',
                y_label='z (m)',
                x_label='x (m)',
                fig_name='img/SL_{}'.format(int(res_list[0].times[record])),
                fig_title='$t = {}s$'.format(res_list[0].times[record]),
                plot_bottom=True)

        records = [res_list[0].ntimestep - 1 - records[i] for i in range(10)]
        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                legend_labels=res_labels,
                record=record,
                fig_size=(6, 5),
                ref_file="DATA/analytical.txt",
                ref_label='analytic $t_\\infty$',
                y_label='z (m)',
                x_label='x (m)',
                fig_name='img/SL_{}'.format(int(res_list[0].times[record])),
                fig_title='$t = {}s$'.format(res_list[0].times[record]),
                plot_bottom=True)

            vnv_plot1d_polylines(\
                'VELOCITY U',
                res_list,
                legend_labels=res_labels,
                record=record,
                fig_size=(6, 5),
                y_label='z (m)',
                x_label='x (m)',
                fig_name='img/U_{}'.format(int(res_list[0].times[record])),
                fig_title='$t = {}s$'.format(res_list[0].times[record]),
                plot_bottom=True)

        #----------------------------------------------------------------------
        # Accuracy of free surface and velocity at outlet boundary:
        vnv_plot1d_history(\
            'FREE SURFACE',
            res_list,
            legend_labels=res_labels,
            fig_size=(6, 4),
            points=[[8000., 600.]],
            points_labels=['(x=8000, y=600)'],
            ref_file="DATA/SL_downstream.txt",
            ref_label='analytic $t_\\infty$',
            fig_name="img/SL_downstream",
            fig_title="Point P2",
            y_label='z (m)',
            x_label='time (s)')

        vnv_plot1d_history(\
            'FREE SURFACE',
            res_list,
            legend_labels=res_labels,
            fig_size=(6, 4),
            points=[[10000., 600.]],
            points_labels=['(x=10000, y=600)'],
            ref_file="DATA/SL_downstream.txt",
            ref_label='analytic $t_\\infty$',
            fig_name="img/SL_downstream_2",
            fig_title="Point P2",
            y_label='z (m)',
            x_label='time (s)')

        for idx, lab in enumerate(res_labels):
            vnv_plot1d_history(\
                'FREE SURFACE',
                res_list[idx],
                legend_labels=lab,
                fig_size=(6, 4),
                points=[[8000., 600.]],
                points_labels=['(x=8000, y=600)'],
                ref_file="DATA/SL_downstream.txt",
                ref_label='analytic $t_\\infty$',
                fig_name="img/SL_downstream_{}".format(lab),
                y_label='z (m)',
                x_label='time (s)')

        vnv_plot1d_history(\
            'VELOCITY U',
            res_list,
            legend_labels=res_labels,
            fig_size=(6, 4),
            points=[[8000., 600.]],
            points_labels=['(x=8000, y=600)'],
            fig_name="img/U_downstream",
            fig_title="Point P2",
            y_label='U (m/s)',
            x_label='time (s)')

        #----------------------------------------------------------------------
        # Accuracy of free surface and velocity at the reservoir:
        vnv_plot1d_history(\
            'FREE SURFACE',
            res_list,
            legend_labels=res_labels,
            fig_size=(6, 4),
            points=[[3600., 600.]],
            points_labels=['(x=3600, y=600)'],
            ref_file="DATA/SL_reservoir.txt",
            ref_label='analytic $t_\\infty$',
            fig_name="img/SL_reservoir",
            fig_title="Point P1",
            y_label='z (m)',
            x_label='time (s)')

        vnv_plot1d_history(\
            'VELOCITY U',
            res_list,
            legend_labels=res_labels,
            fig_size=(6, 4),
            points=[[3600., 600.]],
            points_labels=['(x=3600, y=600)'],
            fig_name="img/U_reservoir",
            fig_title="Point P1",
            y_label='U (m/s)',
            x_label='time (s)')

        #----------------------------------------------------------------------
        # Energy balance:
        times_list = []
        ec_list = [] # kinetic energy
        ep_list = [] # potential energy
        em_list = [] # total energy

        for name, study in self.studies.items():
            if 'seq' in name:
                seq_energy_file = self.get_study_file(name+':T2DRF1')
                energy = np.genfromtxt(seq_energy_file)
                times_list.append(energy[:, 0])
                ec_list.append(energy[:, 1])
                ep_list.append(energy[:, 2])
                em_list.append(energy[:, 3])

        vnv_plot1d(\
           times_list,
           ec_list,
           res_labels,
           fig_size=(8, 3),
           x_label='time (s)',
           y_label='$E_c$',
           fig_name='img/KineticETime')

        vnv_plot1d(\
           times_list,
           ep_list,
           res_labels,
           fig_size=(8, 3),
           x_label='time (s)',
           y_label='$E_p$',
           fig_name='img/PotentialETime')

        vnv_plot1d(\
           times_list,
           em_list,
           res_labels,
           fig_size=(8, 3),
           x_label='time (s)',
           y_label='$E_m$',
           fig_name='img/MechanicalETime')

        #----------------------------------------------------------------------
        # Mass balance:
        times_list = []
        mass_loss_list = []

        for name, study in self.studies.items():
            if 'seq' in name:
                seq_mass_file = self.get_study_file(name+':T2DRF2')
                mass = np.genfromtxt(seq_mass_file)
                times_list.append(mass[:, 0])
                mass_loss_list.append(mass[:, 2])

        vnv_plot1d(\
           times_list,
           mass_loss_list,
           res_labels,
           x_label='time (s)',
           y_label='$M-M_0$',
           fig_name='img/Mass_balance')

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()
        geom_res.close()
        nerd_res.close()
        kin1_res.close()

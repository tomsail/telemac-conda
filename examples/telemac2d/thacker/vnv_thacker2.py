"""
Validation script for thacker (radially symmetrical paraboloid)
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for verification of thacker (radially symmetrical paraboloid)

    """
    def _init(self):
        """
        Defining general parameters
        """
        self.rank = 2
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # kin1 run
        cas = TelemacCas('t2d_thacker2-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d', 't2d_thacker2-fv.cas')
        # kin1 run in parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_thacker2-fv_par.cas', cas=cas)
        del cas

        #======================================================================
        # hllc1 run
        cas = TelemacCas('t2d_thacker2-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc1_seq', 'telemac2d', 't2d_thacker2-fv.cas')
        # hllc1 run in parallel
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc1_par', 'telemac2d', 't2d_thacker2-fv_par.cas', cas=cas)
        del cas

        #======================================================================
        # charac run
        self.add_study('char_seq', 'telemac2d', 't2d_thacker2-charac.cas')
        # charac run in parallel
        cas = TelemacCas('t2d_thacker2-charac.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', 't2d_thacker2-charac_par.cas', cas=cas)
        del cas

        #======================================================================
        # Nerd run
        self.add_study('nerd_seq', 'telemac2d', 't2d_thacker2-nerd.cas')
        # Nerd run in parallel
        cas = TelemacCas('t2d_thacker2-nerd.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', 't2d_thacker2-nerd_par.cas', cas=cas)
        del cas

        #======================================================================
        # eria run
        self.add_study('eria_seq', 'telemac2d', 't2d_thacker2-eria.cas')
        # eria run in parallel
        cas = TelemacCas('t2d_thacker2-eria.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', 't2d_thacker2-eria_par.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('nerd_seq:T2DRES', 'f2d_thacker2.slf',
                            eps=[1e-15])

        # sequential parallel comparison
        self.check_epsilons('char_seq:T2DRES', 'char_par:T2DRES',
                            eps=[1e-15])
        self.check_epsilons('nerd_seq:T2DRES', 'nerd_par:T2DRES',
                            eps=[1e-15])
        self.check_epsilons('eria_seq:T2DRES', 'eria_par:T2DRES',
                            eps=[1e-8])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_par:T2DRES',
                            eps=[0.097, 0.098, 3e-4, 3e-4, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])
        self.check_epsilons('hllc1_seq:T2DRES', 'hllc1_par:T2DRES',
                            eps=[0.032, 5e-3, 1e-5, 1e-5, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        # verification of epsilon with analytic solution
        for name, study in self.studies.items():
            self.check_epsilons(name+':T2DRES', name+':T2DRES',
                                var1='WATER DEPTH', var2='ANALYTIC SOL H',
                                eps=[0.043])
            self.check_epsilons(name+':T2DRES', name+':T2DRES',
                                var1='VELOCITY U', var2='ANALYTIC SOL U',
                                eps=[0.82])
            self.check_epsilons(name+':T2DRES', name+':T2DRES',
                                var1='VELOCITY V', var2='ANALYTIC SOL V',
                                eps=[0.29])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d,\
                vnv_plot1d_polylines, vnv_plot1d_history, \
                vnv_plotbar_cpu_times, vnv_plotbar
        from os import path
        import numpy as np
        #======================================================================
        # GENERAL PARAMETERS (used for adim):
        RHO = 1000.
        G = 9.81
        H0 = 0.1
        A = 1.0
        R0 = 0.8
        ETA = 0.5
        OMEGA = np.sqrt(8*G*H0)/A
        T = 2.*np.pi/OMEGA
        U0 = 1.

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        geom_res, _ = self.get_study_res('nerd_seq:T2DGEO', load_bnd=True)
        nerd_res, _ = self.get_study_res('nerd_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # figures limits
        xlim = [0.5/A, 3.5/A]
        ylim = [0.5/A, 3.5/A]

        # Plot 2d mesh and boundaries:
        vnv_plot2d(\
            'BOTTOM',
            geom_res,
            record=0,
            fig_size=(6, 6),
            x_factor=1./A, y_factor=1./A,
            x_label='x/a', y_label='y/a',
            fig_name="img/t2d_thacker2_mesh",
            annotate_bnd=True,
            plot_mesh=True)

        # Plot initial condition in slice plane:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            nerd_res,
            'initial elevation',
            fig_size=(6, 5),
            record=0,
            xlim=xlim, ylim=[-1., 2.],
            y_factor=1./H0, x_factor=1./A,
            x_label='x/a', y_label='z/H0',
            fig_name='img/t2d_thacker2_initial_elevation',
            plot_bottom=True)

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
            self.action_time,
            fig_size=(7, 3),
            fig_name='img/t2d_thacker2_cpu_times')

        #----------------------------------------------------------------------
        # Accuracy of free surface (1D slice):
        records = [i for i in range(10)]
        for idx, record in enumerate(records):
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                res_labels,
                record=record,
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL E',
                xlim=xlim, ylim=[-1., 1.5],
                y_factor=1./H0,
                x_factor=1./A,
                y_label='z/H0',
                x_label='x/a',
                fig_name='img/t2d_thacker2_schemes_comparison_{}'.format(record),
                plot_bottom=True)

        #----------------------------------------------------------------------
        # Accuracy, 2D plots at t=T:
        for idx, res in enumerate(res_list):
            # Accuracy of water depth (2D):
            fig_name = "img/t2d_thacker2_{}_seq_depth_9".format(res_labels[idx].lower())

            vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=9,
                fig_size=(6, 5),
                fig_name=fig_name,
                var_factor=1./H0,
                cbar_label='H/H0',
                cbar_extend='both',
                xlim=xlim, ylim=ylim,
                vmin=0.0, vmax=1.25, nv=11,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=res_labels[idx],
                contours=True,
                filled_contours=True,
                mask_tidal_flats=True,
                bathy_contours=True)

            # Accuracy of free surface (2D):
            fig_name = "img/t2d_thacker2_{}_seq_elevation_9".format(res_labels[idx].lower())

            vnv_plot2d(\
                'FREE SURFACE',
                res,
                record=9,
                fig_size=(6, 4.7),
                fig_name=fig_name,
                var_factor=1./H0,
                cbar_label='Elevation/H0',
                xlim=xlim, ylim=ylim,
                vmin=-0.2, vmax=0.25, nv=11,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=res_labels[idx],
                contours=True,
                filled_contours=True,
                mask_tidal_flats=True,
                bathy_contours=True)

            # Accuracy of velocity (2D):
            fig_name = "img/t2d_thacker2_{}_seq_velocity_9".format(res_labels[idx].lower())

            vnv_plot2d(\
                'VELOCITY',
                res,
                record=9,
                fig_size=(6, 5),
                fig_name=fig_name,
                var_factor=1./U0, cbar_label='U/U0',
                xlim=xlim, ylim=ylim,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                cbar_extend='both',
                fig_title=res_labels[idx],
                filled_contours=True,
                mask_tidal_flats=True,
                vectors=True,
                bathy_contours=True)

        #----------------------------------------------------------------------
        # Plot timeseries on points:
        vnv_plot1d_history(\
            'WATER DEPTH',
            res_list[1],
            res_labels[1],
            fig_size=(10, 4),
            points=[[2., 2.], [2.5, 2.], [3., 2.]],
            points_labels=['(x=2, y=2)', '(x=2.5, y=2)', '(x=3, y=2)'],
            y_factor=1./H0, x_factor=1./T,
            y_label='H/H0', x_label='t/T',
            fig_name='img/t2d_thacker2_depth_nerd_timeseries',
            markers=True)

        vnv_plot1d_history(\
            'WATER DEPTH',
            res_list,
            res_labels,
            fig_size=(10, 4),
            points=[[2.5, 2.]],
            points_labels=['(x=2.5, y=0)'],
            ref_name='ANALYTIC SOL H',
            ref_label='analytic',
            y_factor=1./H0, x_factor=1./T,
            y_label='H/H0', x_label='t/T',
            fig_name='img/t2d_thacker2_depth_timeseries',
            markers=True)

        #----------------------------------------------------------------------
        # Energy balance:
        times_list = []
        ec_list = [] # kinetic energy
        ep_list = [] # potential energy
        em_list = [] # total energy

        for name, study in self.studies.items():
            if 'seq' in name:
                nerd_seq_energy_file = path.join(
                    self.get_vnv_working_dir(name), 'energy_balance.txt')
                energy = np.genfromtxt(nerd_seq_energy_file)
                times_list.append(energy[:, 0])
                ec_list.append(energy[:, 1])
                ep_list.append(energy[:, 2])
                em_list.append(energy[:, 3])

        vnv_plot1d(\
            times_list,
            ec_list,
            res_labels,
            y_factor=1./(RHO*U0**2*H0**3),
            x_factor=1./T,
            x_label='t/T',
            y_label='$E_c/(\\rho U_0^2 H_0^3$)',
            fig_name='img/t2d_thacker2_kinetic_energy')

        vnv_plot1d(\
            times_list,
            ep_list,
            res_labels,
            y_factor=1./(RHO*U0**2*H0**3),
            x_factor=1./T,
            x_label='t/T',
            y_label='$E_p/(\\rho U_0^2 H_0^3$)',
            fig_name='img/t2d_thacker2_potential_energy')

        vnv_plot1d(\
            times_list,
            em_list,
            res_labels,
            y_factor=1./(RHO*U0**2*H0**3),
            x_factor=1./T,
            x_label='t/T',
            y_label='$E_m/(\\rho U_0^2 H_0^3$)',
            fig_name='img/t2d_thacker2_total_energy')

        #----------------------------------------------------------------------
        # Mass balance:
        times_list = []
        mass_loss_list = []

        for name, study in self.studies.items():
            if 'seq' in name:
                nerd_seq_energy_file = path.join(\
                    self.get_vnv_working_dir(name), 'mass_balance.txt')
                energy = np.genfromtxt(nerd_seq_energy_file)
                times_list.append(energy[:, 0])
                mass_loss_list.append(energy[:, 2])

        vnv_plot1d(\
            times_list,
            mass_loss_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='$M-M_0$',
            fig_name='img/t2d_thacker2_mass_balance')

        #----------------------------------------------------------------------
        # Error timeseries:
        #
        times_list = []

        errLinf_H_list = [] # error Linf on H (list of all cases)
        errLinf_U_list = [] # error Linf on U (list of all cases)
        errLinf_V_list = [] # error Linf on V (list of all cases)

        errL1_H_list = [] # error L1 on H (list of all cases)
        errL1_U_list = [] # error L1 on U (list of all cases)
        errL1_V_list = [] # error L1 on V (list of all cases)

        errL2_H_list = [] # error L2 on H (list of all cases)
        errL2_U_list = [] # error L2 on U (list of all cases)
        errL2_V_list = [] # error L2 on V (list of all cases)

        # Get error timeseries from file:
        for name, study in self.studies.items():
            if 'seq' in name:
                errorLinf_file = path.join(
                    self.get_vnv_working_dir(name), 'error_Linf.txt')
                errorL1_file = path.join(
                    self.get_vnv_working_dir(name), 'error_L1.txt')
                errorL2_file = path.join(
                    self.get_vnv_working_dir(name), 'error_L2.txt')

                erri = np.genfromtxt(errorLinf_file)
                times_list.append(erri[:, 0])
                errLinf_H_list.append(erri[:, 1])
                errLinf_U_list.append(erri[:, 2])
                errLinf_V_list.append(erri[:, 3])

                err1 = np.genfromtxt(errorL1_file)
                errL1_H_list.append(err1[:, 1])
                errL1_U_list.append(err1[:, 2])
                errL1_V_list.append(err1[:, 3])

                err2 = np.genfromtxt(errorL2_file)
                errL2_H_list.append(err2[:, 1])
                errL2_U_list.append(err2[:, 2])
                errL2_V_list.append(err2[:, 3])

        # Plot error Linf:
        vnv_plot1d(\
            times_list,
            errLinf_H_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_{\\infty}$',
            fig_name='img/t2d_thacker2_error_Linf_H')

        vnv_plot1d(\
            times_list,
            errLinf_U_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_{\\infty}$',
            fig_name='img/t2d_thacker2_error_Linf_U')

        vnv_plot1d(\
            times_list,
            errLinf_V_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_{\\infty}$',
            fig_name='img/t2d_thacker2_error_Linf_V')

        # Error L1 timeseries from file:
        vnv_plot1d(\
            times_list,
            errL1_H_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_1$',
            fig_name='img/t2d_thacker2_error_L1_H')

        vnv_plot1d(\
            times_list,
            errL1_U_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_1$',
            fig_name='img/t2d_thacker2_error_L1_U')

        vnv_plot1d(\
            times_list,
            errL1_V_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_1$',
            fig_name='img/t2d_thacker2_error_L1_V')

        # Error L2 timeseries from file:
        vnv_plot1d(\
            times_list,
            errL2_H_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_2$',
            fig_name='img/t2d_thacker2_error_L2_H')

        vnv_plot1d(\
            times_list,
            errL2_U_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_2$',
            fig_name='img/t2d_thacker2_error_L2_U')

        vnv_plot1d(\
            times_list,
            errL2_V_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_2$',
            fig_name='img/t2d_thacker2_error_L2_V')

        #----------------------------------------------------------------------
        # Error at final time (computed from mass matrix file):
        #
        # Compute errors at final time for each case:
        errLinf_Htf_list = [] # error Linf on H at tf
        errLinf_Utf_list = [] # error Linf on U at tf
        errLinf_Vtf_list = [] # error Linf on V at tf

        errL1_Htf_list = [] # error L1 on H at tf
        errL1_Utf_list = [] # error L1 on U at tf
        errL1_Vtf_list = [] # error L1 on V at tf

        errL2_Htf_list = [] # error L2 on H at tf
        errL2_Utf_list = [] # error L2 on U at tf
        errL2_Vtf_list = [] # error L2 on V at tf

        idx = 0
        for name, study in self.studies.items():
            if 'seq' in name:
                # Mass matrix at final time
                massm_file = path.join(
                    self.get_vnv_working_dir(name), 'mass_matrix_tf.txt')
                massm = np.genfromtxt(massm_file)

                # Linf errors:
                errLinf_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='ANALYTIC SOL H',
                    record=-1,
                    norm='linf'))

                errLinf_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='ANALYTIC SOL U',
                    record=-1,
                    norm='linf'))

                errLinf_Vtf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY V',
                    var2='ANALYTIC SOL V',
                    record=-1,
                    norm='linf'))

                # L1 errors:
                errL1_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='ANALYTIC SOL H',
                    record=-1,
                    norm='l1',
                    mass=massm))

                errL1_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='ANALYTIC SOL U',
                    record=-1,
                    norm='l1',
                    mass=massm))

                errL1_Vtf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY V',
                    var2='ANALYTIC SOL V',
                    record=-1,
                    norm='l1',
                    mass=massm))

                # L2 errors:
                errL2_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='ANALYTIC SOL H',
                    record=-1,
                    norm='l2',
                    mass=massm))

                errL2_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='ANALYTIC SOL U',
                    record=-1,
                    norm='l2',
                    mass=massm))

                errL2_Vtf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY V',
                    var2='ANALYTIC SOL V',
                    record=-1,
                    norm='l2',
                    mass=massm))

                idx += 1

        # Bar plots of errors at final time
        errors_H_tf = [errLinf_Htf_list, errL1_Htf_list, errL2_Htf_list]
        errors_U_tf = [errLinf_Utf_list, errL1_Utf_list, errL2_Utf_list]
        errors_V_tf = [errLinf_Vtf_list, errL1_Vtf_list, errL2_Vtf_list]

        vnv_plotbar(\
            errors_H_tf,
            fig_size=(8, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on H at $t=t_f$',
            y_scale='log',
            fig_name="img/t2d_thacker2_errors_H_tf")

        vnv_plotbar(\
            errors_U_tf,
            fig_size=(8, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U at $t=t_f$',
            y_scale='log',
            fig_name="img/t2d_thacker2_errors_U_tf")

        vnv_plotbar(\
            errors_V_tf,
            fig_size=(8, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on V at $t=t_f$',
            y_scale='log',
            fig_name="img/t2d_thacker2_errors_V_tf")

        #======================================================================
        # Delete results
        geom_res.close()
        nerd_res.close()
        for res in res_list:
            res.close()

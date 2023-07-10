"""
Validation script for thacker (planar surface in a paraboloid)
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for verification of thacker (planar surface in a paraboloid)

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
        # KIN1 run
        cas = TelemacCas('t2d_thacker-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d', 't2d_thacker-fv.cas', cas=cas)
        # KIN1 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_thacker-fv_par.cas', cas=cas)
        del cas

        #======================================================================
        # HLLC1 run
        cas = TelemacCas('t2d_thacker-fv.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc1_seq', 'telemac2d', 't2d_thacker-fv.cas', cas=cas)
        # HLLC1 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc1_par', 'telemac2d', 't2d_thacker-fv_par.cas', cas=cas)
        del cas

        #======================================================================
        # charac run
        self.add_study('char_seq', 'telemac2d', 't2d_thacker-charac.cas')
        # charac run in parallel
        cas = TelemacCas('t2d_thacker-charac.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', 't2d_thacker-charac_par.cas', cas=cas)
        del cas

        #======================================================================
        # nerd run
        self.add_study('nerd_seq', 'telemac2d', 't2d_thacker-nerd.cas')
        # Nerd run in parallel
        cas = TelemacCas('t2d_thacker-nerd.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', 't2d_thacker-nerd_par.cas', cas=cas)
        del cas

        #======================================================================
        # eria run
        self.add_study('eria_seq', 'telemac2d', 't2d_thacker-eria.cas')
        # eria run in parallel
        cas = TelemacCas('t2d_thacker-eria.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', 't2d_thacker-eria_par.cas', cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('kin1_seq:T2DRES', 'f2d_thacker-kin1.slf', eps=[1e-8])
        self.check_epsilons('kin1_par:T2DRES', 'f2d_thacker-kin1.slf', eps=[1e-8])
        self.check_epsilons('kin1_seq:T2DRES' , 'kin1_par:T2DRES', eps=[1e-8])

        self.check_epsilons('hllc1_seq:T2DRES', 'f2d_thacker-hllc1.slf', eps=[1e-8])
        self.check_epsilons('hllc1_par:T2DRES', 'f2d_thacker-hllc1.slf',
                            eps=[2e-3, 3e-3, 1e-5, 1e-5, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])
        self.check_epsilons('hllc1_seq:T2DRES', 'hllc1_par:T2DRES',
                            eps=[2e-3, 3e-3, 1e-5, 1e-5, 1e-8, 1e-8, 1e-8, 1e-8, 1e-8])

        self.check_epsilons('char_seq:T2DRES' , 'char_par:T2DRES',
                            eps=[1e-15])
        self.check_epsilons('nerd_seq:T2DRES' , 'nerd_par:T2DRES',
                            eps=[1e-14])
        self.check_epsilons('nerd_seq:T2DRES', 'f2d_thacker.slf', eps=[1e-15])
        self.check_epsilons('eria_seq:T2DRES' , 'eria_par:T2DRES',
                            eps=[1e-10])

        # verification of epsilon with analytic solution
        for name, study in self.studies.items():
            self.check_epsilons(name+':T2DRES', name+':T2DRES',
                                var1='WATER DEPTH', var2='ANALYTIC SOL H',
                                eps=[0.054])
            self.check_epsilons(name+':T2DRES', name+':T2DRES',
                                var1='VELOCITY U', var2='ANALYTIC SOL U',
                                eps=[1.4])
            self.check_epsilons(name+':T2DRES', name+':T2DRES',
                                var1='VELOCITY V', var2='ANALYTIC SOL V',
                                eps=[0.53])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot3d, vnv_plot2d, vnv_plot1d, vnv_plotbar, \
                vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from os import path
        import numpy as np
        #======================================================================
        # GENERAL PARAMETERS (used for adim):
        #
        RHO = 1000.
        G = 9.81
        H0 = 0.1
        A = 1.0
        ETA = 0.5
        OMEGA = np.sqrt(2*G*H0)/A
        T = 2.*np.pi/OMEGA
        U0 = ETA*OMEGA

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        geom_res, _ = self.get_study_res('nerd_seq:T2DGEO', load_bnd=True)
        nerd_res, _ = self.get_study_res('nerd_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plot 2d mesh and boundaries:
        vnv_plot2d(\
            'BOTTOM',
            geom_res,
            record=0,
            fig_size=(6, 6),
            x_factor=1./A, y_factor=1./A,
            x_label='x/a', y_label='y/a',
            fig_name="img/t2d_thacker_mesh",
            annotate_bnd=True,
            plot_mesh=True)

        # Plot 3d bathymetry:
        vnv_plot3d(\
            'BOTTOM',
            nerd_res,
            record=0,
            fig_size=(8, 6),
            fig_name="img/t2d_thacker_bathy")

        # Plot initial condition in slice plane:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            nerd_res,
            'initial elevation',
            fig_size=(6, 5),
            record=0,
            y_factor=1./H0, x_factor=1./A,
            x_label='x/a', y_label='z/H0',
            fig_name='img/t2d_thacker_initial_elevation',
            plot_bottom=True)

        #======================================================================
        # FIRST OBSERVATION OF NERD RESULTS:
        #
        # figures limits
        xlim = [0.5/A, 3.5/A]
        ylim = [0.5/A, 3.5/A]

        # One record every 0.5s i.e. record=9 ~ t=T
        records = [0, 1, 3, 6, 9]
        time_labels = ['0', 'T/9', 'T/3', '2T/3', 'T']

        for idx, record in enumerate(records):
            time_label = 't='+ time_labels[idx]

            # Plot water depth at different times.
            vnv_plot2d(\
                'WATER DEPTH',
                nerd_res,
                record=record,
                fig_size=(6, 5),
                fig_name="img/t2d_thacker_nerd_seq_depth_firstobs{}".format(record),
                var_factor=1./H0,
                cbar_label='H/H0',
                xlim=xlim, ylim=ylim,
                vmin=0.01, vmax=1., nv=11,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=time_label,
                contours=True,
                filled_contours=True,
                bathy_contours=True)

            vnv_plot2d(\
                'WATER DEPTH',
                nerd_res,
                record=record,
                fig_size=(6, 5),
                fig_name="img/t2d_thacker_nerd_seq_depth_dry_firstobs{}".format(record),
                var_factor=1./H0,
                cbar_label='H/H0',
                xlim=xlim, ylim=ylim,
                vmin=0.01, vmax=1., nv=11,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=time_label,
                contours=True,
                filled_contours=True,
                mask_tidal_flats=True,
                bathy_contours=True)

            if idx > 0:
                # Plot velocity at different times.
                vnv_plot2d(\
                    'VELOCITY',
                    nerd_res,
                    record=record,
                    fig_size=(6, 5),
                    fig_name="img/t2d_thacker_nerd_seq_vel_firstobs{}".format(record),
                    var_factor=1./U0,
                    cbar_label='|U|/U0',
                    xlim=xlim, ylim=ylim,
                    vmin=0.75, vmax=1.25, nv=11,
                    x_factor=1./A, y_factor=1./A,
                    x_label='x/a', y_label='y/a',
                    fig_title=time_label,
                    contours=True,
                    filled_contours=True,
                    vectors=True,
                    vectors_scale=20,
                    bathy_contours=True)

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
            self.action_time,
            fig_size=(7, 3),
            fig_name='img/t2d_thacker_cpu_times')

        #----------------------------------------------------------------------
        # Accuracy of free surface (1D slice):
        for idx, record in enumerate(records):
            time_label = 't='+ time_labels[idx]
            vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_list,
                res_labels,
                record=record,
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL E',
                y_factor=1./H0,
                x_factor=1./A,
                y_label='z/H0',
                x_label='x/a',
                fig_name='img/t2d_thacker_schemes_comparison_{}'.format(record),
                fig_title=time_label,
                plot_bottom=True)

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
            fig_name="img/t2d_thacker_minT",
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
            fig_name="img/t2d_thacker_maxT",
            bar_width=.5,
            annotate=True)

        #----------------------------------------------------------------------
        # Accuracy, 2D plots at t=T:
        for idx, res in enumerate(res_list):
            # Accuracy of water depth (2D):
            fig_name = "img/t2d_thacker_{}_seq_depth_9".format(res_labels[idx].lower())

            vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=9,
                fig_size=(6, 5),
                fig_name=fig_name,
                var_factor=1./H0,
                cbar_label='H/H0',
                xlim=xlim, ylim=ylim,
                vmin=0.0, vmax=1., nv=11,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=res_labels[idx],
                cbar_extend='both',
                contours=True,
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.001,
                bathy_contours=True)

            # Accuracy of free surface (2D):
            fig_name = "img/t2d_thacker_{}_seq_elevation_9".format(res_labels[idx].lower())

            vnv_plot2d(\
                'FREE SURFACE',
                res,
                record=9,
                fig_size=(6, 4.7),
                fig_name=fig_name,
                var_factor=1./H0,
                cbar_label='Elevation/H0',
                xlim=xlim, ylim=ylim,
                vmin=-0.75, vmax=1.25, nv=9,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=res_labels[idx],
                cbar_extend='both',
                contours=True,
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.001,
                bathy_contours=True)

            # Accuracy of velocity (2D):
            fig_name = "img/t2d_thacker_{}_seq_velocity_9".format(res_labels[idx].lower())

            vnv_plot2d(\
                'VELOCITY',
                res,
                record=9,
                fig_size=(6, 5),
                fig_name=fig_name,
                var_factor=1./U0, cbar_label='|U|/U0',
                xlim=xlim, ylim=ylim,
                vmin=0.75, vmax=1.25, nv=11,
                x_factor=1./A, y_factor=1./A,
                x_label='x/a', y_label='y/a',
                fig_title=res_labels[idx],
                cbar_extend='both',
                filled_contours=True,
                mask_tidal_flats=True,
                tidal_flats_threshold=0.001,
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
            fig_name='img/t2d_thacker_depth_nerd_timeseries',
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
            fig_name='img/t2d_thacker_depth_timeseries',
            markers=True)

        #----------------------------------------------------------------------
        # Energy balance:
        times_list = []
        ec_list = [] # kinetic energy
        ep_list = [] # potential energy
        em_list = [] # total energy

        for name, study in self.studies.items():
            if 'seq' in name:
                energy_file = self.get_study_file(name+':T2DRF1')
                energy = np.genfromtxt(energy_file)
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
            fig_name='img/t2d_thacker_kinetic_energy')

        vnv_plot1d(\
            times_list,
            ep_list,
            res_labels,
            y_factor=1./(RHO*U0**2*H0**3),
            x_factor=1./T,
            x_label='t/T',
            y_label='$E_p/(\\rho U_0^2 H_0^3$)',
            fig_name='img/t2d_thacker_potential_energy')

        vnv_plot1d(\
            times_list,
            em_list,
            res_labels,
            y_factor=1./(RHO*U0**2*H0**3),
            x_factor=1./T,
            x_label='t/T',
            y_label='$E_m/(\\rho U_0^2 H_0^3$)',
            fig_name='img/t2d_thacker_total_energy')

        #----------------------------------------------------------------------
        # Mass balance:
        times_list = []
        mass_loss_list = []

        for name, study in self.studies.items():
            if 'seq' in name:
                mass_file = self.get_study_file(name+':T2DRF2')
                mass = np.genfromtxt(mass_file)
                times_list.append(mass[:, 0])
                mass_loss_list.append(mass[:, 2])

        vnv_plot1d(\
            times_list,
            mass_loss_list,
            res_labels,
            x_factor=1./T,
            x_label='t/T',
            y_label='$M-M_0$',
            fig_name='img/t2d_thacker_mass_balance')

        #----------------------------------------------------------------------
        # Error timeseries and integrated in time:
        #
        times_list = []

        #errors time series:
        errLinf_H_list = []
        errLinf_U_list = []
        errLinf_V_list = []

        errL1_H_list = []
        errL1_U_list = []
        errL1_V_list = []

        errL2_H_list = []
        errL2_U_list = []
        errL2_V_list = []

        #time integrated errors:
        errLinf_H = []
        errLinf_U = []
        errLinf_V = []

        errL1_H = []
        errL1_U = []
        errL1_V = []

        errL2_H = []
        errL2_U = []
        errL2_V = []

        # Get error timeseries from file:
        for name, study in self.studies.items():
            if 'seq' in name:
                errorLinf_file = self.get_study_file(name+':T2DRF4')
                errorL1_file = self.get_study_file(name+':T2DRF5')
                errorL2_file = self.get_study_file(name+':T2DRF6')

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

                # compute errors integrals:
                times = erri[:, 0]
                time_incrs = np.zeros(len(times))
                for i in range(len(times)):
                    if i == 0:
                        time_incrs[i] = times[i]
                    else:
                        time_incrs[i] = times[i] - times[i-1]

                errLinf_H.append(np.sum(erri[:, 1]*time_incrs[:])/times[-1])
                errLinf_U.append(np.sum(erri[:, 2]*time_incrs[:])/times[-1])
                errLinf_V.append(np.sum(erri[:, 3]*time_incrs[:])/times[-1])

                errL1_H.append(np.sum(err1[:, 1]*time_incrs[:])/times[-1])
                errL1_U.append(np.sum(err1[:, 2]*time_incrs[:])/times[-1])
                errL1_V.append(np.sum(err1[:, 3]*time_incrs[:])/times[-1])

                errL2_H.append(np.sum(err2[:, 1]*time_incrs[:])/times[-1])
                errL2_U.append(np.sum(err2[:, 2]*time_incrs[:])/times[-1])
                errL2_V.append(np.sum(err2[:, 3]*time_incrs[:])/times[-1])

        # Plot error Linf:
        size = (5.5, 4)
        vnv_plot1d(\
            times_list,
            errLinf_H_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_{\\infty}$ on H',
            fig_name='img/t2d_thacker_error_Linf_H')

        vnv_plot1d(\
            times_list,
            errLinf_U_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_{\\infty}$ on U',
            fig_name='img/t2d_thacker_error_Linf_U')

        vnv_plot1d(\
            times_list,
            errLinf_V_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_{\\infty}$ on V',
            fig_name='img/t2d_thacker_error_Linf_V')

        # Error L1 timeseries from file:
        vnv_plot1d(\
            times_list,
            errL1_H_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_1$ on H',
            fig_name='img/t2d_thacker_error_L1_H')

        vnv_plot1d(\
            times_list,
            errL1_U_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_1$ on U',
            fig_name='img/t2d_thacker_error_L1_U')

        vnv_plot1d(\
            times_list,
            errL1_V_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_1$ on V',
            fig_name='img/t2d_thacker_error_L1_V')

        # Error L2 timeseries from file:
        vnv_plot1d(\
            times_list,
            errL2_H_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_2$ on H',
            fig_name='img/t2d_thacker_error_L2_H')

        vnv_plot1d(\
            times_list,
            errL2_U_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_2$ on U',
            fig_name='img/t2d_thacker_error_L2_U')

        vnv_plot1d(\
            times_list,
            errL2_V_list,
            res_labels,
            fig_size=size,
            x_factor=1./T,
            x_label='t/T',
            y_label='Error $L_2$ on V',
            fig_name='img/t2d_thacker_error_L2_V')

        # Bar plots of time integrated errors
        errors_H_tf = [errLinf_H, errL1_H, errL2_H]
        errors_U_tf = [errLinf_U, errL1_U, errL2_U]
        errors_V_tf = [errLinf_V, errL1_V, errL2_V]

        size = (6.5, 4)
        vnv_plotbar(\
            errors_H_tf,
            fig_size=size,
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on H integrated in time',
            y_scale='log',
            fig_name="img/t2d_thacker_errors_timeintegral_H",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=size,
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U integrated in time',
            y_scale='log',
            fig_name="img/t2d_thacker_errors_timeintegral_U",
            annotate=True)

        vnv_plotbar(\
            errors_V_tf,
            fig_size=size,
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on V integrated in time',
            y_scale='log',
            fig_name="img/t2d_thacker_errors_timeintegral_V",
            annotate=True)

        #----------------------------------------------------------------------
        # Error at t=T (computed from mass matrix file):
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
                massm_file = self.get_study_file(name+':T2DRF3')
                massm = np.genfromtxt(massm_file)

                # Linf errors:
                errLinf_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='ANALYTIC SOL H',
                    record=9,
                    norm='linf'))

                errLinf_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='ANALYTIC SOL U',
                    record=9,
                    norm='linf'))

                errLinf_Vtf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY V',
                    var2='ANALYTIC SOL V',
                    record=9,
                    norm='linf'))

                # L1 errors:
                errL1_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='ANALYTIC SOL H',
                    record=9,
                    norm='l1',
                    mass=massm))

                errL1_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='ANALYTIC SOL U',
                    record=9,
                    norm='l1',
                    mass=massm))

                errL1_Vtf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY V',
                    var2='ANALYTIC SOL V',
                    record=9,
                    norm='l1',
                    mass=massm))

                # L2 errors:
                errL2_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='ANALYTIC SOL H',
                    record=9,
                    norm='l2',
                    mass=massm))

                errL2_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='ANALYTIC SOL U',
                    record=9,
                    norm='l2',
                    mass=massm))

                errL2_Vtf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY V',
                    var2='ANALYTIC SOL V',
                    record=9,
                    norm='l2',
                    mass=massm))

                idx += 1

        errors_H_tf = [errLinf_Htf_list, errL1_Htf_list, errL2_Htf_list]
        errors_U_tf = [errLinf_Utf_list, errL1_Utf_list, errL2_Utf_list]
        errors_V_tf = [errLinf_Vtf_list, errL1_Vtf_list, errL2_Vtf_list]

        # Bar plots of errors at final time
        vnv_plotbar(\
            errors_H_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on H at $t=T$',
            y_scale='log',
            fig_name="img/t2d_thacker_errors_H_tf",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U at $t=T$',
            y_scale='log',
            fig_name="img/t2d_thacker_errors_U_tf",
            annotate=True)

        vnv_plotbar(\
            errors_V_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on V at $t=T$',
            y_scale='log',
            fig_name="img/t2d_thacker_errors_V_tf",
            annotate=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()
        geom_res.close()
        nerd_res.close()


"""
Validation script for stoker
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # charac run
        self.add_study('char_seq', 'telemac2d', 't2d_stoker-charac.cas')
        # charac run in parallel
        cas = TelemacCas('t2d_stoker-charac.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', 't2d_stoker-charac_par.cas', cas=cas)
        del cas

        #======================================================================
        # Nerd run
        self.add_study('nerd_seq', 'telemac2d', 't2d_stoker-nerd.cas')
        # Nerd run in parallel
        cas = TelemacCas('t2d_stoker-nerd.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', 't2d_stoker-nerd_par.cas', cas=cas)
        del cas

        #======================================================================
        # eria run
        self.add_study('eria_seq', 'telemac2d', 't2d_stoker-eria.cas')
        # eria run in parallel
        cas = TelemacCas('t2d_stoker-eria.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', 't2d_stoker-eria_par.cas', cas=cas)
        del cas

        #======================================================================
        # kin1 run
        self.add_study('kin1_seq', 'telemac2d', 't2d_stoker-kin1.cas')
        # kin1 run in parallel
        cas = TelemacCas('t2d_stoker-kin1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_stoker-kin1_par.cas', cas=cas)
        del cas

        #======================================================================
        # kin2 run
        self.add_study('kin2_seq', 'telemac2d', 't2d_stoker-kin2.cas')
        # kin1 run in parallel
        cas = TelemacCas('t2d_stoker-kin2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2_par', 'telemac2d', 't2d_stoker-kin2_par.cas', cas=cas)
        del cas

        #======================================================================
        # hllc run
        self.add_study('hllc_seq', 'telemac2d', 't2d_stoker-hllc.cas')
        # hllc run in parallel
        cas = TelemacCas('t2d_stoker-hllc.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc_par', 'telemac2d', 't2d_stoker-hllc_par.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # sequential parallel comparison
        self.check_epsilons('char_seq:T2DRES', 'f2d_stoker-char.slf', eps=[1e-8])
        self.check_epsilons('char_par:T2DRES', 'f2d_stoker-char.slf', eps=[1e-8])
        self.check_epsilons('char_seq:T2DRES', 'char_par:T2DRES', eps=[1.e-12])

        self.check_epsilons('nerd_seq:T2DRES', 'f2d_stoker-nerd.slf', eps=[1e-8])
        self.check_epsilons('nerd_par:T2DRES', 'f2d_stoker-nerd.slf', eps=[1e-8])
        self.check_epsilons('nerd_seq:T2DRES', 'nerd_par:T2DRES', eps=[1.e-12])

        self.check_epsilons('eria_seq:T2DRES', 'f2d_stoker-eria.slf', eps=[1e-8])
        self.check_epsilons('eria_par:T2DRES', 'f2d_stoker-eria.slf', eps=[1e-8])
        self.check_epsilons('eria_seq:T2DRES', 'eria_par:T2DRES', eps=[1.e-14])

        self.check_epsilons('kin1_seq:T2DRES', 'f2d_stoker-kin1.slf', eps=[1e-8])
        self.check_epsilons('kin1_par:T2DRES', 'f2d_stoker-kin1.slf', eps=[1e-8])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_par:T2DRES', eps=[1.e-14])

        self.check_epsilons('kin2_seq:T2DRES', 'f2d_stoker-kin2.slf', eps=[1.])
        self.check_epsilons('kin2_par:T2DRES', 'f2d_stoker-kin2.slf', eps=[1.])
        self.check_epsilons('kin2_seq:T2DRES', 'kin2_par:T2DRES', eps=[1.])

        self.check_epsilons('hllc_seq:T2DRES', 'f2d_stoker-hllc.slf', eps=[1e-8])
        self.check_epsilons('hllc_par:T2DRES', 'f2d_stoker-hllc.slf', eps=[1e-8])
        self.check_epsilons('hllc_seq:T2DRES', 'hllc_seq:T2DRES', eps=[1e-8])

        # verification of epsilon with analytic solution
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_seq:T2DRES',
                            var1='WATER DEPTH', var2='ANALYTIC SOL H',
                            eps=[1.])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_seq:T2DRES',
                            var1='VELOCITY U', var2='ANALYTIC SOL U',
                            eps=[2.])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_seq:T2DRES',
                            var1='VELOCITY V', var2='ANALYTIC SOL V',
                            eps=[2.])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d, vnv_plotbar, \
                vnv_plot1d_polylines, vnv_plot1d_history, vnv_plotbar_cpu_times
        from os import path
        import numpy as np

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        geom_res, _ = self.get_study_res('kin1_seq:T2DGEO', load_bnd=True)
        kin1_res, _ = self.get_study_res('kin1_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plot 2d mesh and boundaries:
        vnv_plot2d(\
            '',
            geom_res,
            record=0,
            fig_size=(10, 2),
            fig_name="img/t2d_stoker_mesh",
            annotate_bnd=True,
            plot_mesh=True)

        # Plot initial condition in slice plane:
        vnv_plot1d_polylines(\
            'WATER DEPTH',
            kin1_res,
            'initial water depth',
            fig_size=(5, 3),
            record=0,
            ylim=[0., 1.2],
            y_label='h (m)',
            fig_name='img/t2d_stoker_initial_elevation')

        #======================================================================
        # FIRST OBSERVATION OF HLLC RESULTS:
        #
        records = [0, 50, 100, 150, 200]

        for idx, record in enumerate(records):
            time_label = 't={:.2f}'.format(kin1_res.times[record])

            # Plot water depth at different times.
            vnv_plot1d_polylines(\
                'WATER DEPTH',
                kin1_res,
                'KIN1',
                record=record,
                y_label='h (m)',
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL H',
                fig_name='img/t2d_stoker_kin1_depth_firstobs{}'.format(record),
                fig_title=time_label)

            vnv_plot2d(\
                'WATER DEPTH',
                kin1_res,
                record=record,
                fig_size=(10, 3),
                fig_name="img/t2d_stoker_kin1_depth2d_firstobs{}".format(record),
                cbar_label='Water depth',
                vmin=0.2,
                vmax=1.,
                nv=11,
                fig_title=time_label,
                contours=True,
                filled_contours=True)

            # Plot velocity at different times.
            vnv_plot1d_polylines(\
                'VELOCITY U',
                kin1_res,
                'KIN1',
                record=record,
                y_label='u (m/s)',
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL U',
                fig_name='img/t2d_stoker_kin1_vel_firstobs{}'.format(record),
                fig_title=time_label)

            if idx > 0:
                vnv_plot2d(\
                    'VELOCITY',
                    kin1_res,
                    record=record,
                    fig_size=(10, 3),
                    fig_name="img/t2d_stoker_kin1_vel2d_firstobs{}".format(record),
                    fig_title=time_label,
                    cbar_label='Velocity norm',
                    vmin=0.,
                    vmax=2.,
                    nv=11,
                    contours=True,
                    filled_contours=True)

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
            self.action_time,
            fig_size=(7, 3),
            fig_name='img/t2d_stoker_cpu_times')

        #----------------------------------------------------------------------
        # Accuracy of free surface (1D slice):
        for idx, record in enumerate(records):
            time_label = 't={:.2f}'.format(kin1_res.times[record])

            vnv_plot1d_polylines(\
                'WATER DEPTH',
                res_list,
                res_labels,
                record=record,
                y_label='h (m)',
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL H',
                fig_name='img/t2d_stoker_H_schemes_comparison_{}'.format(record),
                fig_title=time_label)

            vnv_plot1d_polylines(\
                'VELOCITY U',
                res_list,
                res_labels,
                record=record,
                y_label='u (m/s)',
                fig_size=(6, 5),
                ref_name='ANALYTIC SOL U',
                fig_name='img/t2d_stoker_U_schemes_comparison_{}'.format(record),
                fig_title=time_label)

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
            fig_name="img/t2d_stoker_minT",
            bar_width=.5,
            annotate=True)

        vnv_plotbar(\
            [max_vars],
            fig_size=(10, 3),
            legend_labels=['max(T)'],
            x_labels=res_labels,
            y_scale='linear',
            fig_title='Maximum values of water depth at $t=T$',
            fig_name="img/t2d_stoker_maxT",
            bar_width=.5,
            annotate=True)

        #----------------------------------------------------------------------
        # Accuracy, 2D plots at t=T:
        for idx, res in enumerate(res_list):
            # Accuracy of water depth (2D):
            fig_name = "img/t2d_stoker_{}_depth_100".format(res_labels[idx].lower())

            vnv_plot2d(\
                'WATER DEPTH',
                res,
                record=50,
                fig_size=(10, 3),
                fig_name=fig_name,
                fig_title=res_labels[idx],
                contours=True,
                filled_contours=True)

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
            x_label='t',
            y_label='$E_c$',
            fig_name='img/t2d_stoker_kinetic_energy')

        vnv_plot1d(\
            times_list,
            ep_list,
            res_labels,
            x_label='t',
            y_label='$E_p$',
            fig_name='img/t2d_stoker_potential_energy')

        vnv_plot1d(\
            times_list,
            em_list,
            res_labels,
            x_label='t',
            y_label='$E_m$',
            fig_name='img/t2d_stoker_total_energy')

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
            x_label='t',
            y_label='$M-M_0$',
            fig_name='img/t2d_stoker_mass_balance')

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
            x_label='t',
            y_label='Error $L_{\\infty}$ on H',
            fig_name='img/t2d_stoker_error_Linf_H')

        vnv_plot1d(\
            times_list,
            errLinf_U_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_{\\infty}$ on U',
            fig_name='img/t2d_stoker_error_Linf_U')

        vnv_plot1d(\
            times_list,
            errLinf_V_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_{\\infty}$ on V',
            fig_name='img/t2d_stoker_error_Linf_V')

        # Error L1 timeseries from file:
        vnv_plot1d(\
            times_list,
            errL1_H_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_1$ on H',
            fig_name='img/t2d_stoker_error_L1_H')

        vnv_plot1d(\
            times_list,
            errL1_U_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_1$ on U',
            fig_name='img/t2d_stoker_error_L1_U')

        vnv_plot1d(\
            times_list,
            errL1_V_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_1$ on V',
            fig_name='img/t2d_stoker_error_L1_V')

        # Error L2 timeseries from file:
        vnv_plot1d(\
            times_list,
            errL2_H_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_2$ on H',
            fig_name='img/t2d_stoker_error_L2_H')

        vnv_plot1d(\
            times_list,
            errL2_U_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_2$ on U',
            fig_name='img/t2d_stoker_error_L2_U')

        vnv_plot1d(\
            times_list,
            errL2_V_list,
            res_labels,
            fig_size=size,
            x_label='t',
            y_label='Error $L_2$ on V',
            fig_name='img/t2d_stoker_error_L2_V')

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
            fig_name="img/t2d_stoker_errors_timeintegral_H",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=size,
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U integrated in time',
            y_scale='log',
            fig_name="img/t2d_stoker_errors_timeintegral_U",
            annotate=True)

        vnv_plotbar(\
            errors_V_tf,
            fig_size=size,
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on V integrated in time',
            y_scale='log',
            fig_name="img/t2d_stoker_errors_timeintegral_V",
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
            fig_name="img/t2d_stoker_errors_H_tf",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U at $t=T$',
            y_scale='log',
            fig_name="img/t2d_stoker_errors_U_tf",
            annotate=True)

        vnv_plotbar(\
            errors_V_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on V at $t=T$',
            y_scale='log',
            fig_name="img/t2d_stoker_errors_V_tf",
            annotate=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()
        geom_res.close()
        kin1_res.close()

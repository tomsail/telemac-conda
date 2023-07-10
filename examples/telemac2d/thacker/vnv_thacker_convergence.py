"""
Validation script for thacker (planar surface in a paraboloid)
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from pretel.stbtel_refine import run_refine
from config import CFGS

class VnvStudy(AbstractVnvStudy):
    """
    Class for verification of thacker (planar surface in a paraboloid)

    """
    def _init(self):
        """
        Defining general parameters
        """
        self.rank = 4
        self.tags = ['telemac2d','fv']
        self.refinement_levels = 4
        self.temporary_files = []

        # Duration:
        self.time_period = 4.4857
        self.time_coef = 2.
        self.duration = self.time_period/self.time_coef

        # Time discretization:
        self.variable_timestep = True
        self.timestep = 2.65e-3
        self.CFL = 0.9

        # Numerical schemes tested:
        self.treatment_of_the_linear_system = 2
        self.FV_schemes = ['KIN1', 'KIN2', 'HLLC1', 'HLLC2']
        self.FE_schemes = ['CHARAC', 'LEOP', 'NERD', 'ERIA']
        self.schemes = self.FV_schemes + self.FE_schemes

    def set_thacker_values(self, cas, scheme):
        if scheme == 'KIN1':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'KIN2':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
        elif scheme == 'HLLC1':
            cas.set('FINITE VOLUME SCHEME', 5)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'HLLC2':
            cas.set('FINITE VOLUME SCHEME', 5)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)


    def _pre(self):
        """
        Defining the studies
        """
        # get TELEMAC root directory:
        root_dir = CFGS.get_root()

        # Loop over schemes
        for ids, sc in enumerate(self.schemes):

            # geometry files for first case
            geo_file = "geo-thacker_0.slf"
            bnd_file = "geo-thacker_0.cli"

            # set base study:
            if sc in self.FE_schemes:
                cas = TelemacCas('t2d_thacker-{}.cas'.format(sc.lower()),\
                                 get_dico('telemac2d'))

            elif sc in self.FV_schemes:
                cas = TelemacCas('t2d_thacker-fv.cas'.format(sc),\
                                 get_dico('telemac2d'))
                self.set_thacker_values(cas, sc)

            cas.set('GEOMETRY FILE', geo_file)
            cas.set('BOUNDARY CONDITIONS FILE', bnd_file)
            cas.set('RESULTS FILE', "r2d-thacker_0.slf")
            cas.set('TREATMENT OF THE LINEAR SYSTEM',\
                self.treatment_of_the_linear_system)
            cas.set('TIME STEP', self.timestep)

            # timestep options
            if self.variable_timestep:
                cas.remove('NUMBER OF TIME STEPS')
                cas.set('DURATION', self.duration)
                cas.set('DESIRED COURANT NUMBER', self.CFL)
                cas.set('VARIABLE TIME-STEP', self.variable_timestep)
                cas.set('TIME STEP', 0.5)
            else:
                cas.set('NUMBER OF TIME STEPS', int(self.duration/self.timestep))

            # add study
            sc = sc.lower()
            self.add_study('{}_mesh0'.format(sc), 'telemac2d',\
                           't2d_thacker-{}.cas'.format(sc), cas=cas)

            # generate refined geometries:
            input_file = geo_file

            for i in range(self.refinement_levels):
                # refine previous mesh
                output_file = 'geo-thacker_{}'.format(i+1)

                # only refine once
                if ids == 0:
                    run_refine(input_file, output_file, root_dir, bnd_file)

                    self.temporary_files.append(output_file+".slf")
                    self.temporary_files.append(output_file+".cli")

                # add run i
                cas.set('GEOMETRY FILE', output_file+".slf")
                cas.set('BOUNDARY CONDITIONS FILE', output_file+".cli")
                cas.set('RESULTS FILE', "r2d-thacker_{}.slf".format(i+1))

                self.add_study('{}_mesh{}'.format(sc, i+1), 'telemac2d',
                               't2d_thacker-{}_{}.cas'.format(sc, i+1), cas=cas)

                # reset input mesh for next refinment
                input_file = output_file+".slf"
                bnd_file = 'geo-thacker_{}.cli'.format(i+1)

            del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        #TODO: Check convergence slope

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_convergence, vnv_plotbar
        from vvytel.vnv_tools import compute_norm, compute_diff
        from data_manip.computation.volume import compute_fv_cell_area
        import numpy as np
        import matplotlib.tri as mtri
        from os import path
        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D')

        if self.variable_timestep:
            time_label = "$CFL = {}$".format(self.CFL)
        else:
            time_label = "$\\Delta t = {:0.3f}$".format(self.timestep)

        #======================================================================
        # PLOT MESHES:
        #
        for i, res in enumerate(res_list):
            # plot only once:
            if res_labels[i].split('_')[0] == self.schemes[0]:
                vnv_plot2d(\
                    '',
                    res,
                    record=0,
                    fig_size=(6, 6),
                    fig_name="img/mesh_{}".format(i),
                    plot_mesh=True)

        #======================================================================
        # COMPUTE ERRORS:
        #
        ERRORS = True                # with analytic sol on each mesh
        ERRORS_ON_FINE_MESH = True   # with analytic sol on fine mesh
        ERRORS_TIME_INTEGRALS = True # with analytic sol on each mesh
        #                              and integrated in time (unsteady case)
        #
        #----------------------------------------------------------------------
        # Build abscissa of convergence plot (mesh sizes):
        absc = []
        for i, res in enumerate(res_list):
            if res_labels[i].split('_')[0] == self.schemes[0]:
                absc.append(np.sqrt(res.npoin2))

        #----------------------------------------------------------------------
        # COMPUTE ERROR TIME INTEGRALS:
        # Errors are computed in user fortran at each timestep and timeseries
        # are then retreived from txt files. Error time integrals are computed
        # from error timeseries.
        #
        if ERRORS_TIME_INTEGRALS:
            errors_H_Linf_allschemes = []
            errors_H_L1_allschemes = []
            errors_H_L2_allschemes = []

            errors_U_Linf_allschemes = []
            errors_U_L1_allschemes = []
            errors_U_L2_allschemes = []

            errors_V_Linf_allschemes = []
            errors_V_L1_allschemes = []
            errors_V_L2_allschemes = []

            # loop over schemes:
            for i, sc in enumerate(self.schemes):
                errLinf_H = []
                errLinf_U = []
                errLinf_V = []

                errL1_H = []
                errL1_U = []
                errL1_V = []

                errL2_H = []
                errL2_U = []
                errL2_V = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):

                    # Compute only the selected scheme:
                    if res_labels[j].split('_')[0] == sc:

                        # detect if FV or FE scheme:
                        if res_labels[j].split('_')[0] in self.FV_schemes:
                            FV = True
                            U_label = 'HU'
                            V_label = 'HV'
                            U_idx = 4  # location of error HU in err file
                            V_idx = 5  # location of error HV in err file
                        else:
                            FV = False
                            U_label = 'U'
                            V_label = 'V'
                            U_idx = 2 # location of error U in err file
                            V_idx = 3 # location of error V in err file

                        # read error time series file:
                        name = res_labels[j].lower()
                        errorLinf_file = path.join(
                            self.get_vnv_working_dir(name), 'error_Linf.txt')
                        errorL1_file = path.join(
                            self.get_vnv_working_dir(name), 'error_L1.txt')
                        errorL2_file = path.join(
                            self.get_vnv_working_dir(name), 'error_L2.txt')

                        # get errors time series
                        erri = np.genfromtxt(errorLinf_file)
                        err1 = np.genfromtxt(errorL1_file)
                        err2 = np.genfromtxt(errorL2_file)

                        times = erri[:, 0]

                        # compute errors integrals:
                        time_incrs = np.zeros(len(times))
                        for i in range(len(times)):
                            if i == 0:
                                time_incrs[i] = times[i]
                            else:
                                time_incrs[i] = times[i] - times[i-1]

                        errLinf_H.append(np.sum(erri[:, 1]*time_incrs[:])/times[-1])
                        errLinf_U.append(np.sum(erri[:, U_idx]*time_incrs[:])/times[-1])
                        errLinf_V.append(np.sum(erri[:, V_idx]*time_incrs[:])/times[-1])

                        errL1_H.append(np.sum(err1[:, 1]*time_incrs[:])/times[-1])
                        errL1_U.append(np.sum(err1[:, U_idx]*time_incrs[:])/times[-1])
                        errL1_V.append(np.sum(err1[:, V_idx]*time_incrs[:])/times[-1])

                        errL2_H.append(np.sum(err2[:, 1]*time_incrs[:])/times[-1])
                        errL2_U.append(np.sum(err2[:, U_idx]*time_incrs[:])/times[-1])
                        errL2_V.append(np.sum(err2[:, V_idx]*time_incrs[:])/times[-1])

                errors_H = [errLinf_H, errL1_H, errL2_H]
                errors_U = [errLinf_U, errL1_U, errL2_U]
                errors_V = [errLinf_V, errL1_V, errL2_V]

                errors_H_Linf_allschemes.append(errLinf_H)
                errors_H_L1_allschemes.append(errL1_H)
                errors_H_L2_allschemes.append(errL2_H)

                errors_U_Linf_allschemes.append(errLinf_U)
                errors_U_L1_allschemes.append(errL1_U)
                errors_U_L2_allschemes.append(errL2_U)

                errors_V_Linf_allschemes.append(errLinf_V)
                errors_V_L1_allschemes.append(errL1_V)
                errors_V_L2_allschemes.append(errL2_V)

                # Convergence plots:
                vnv_plot1d_convergence(\
                    absc, errors_H,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$\\int_{0}^{t_f} E_i/E_0 dt$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on H with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_thacker_{}_errors_timeintegrals_H".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_U,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$\\int_{0}^{t_f} E_i/E_0 dt$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} with {}'\
                    .format(sc, U_label, time_label),
                    fig_name="img/t2d_thacker_{}_errors_timeintegrals_U".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_V,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$\\int_{0}^{t_f} E_i/E_0 dt$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} with {}'\
                    .format(sc, V_label, time_label),
                    fig_name="img/t2d_thacker_{}_errors_timeintegrals_V".format(sc))

            # reference slopes:
            error1_ref = errors_H_L2_allschemes[0]
            error2_ref = errors_H_L2_allschemes[-1]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            size = (8, 4.5)
            vnv_plot1d_convergence(\
                absc, errors_H_L2_allschemes,
                fig_size=size,
                legend_labels=self.schemes,
                y_label='$\\frac{1}{t_f} \\int_{0}^{t_f} E_i dt$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on H with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_thacker_errors_timeintegrals_H_L2_allsc")

            error1_ref = errors_U_L2_allschemes[0]
            error2_ref = errors_U_L2_allschemes[-1]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            vnv_plot1d_convergence(\
                absc, errors_U_L2_allschemes,
                fig_size=size,
                legend_labels=self.schemes,
                y_label='$\\frac{1}{t_f} \\int_{0}^{t_f} E_i dt$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on U with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_thacker_errors_timeintegrals_U_L2_allsc")

            error1_ref = errors_V_L2_allschemes[0]
            error2_ref = errors_V_L2_allschemes[-1]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            vnv_plot1d_convergence(\
                absc, errors_V_L2_allschemes,
                fig_size=size,
                legend_labels=self.schemes,
                y_label='$\\frac{1}{t_f} \\int_{0}^{t_f} E_i dt$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on V with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_thacker_errors_timeintegrals_V_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels+1):
                ns = len(errors_H_L2_allschemes)
                errors_meshj_H = [\
                    [errors_H_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L2_allschemes[i][j] for i in range(ns)]]

                errors_meshj_U = [\
                    [errors_U_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_U_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_U_L2_allschemes[i][j] for i in range(ns)]]

                errors_meshj_V = [\
                    [errors_V_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_V_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_V_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj_H,
                    fig_size=size,
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on H time integrals: '\
                    '$\\frac{1}{t_f} \\int_{0}^{t_f} E(t) dt$',
                    fig_name="img/t2d_thacker_errors_timeintegrals_H_mesh{}"\
                    .format(j),
                    annotate=True)

                vnv_plotbar(\
                    errors_meshj_U,
                    fig_size=size,
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on U time integrals: '\
                    '$\\frac{1}{t_f} \\int_{0}^{t_f} E(t) dt$',
                    fig_name="img/t2d_thacker_errors_timeintegrals_U_mesh{}"\
                    .format(j),
                    annotate=True)

                vnv_plotbar(\
                    errors_meshj_V,
                    fig_size=size,
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on V time integrals: '\
                    '$\\frac{1}{t_f} \\int_{0}^{t_f} E(t) dt$',
                    fig_name="img/t2d_thacker_errors_timeintegrals_V_mesh{}"\
                    .format(j),
                    annotate=True)

        #----------------------------------------------------------------------
        # COMPUTE ERRORS AT FNAL TIME
        # CASE I: error computed with analytic solution on each mesh
        #
        if ERRORS:
            errors_H_Linf_allschemes = []
            errors_H_L1_allschemes = []
            errors_H_L2_allschemes = []

            # loop over schemes:
            for i, sc in enumerate(self.schemes):
                errLinf_H = []
                errLinf_U = []
                errLinf_V = []

                errL1_H = []
                errL1_U = []
                errL1_V = []

                errL2_H = []
                errL2_U = []
                errL2_V = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):
                    # Compute only the selected scheme:
                    if res_labels[j].split('_')[0] == sc:

                        # detect if FV or FE scheme:
                        if res_labels[j].split('_')[0] in self.FV_schemes:
                            FV = True
                            U_label = 'HU'
                            V_label = 'HV'
                        else:
                            FV = False
                            U_label = 'U'
                            V_label = 'V'

                        # Compute diff between analytic solution and computation
                        H = res.get_data_value('WATER DEPTH', -1)
                        U = res.get_data_value('VELOCITY U', -1)
                        V = res.get_data_value('VELOCITY V', -1)

                        H_ref = res.get_data_value('ANALYTIC SOL H', -1)
                        U_ref = res.get_data_value('ANALYTIC SOL U', -1)
                        V_ref = res.get_data_value('ANALYTIC SOL V', -1)

                        H_diff = compute_diff(H, H_ref, relative=False)
                        if FV:
                            # Get mass matrix (finite volume cells area):
                            massm = compute_fv_cell_area(res.tri)

                            # Compute diff
                            U_diff = compute_diff(H*U, H_ref*U_ref, relative=False)
                            V_diff = compute_diff(H*V, H_ref*V_ref, relative=False)
                        else:
                            # Get mass matrix at final time from file
                            name = res_labels[j].lower()
                            massm_file = path.join(
                                self.get_vnv_working_dir(name), 'mass_matrix_tf.txt')
                            massm = np.genfromtxt(massm_file)

                            # Compute diff
                            U_diff = compute_diff(U, U_ref, relative=False)
                            V_diff = compute_diff(V, V_ref, relative=False)

                        # Compute Linf errors:
                        errLinf_H.append(compute_norm(H_diff, norm='linf', mass=massm))
                        errLinf_U.append(compute_norm(U_diff, norm='linf', mass=massm))
                        errLinf_V.append(compute_norm(V_diff, norm='linf', mass=massm))
                        # Compute L1 errors:
                        errL1_H.append(compute_norm(H_diff, norm='l1', mass=massm))
                        errL1_U.append(compute_norm(U_diff, norm='l1', mass=massm))
                        errL1_V.append(compute_norm(V_diff, norm='l1', mass=massm))
                        # Compute L2 errors:
                        errL2_H.append(compute_norm(H_diff, norm='l2', mass=massm))
                        errL2_U.append(compute_norm(U_diff, norm='l2', mass=massm))
                        errL2_V.append(compute_norm(V_diff, norm='l2', mass=massm))

                errors_H = [errLinf_H, errL1_H, errL2_H]
                errors_U = [errLinf_U, errL1_U, errL2_U]
                errors_V = [errLinf_V, errL1_V, errL2_V]

                errors_H_Linf_allschemes.append(errLinf_H)
                errors_H_L1_allschemes.append(errL1_H)
                errors_H_L2_allschemes.append(errL2_H)

                # Convergence plots:
                vnv_plot1d_convergence(\
                    absc, errors_H,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on H at $t=T/{}$ with {}'\
                    .format(sc, int(self.time_coef), time_label),
                    fig_name="img/t2d_thacker_{}_errors_tf_H".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_U,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=T/{}$ with {}'\
                    .format(sc, U_label, int(self.time_coef), time_label),
                    fig_name="img/t2d_thacker_{}_errors_tf_U".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_V,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=T/{}$ with {}'\
                    .format(sc, V_label, int(self.time_coef), time_label),
                    fig_name="img/t2d_thacker_{}_errors_tf_V".format(sc))

            # reference slopes:
            error1_ref = errors_H_L2_allschemes[0]
            error2_ref = errors_H_L2_allschemes[-1]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            vnv_plot1d_convergence(\
                absc, errors_H_L2_allschemes,
                fig_size=(9, 5),
                legend_labels=self.schemes,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on H with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_thacker_errors_tf_H_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels):
                ns = len(errors_H_L2_allschemes)
                errors_meshj = [\
                    [errors_H_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on H at $t=t_f$',
                    fig_name="img/t2d_thacker_errors_tf_mesh{}"\
                    .format(j),
                    annotate=True)

        #----------------------------------------------------------------------
        # COMPUTE ERRORS AT FNAL TIME
        # CASE II: error is computed with analytic solution on fine mesh
        #
        #  -> This method require interpolation of results on the fine mesh.
        #  -> Linear interpolation of mtri is used.
        #  -> This method is avaible in Telemac2d sources for direct computation
        #     within TELEMAC. See convergence example for more details.
        #
        if ERRORS_ON_FINE_MESH:
            errors_H_Linf_allschemes = []
            errors_H_L1_allschemes = []
            errors_H_L2_allschemes = []

            # loop over schemes:
            for i, sc in enumerate(self.schemes):
                errLinf_H = []
                errLinf_U = []
                errLinf_V = []

                errL1_H = []
                errL1_U = []
                errL1_V = []

                errL2_H = []
                errL2_U = []
                errL2_V = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):
                    # Compute only the selected scheme:
                    if res_labels[j].split('_')[0] == sc:

                        # detect if FV or FE scheme:
                        if res_labels[j].split('_')[0] in self.FV_schemes:
                            FV = True
                            U_label = 'HU'
                            V_label = 'HV'
                        else:
                            FV = False
                            U_label = 'U'
                            V_label = 'V'

                        # Interpolation on fine mesh
                        res_fine = res_list[-1]

                        H = res.get_data_value('WATER DEPTH', -1)
                        U = res.get_data_value('VELOCITY U', -1)
                        V = res.get_data_value('VELOCITY V', -1)

                        H_interp = mtri.LinearTriInterpolator(res.tri, H)
                        U_interp = mtri.LinearTriInterpolator(res.tri, U)
                        V_interp = mtri.LinearTriInterpolator(res.tri, V)

                        H_fine = H_interp(res_fine.tri.x, res_fine.tri.y)
                        U_fine = U_interp(res_fine.tri.x, res_fine.tri.y)
                        V_fine = V_interp(res_fine.tri.x, res_fine.tri.y)

                        H_ref = res_fine.get_data_value('ANALYTIC SOL H', -1)
                        U_ref = res_fine.get_data_value('ANALYTIC SOL U', -1)
                        V_ref = res_fine.get_data_value('ANALYTIC SOL V', -1)

                        H_diff = compute_diff(H_fine, H_ref, relative=False)
                        if FV:
                            # Get mass matrix (finite volume cells area):
                            massm = compute_fv_cell_area(res_fine.tri)

                            # Compute diff
                            U_diff = compute_diff(H_fine*U_fine, H_ref*U_ref, relative=False)
                            V_diff = compute_diff(H_fine*V_fine, H_ref*V_ref, relative=False)
                        else:
                            # Mass matrix of fine mesh
                            name_fine = res_labels[-1].lower()
                            massm_file = path.join(
                                self.get_vnv_working_dir(name_fine), 'mass_matrix_tf.txt')
                            massm = np.genfromtxt(massm_file)

                            # Compute diff
                            U_diff = compute_diff(U_fine, U_ref, relative=False)
                            V_diff = compute_diff(V_fine, V_ref, relative=False)

                        # Compute Linf errors:
                        errLinf_H.append(compute_norm(H_diff, norm='linf', mass=massm))
                        errLinf_U.append(compute_norm(U_diff, norm='linf', mass=massm))
                        errLinf_V.append(compute_norm(V_diff, norm='linf', mass=massm))
                        # Compute L1 errors:
                        errL1_H.append(compute_norm(H_diff, norm='l1', mass=massm))
                        errL1_U.append(compute_norm(U_diff, norm='l1', mass=massm))
                        errL1_V.append(compute_norm(V_diff, norm='l1', mass=massm))
                        # Compute L2 errors:
                        errL2_H.append(compute_norm(H_diff, norm='l2', mass=massm))
                        errL2_U.append(compute_norm(U_diff, norm='l2', mass=massm))
                        errL2_V.append(compute_norm(V_diff, norm='l2', mass=massm))

                errors_H = [errLinf_H, errL1_H, errL2_H]
                errors_U = [errLinf_U, errL1_U, errL2_U]
                errors_V = [errLinf_V, errL1_V, errL2_V]

                errors_H_Linf_allschemes.append(errLinf_H)
                errors_H_L1_allschemes.append(errL1_H)
                errors_H_L2_allschemes.append(errL2_H)

                # Convergence plots:
                vnv_plot1d_convergence(\
                    absc, errors_H,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on H at $t=T/{}$ with {}'\
                    .format(sc, int(self.time_coef), time_label),
                    fig_name="img/t2d_thacker_{}_errors_tf_finemesh_H".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_U,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=T/{}$ with {}'\
                    .format(sc, U_label, int(self.time_coef), time_label),
                    fig_name="img/t2d_thacker_{}_errors_tf_finemesh_U".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_V,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=T/{}$ with {}'\
                    .format(sc, V_label, int(self.time_coef), time_label),
                    fig_name="img/t2d_thacker_{}_errors_tf_finemesh_V".format(sc))

            # reference slopes:
            error1_ref = errors_H_L2_allschemes[0]
            error2_ref = errors_H_L2_allschemes[-1]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            vnv_plot1d_convergence(\
                absc, errors_H_L2_allschemes,
                fig_size=(9, 5),
                legend_labels=self.schemes,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on H with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_thacker_errors_tf_finemesh_H_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels):
                ns = len(errors_H_L2_allschemes)
                errors_meshj = [\
                    [errors_H_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on H at $t=t_f$',
                    fig_name="img/t2d_thacker_errors_tf_finemesh_mesh{}"\
                    .format(j),
                    annotate=True)

        #======================================================================
        # Plot 2d maps on fine mesh:
        for i, res in enumerate(res_list):
            if res_labels[i].split('_')[-1].lower() == 'mesh{}'\
                    .format(self.refinement_levels-1):
                vnv_plot2d(\
                    'WATER DEPTH',
                    res,
                    record=-1,
                    fig_size=(10, 8),
                    fig_name='img/2dmap_H_converged_{}'\
                    .format(res_labels[i].split('_')[0]),
                    fig_title=res_labels[i].split('_')[0],
                    contours=True,
                    filled_contours=True)

                vnv_plot2d(\
                    'VELOCITY U',
                    res,
                    record=-1,
                    fig_size=(10, 8),
                    fig_name='img/2dmap_U_converged_{}'\
                    .format(res_labels[i].split('_')[0]),
                    fig_title=res_labels[i].split('_')[0],
                    contours=True,
                    filled_contours=True)

                vnv_plot2d(\
                    'VELOCITY V',
                    res,
                    record=-1,
                    fig_size=(10, 8),
                    fig_name='img/2dmap_V_converged_{}'\
                    .format(res_labels[i].split('_')[0]),
                    fig_title=res_labels[i].split('_')[0],
                    contours=True,
                    filled_contours=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()

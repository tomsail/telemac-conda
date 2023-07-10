
"""
Validation script for cone
"""
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from pretel.stbtel_refine import run_refine
from config import CFGS

class VnvStudy(AbstractVnvStudy):
    """
    Class for mesh convergence of cone
    """

    def _init(self):
        """
        Defines the general parameters
        """
        self.rank = 4
        self.tags = ['telemac2d']
        self.refinement_levels = 3
        self.temporary_files = []

        # Duration:
        self.time_period = 6.283185308 # one rotation
        self.duration = self.time_period/4. # 1/4 of the complete rotation

        # Time discretization:
        # WARNING: variable timestep not fully fonctionnal with FE methods atm.
        self.variable_timestep = False
        self.CFL = 0.9
        #----------------------------------------------------------------------
        self.timestep = self.time_period/640.
        #print("CONSTANT TIME STEP FIXED TO : {}".format(self.timestep))
        #----------------------------------------------------------------------
        # Manual CFL condition computation to determine timestep:
        dx_0 = 1. # dx_0 ~= Lx/Nx0 = 20/20
        dx_min = dx_0/(2.**self.refinement_levels) #dx on the finest mesh
        um = 15. # max(U) = Uanalytic(x=20, y=20) = 14.1421
        hm = 2.  # max(h) = h0
        sigma = abs(um) + np.sqrt(9.81*hm)
        self.timestep = min(self.timestep, self.CFL*dx_min/sigma)
        #print("CFL: {} TIME STEP FIXED TO : {}"\
        #    .format(self.CFL, self.timestep))
        #----------------------------------------------------------------------

    def _pre(self):
        """
        Defining the studies
        """
        # get TELEMAC root directory:
        root_dir = CFGS.get_root()

        # geometry files for first case
        geo_file = "geo_cone_0.slf"
        bnd_file = "geo_cone_0.cli"

        # set base study:
        cas = TelemacCas('t2d_cone.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', geo_file)
        cas.set('BOUNDARY CONDITIONS FILE', bnd_file)
        cas.set('RESULTS FILE', "r2d_cone_0.slf")
        cas.set('GRAPHIC PRINTOUT PERIOD', 1)
        cas.set('TIME STEP', self.timestep)

        if self.variable_timestep:
            cas.remove('NUMBER OF TIME STEPS')
            cas.set('DURATION', self.duration)
            cas.set('DESIRED COURANT NUMBER', self.CFL)
            cas.set('VARIABLE TIME-STEP', self.variable_timestep)
        else:
            cas.set('NUMBER OF TIME STEPS', int(self.duration/self.timestep))

        self.add_study('cone_mesh0', 'telemac2d', 't2d_cone.cas', cas=cas)

        # Generate refined geometries:
        input_file = geo_file

        for i in range(self.refinement_levels):
            # refine previous mesh
            output_file = 'geo_cone_{}'.format(i+1)
            run_refine(input_file, output_file, root_dir, bnd_file)

            self.temporary_files.append(output_file+".slf")
            self.temporary_files.append(output_file+".cli")

            # add run i
            cas.set('GEOMETRY FILE', output_file+".slf")
            cas.set('BOUNDARY CONDITIONS FILE', output_file+".cli")
            cas.set('RESULTS FILE', "r2d_cone_{}.slf".format(i+1))

            self.add_study('cone_mesh{}'.format(i+1), 'telemac2d',
                           't2d_cone_{}.cas'.format(i+1), cas=cas)

            # reset input mesh for next refinment
            input_file = output_file+".slf"
            bnd_file = 'geo_cone_{}.cli'.format(i+1)

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
        import numpy as np
        import matplotlib.tri as mtri
        from os import path
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_convergence, vnv_plotbar
        from vvytel.vnv_tools import compute_norm, compute_diff

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D')

        var_list = [
            'CHARACT_STRONG',
            'N_SCHEME', 'N_SCHEME   COR1', 'N_SCHEME   COR2', 'LIMP N_SCHEME',
            'NERD SCHEME', 'CHARACT_WEAK',
            'PSI_SCHEME', 'PSI_SCHEME COR1', 'PSI_SCHEME COR2', 'LIPS_SCHEME']

        var_labels_short = [
            'SCHAR', 'N', 'N PC1', 'N PC2', 'N LIPS',
            'NERD', 'WCHAR', 'PSI', 'PSI PC1', 'PSI PC2', 'PSI LIPS']

        if self.variable_timestep:
            time_label = "$CFL = {}$".format(self.CFL)
        else:
            time_label = "$\\Delta t = {:0.3f}$".format(self.timestep)

        #======================================================================
        # PLOT MESHES:
        #
        for i, res in enumerate(res_list):
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
        # Build abscissa of convergence plot:
        absc = []
        for i, res in enumerate(res_list):
            absc.append(np.sqrt(res.npoin2))

        #----------------------------------------------------------------------
        # COMPUTE ERROR TIME INTEGRALS:
        # Errors are computed in user fortran at each timestep and timeseries
        # are then retreived from txt files. Error time integrals are computed
        # from error timeseries.
        #
        if ERRORS_TIME_INTEGRALS:
            # Error lists of all tracers
            errors_Linf_vars = []
            errors_L1_vars = []
            errors_L2_vars = []

            # Loop over tracers:
            for i, var in enumerate(var_list):
                # Error lists (Linf, L1, L2) of one tracer
                errLinf_T = []
                errL1_T = []
                errL2_T = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):

                    # Mass matrix at final time
                    name = res_labels[j].lower()
                    massm_file = self.get_study_file(name+':T2DRFO')
                    massm = np.genfromtxt(massm_file)

                    # Loop over timesteps for time integral computation
                    time_incrs = np.zeros(res.ntimestep)
                    errLi = 0.
                    errL1 = 0.
                    errL2 = 0.

                    for ite in range(res.ntimestep):
                        if ite == 0:
                            time_incrs[ite] = res.times[ite]
                        else:
                            time_incrs[ite] = res.times[ite] - res.times[ite-1]

                        # Compute diff
                        data = res.get_data_value(var, ite)
                        ref = res.get_data_value('ANALYTIC SOL T', ite)
                        diff = compute_diff(data, ref, relative=False)

                        # Compute errors:
                        errLi += time_incrs[ite]*compute_norm(\
                            diff, norm='linf', mass=massm)
                        errL1 += time_incrs[ite]*compute_norm(\
                            diff, norm='l1', mass=massm)
                        errL2 += time_incrs[ite]*compute_norm(\
                            diff, norm='l2', mass=massm)

                    errLinf_T.append(errLi/res.times[-1])
                    errL1_T.append(errL1/res.times[-1])
                    errL2_T.append(errL2/res.times[-1])

                errors_T = [errLinf_T, errL1_T, errL2_T]
                errors_Linf_vars.append(errLinf_T)
                errors_L1_vars.append(errL1_T)
                errors_L2_vars.append(errL2_T)

                # Convergence plots for each variables:
                vnv_plot1d_convergence(\
                    absc, errors_T,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$\\int_{0}^{t_f} E_i/E_0 dt$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors with '\
                    .format(var_labels_short[i]) + time_label,
                    fig_name="img/t2d_cone_errors_timeintegrals_{}"\
                    .format(var_labels_short[i].replace(' ', '')))

            # reference slopes:
            error1_ref = errors_L2_vars[0]
            error2_ref = errors_L2_vars[2]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot for all variables:
            vnv_plot1d_convergence(\
                absc, errors_Linf_vars,
                fig_size=(9, 5),
                legend_labels=var_labels_short,
                y_label='$\\frac{1}{t_f} \\int_{0}^{t_f} E_i dt$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_\\infty$ with {}'.format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                fig_name="img/t2d_cone_errors_timeintegrals_Linf_allvars")

            vnv_plot1d_convergence(\
                absc, errors_L1_vars,
                fig_size=(9, 5),
                legend_labels=var_labels_short,
                y_label='$\\frac{1}{t_f} \\int_{0}^{t_f} E_i dt$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L1$ with {}'.format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                fig_name="img/t2d_cone_errors_timeintegrals_L1_allvars")

            vnv_plot1d_convergence(\
                absc, errors_L2_vars,
                fig_size=(9, 5),
                legend_labels=var_labels_short,
                y_label='$\\frac{1}{t_f} \\int_{0}^{t_f} E_i dt$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L2$ with {}'.format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_cone_errors_timeintegrals_L2_allvars")

            # Bar plot of error integrals for each mesh:
            # loop over meshes:
            for j in range(len(res_list)):
                ns = len(errors_Linf_vars)
                errors_meshj = [[errors_Linf_vars[i][j] for i in range(ns)],\
                                [errors_L1_vars[i][j] for i in range(ns)],\
                                [errors_L2_vars[i][j] for i in range(ns)]]
                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=var_labels_short,
                    y_scale='log',
                    fig_title='Error time integrals: $\\frac{1}{t_f} \\int_{0}^{t_f} E(t) dt$',
                    fig_name="img/t2d_cone_errors_timeintegrals_mesh{}"\
                    .format(j),
                    annotate=True)

        #----------------------------------------------------------------------
        # COMPUTE ERRORS AT FNAL TIME
        # CASE I: error computed with analytic solution on each mesh
        #
        if ERRORS:
            # Error lists of all tracers
            errors_Linf_vars = []
            errors_L1_vars = []
            errors_L2_vars = []

            # Loop over tracers:
            for i, var in enumerate(var_list):
                # Error lists (Linf, L1, L2) of one tracer
                errLinf_T = []
                errL1_T = []
                errL2_T = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):
                    name = res_labels[j].lower()

                    # Mass matrix at final time
                    massm_file = self.get_study_file(name+':T2DRFO')
                    massm = np.genfromtxt(massm_file)

                    # Compute diff
                    data = res.get_data_value(var, -1)
                    ref = res.get_data_value('ANALYTIC SOL T', -1)
                    diff = compute_diff(data, ref, relative=False)

                    # Compute Linf errors:
                    err = compute_norm(diff, norm='linf', mass=massm)
                    errLinf_T.append(err)
                    # Compute L1 errors:
                    err = compute_norm(diff, norm='l1', mass=massm)
                    errL1_T.append(err)
                    # Compute L2 errors:
                    err = compute_norm(diff, norm='l2', mass=massm)
                    errL2_T.append(err)

                errors_T = [errLinf_T, errL1_T, errL2_T]
                errors_Linf_vars.append(errLinf_T)
                errors_L1_vars.append(errL1_T)
                errors_L2_vars.append(errL2_T)

                # Convergence plots for each variables:
                vnv_plot1d_convergence(\
                    absc, errors_T,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors at $t=t_f$ with '\
                    .format(var_labels_short[i]) + time_label,
                    fig_name="img/t2d_cone_errors_tf_{}"\
                    .format(var_labels_short[i].replace(' ', '')))

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
            # Error lists of all tracers
            errors_Linf_vars = []
            errors_L1_vars = []
            errors_L2_vars = []

            # Loop over tracers:
            for i, var in enumerate(var_list):
                # Error lists (Linf, L1, L2) of one tracer
                errLinf_T = []
                errL1_T = []
                errL2_T = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):
                    name = res_labels[j].lower()
                    name_fine = res_labels[-1].lower()

                    # Mass matrix of fine mesh
                    
                    massm_file = self.get_study_file(name_fine+':T2DRFO')
                    massm = np.genfromtxt(massm_file)

                    # Interpolation on fine mesh
                    res_fine = res_list[-1]
                    data = res.get_data_value(var, -1)
                    data_interp = mtri.LinearTriInterpolator(res.tri, data)
                    data_fine = data_interp(res_fine.tri.x, res_fine.tri.y)
                    ref_fine = res_fine.get_data_value('ANALYTIC SOL T', -1)
                    diff = compute_diff(data_fine, ref_fine, relative=False)
                    # Compute Linf errors:
                    err = compute_norm(diff, norm='linf', mass=massm)
                    errLinf_T.append(err)
                    # Compute L1 errors:
                    err = compute_norm(diff, norm='l1', mass=massm)
                    errL1_T.append(err)
                    # Compute L2 errors:
                    err = compute_norm(diff, norm='l2', mass=massm)
                    errL2_T.append(err)

                errors_T = [errLinf_T, errL1_T, errL2_T]

                errors_Linf_vars.append(errLinf_T)
                errors_L1_vars.append(errL1_T)
                errors_L2_vars.append(errL2_T)

                # Convergence plots for each variables:
                vnv_plot1d_convergence(\
                    absc,
                    errors_T,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors at $t=t_f$ with '\
                    .format(var_labels_short[i]) + time_label,
                    fig_name="img/t2d_cone_errors_tf_finemesh_{}"\
                    .format(var_labels_short[i].replace(' ', '')))

            # reference slopes:
            error1_ref = errors_L2_vars[0]
            error2_ref = errors_L2_vars[2]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot for all variables:
            vnv_plot1d_convergence(\
                absc, errors_Linf_vars,
                fig_size=(9, 5),
                legend_labels=var_labels_short,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_\\infty$ at $t=t_f$ with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                fig_name="img/t2d_cone_errors_tf_finemesh_Linf_allvars")

            vnv_plot1d_convergence(\
                absc, errors_L1_vars,
                fig_size=(9, 5),
                legend_labels=var_labels_short,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L1$ at $t=t_f$ with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                fig_name="img/t2d_cone_errors_tf_finemesh_L1_allvars")

            vnv_plot1d_convergence(\
                absc, errors_L2_vars,
                fig_size=(9, 5),
                legend_labels=var_labels_short,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L2$ at $t=t_f$ with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_cone_errors_tf_finemesh_L2_allvars")

            # Bar plot of errors for each mesh:
            # loop over meshes:
            for j in range(len(res_list)):
                ns = len(errors_Linf_vars)
                errors_meshj = [[errors_Linf_vars[i][j] for i in range(ns)],\
                                [errors_L1_vars[i][j] for i in range(ns)],\
                                [errors_L2_vars[i][j] for i in range(ns)]]
                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=var_labels_short,
                    y_scale='log',
                    fig_title='Errors',
                    fig_name="img/t2d_cone_errors_tf_finemesh_mesh{}"\
                    .format(j),
                    annotate=True)

        #======================================================================
        # Plot 2d maps on fine mesh:
        vnv_plot2d(\
            'ANALYTIC SOL T',
            res_list[-1],
            record=-1,
            fig_size=(10, 8),
            fig_name='img/2dmap_converged_EX',
            fig_title='EXACT',
            vmin=0.0,
            vmax=1.0,
            nv=11,
            contours=True,
            filled_contours=True)

        for idx, var in enumerate(var_list):
            vnv_plot2d(\
                var,
                res_list[-1],
                record=-1,
                fig_size=(10, 8),
                fig_name='img/2dmap_converged_{}'\
                         .format(var_labels_short[idx].replace(' ', '')),
                fig_title=var_labels_short[idx],
                vmin=0.0,
                vmax=1.0,
                nv=11,
                contours=True,
                filled_contours=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()

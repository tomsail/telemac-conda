
"""
Validation script for flume tracer
"""
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from pretel.stbtel_refine import run_refine
from config import CFGS

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 4
        self.tags = ['telemac2d', 'fv']
        self.refinement_levels = 3
        self.temporary_files = []

        # Time discretization:
        self.variable_timestep = True
        self.CFL = 0.9

        # Numerical schemes tested:
        self.FE_schemes = []
        self.FV_schemes = ['ROE', 'KIN1', 'HLLC1',
                           'KIN2-MM', 'KIN2-VA',
                           'KIN2-MC', 'HLLC2-MC']

        self.schemes = self.FE_schemes + self.FV_schemes

    def set_flume_values(self, cas, scheme):
        if scheme == 'ROE':
            cas.set('FINITE VOLUME SCHEME', 0)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'KIN1':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'HLLC1':
            cas.set('FINITE VOLUME SCHEME', 5)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'KIN2-MM':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
            cas.set('FLUX LIMITOR FOR TRACERS', 1)
        elif scheme == 'KIN2-VA':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
            cas.set('FLUX LIMITOR FOR TRACERS', 2)
        elif scheme == 'KIN2-MC':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
            cas.set('FLUX LIMITOR FOR TRACERS', 3)
        elif scheme == 'HLLC2-MC':
            cas.set('FINITE VOLUME SCHEME', 5)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
            cas.set('FLUX LIMITOR FOR TRACERS', 3)


    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # Define the cases and studies:
        #
        # get TELEMAC root directory
        root_dir = CFGS.get_root()

        # Loop over schemes
        for ids, sc in enumerate(self.schemes):
            # geometry files for first case
            geo_file = "geo_flume0.slf"
            bnd_file = "geo_flume0.cli"

            # set base study:
            if sc in self.FE_schemes:
                cas = TelemacCas('t2d_flume_FE.cas', get_dico('telemac2d'))
            elif sc in self.FV_schemes:
                cas = TelemacCas('t2d_flume_FV.cas', get_dico('telemac2d'))

            # Set default values
            self.set_flume_values(cas, sc)

            # Set values specific of convergence case
            cas.set('GEOMETRY FILE', geo_file)
            cas.set('BOUNDARY CONDITIONS FILE', bnd_file)
            cas.set('RESULTS FILE', "r2d_flume0.slf")
            cas.set('VARIABLE TIME-STEP', self.variable_timestep)
            cas.set('DESIRED COURANT NUMBER', self.CFL)

            # add study
            sc = sc.lower()
            self.add_study('{}_mesh0'.format(sc), 'telemac2d',\
                           't2d_flume-{}.cas'.format(sc), cas=cas)

            # generate refined geometries:
            input_file = geo_file

            for i in range(self.refinement_levels):
                # refine previous mesh
                output_file = 'geo_flume{}'.format(i+1)

                # only refine once
                if ids == 0:
                    run_refine(input_file, output_file, root_dir, bnd_file)

                    self.temporary_files.append(output_file+".slf")
                    self.temporary_files.append(output_file+".cli")

                # add run i
                cas.set('GEOMETRY FILE', output_file+".slf")
                cas.set('BOUNDARY CONDITIONS FILE', output_file+".cli")
                cas.set('RESULTS FILE', "r2d_flume{}.slf".format(i+1))

                self.add_study('{}_mesh{}'.format(sc, i+1), 'telemac2d',
                               't2d_flume-{}_{}.cas'.format(sc, i+1), cas=cas)

                # reset input mesh for next refinment
                input_file = output_file+".slf"
                bnd_file = 'geo_flume{}.cli'.format(i+1)

            del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
        #TODO

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        import matplotlib.tri as mtri
        from scipy.interpolate import interp1d
        from os import path
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, \
                vnv_plotbar, vnv_plot1d_convergence
        from vvytel.vnv_tools import compute_norm, compute_diff
        from data_manip.computation.volume import compute_fv_cell_area

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
                    fig_size=(10, 2),
                    fig_name="img/mesh_{}".format(i),
                    plot_mesh=True)

        #======================================================================
        # COMPUTE ERRORS:
        #
        ERRORS = True                # with analytic sol on each mesh
        ERRORS_ON_FINE_MESH = True   # with analytic sol on fine mesh
        #
        #----------------------------------------------------------------------
        # Build abscissa of convergence plot (mesh sizes):
        absc = []
        for i, res in enumerate(res_list):
            if res_labels[i].split('_')[0] == self.schemes[0]:
                absc.append(np.sqrt(res.npoin2))

        #----------------------------------------------------------------------
        # COMPUTE ERRORS AT FNAL TIME
        # CASE I: error computed with analytic solution on each mesh
        #
        if ERRORS:
            errors_T1_Linf_allschemes = []
            errors_T1_L1_allschemes = []
            errors_T1_L2_allschemes = []
            errors_T2_Linf_allschemes = []
            errors_T2_L1_allschemes = []
            errors_T2_L2_allschemes = []

            # loop over schemes:
            for i, sc in enumerate(self.schemes):
                errLinf_T1 = []
                errLinf_T2 = []
                errL1_T1 = []
                errL1_T2 = []
                errL2_T1 = []
                errL2_T2 = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):
                    # Compute only the selected scheme:
                    if res_labels[j].split('_')[0] == sc:

                        # Compute diff between analytic solution and computation
                        T1 = res.get_data_value('TRACER 1', -1)
                        T2 = res.get_data_value('TRACER 2', -1)
                        T1_ref = res.get_data_value('SOL T1', -1)
                        T2_ref = res.get_data_value('SOL T2', -1)

                        # Compute diff between ref and solution
                        T1_diff = compute_diff(T1, T1_ref, relative=False)
                        T2_diff = compute_diff(T2, T2_ref, relative=False)

                        # Get mass matrix (finite volume cells area):
                        massm = compute_fv_cell_area(res.tri)

                        # Compute Linf errors:
                        errLinf_T1.append(compute_norm(T1_diff, norm='linf', mass=massm))
                        errLinf_T2.append(compute_norm(T2_diff, norm='linf', mass=massm))
                        # Compute L1 errors:
                        errL1_T1.append(compute_norm(T1_diff, norm='l1', mass=massm))
                        errL1_T2.append(compute_norm(T2_diff, norm='l1', mass=massm))
                        # Compute L2 errors:
                        errL2_T1.append(compute_norm(T1_diff, norm='l2', mass=massm))
                        errL2_T2.append(compute_norm(T2_diff, norm='l2', mass=massm))

                errors_T1 = [errLinf_T1, errL1_T1, errL2_T1]
                errors_T2 = [errLinf_T2, errL1_T2, errL2_T2]

                errors_T1_Linf_allschemes.append(errLinf_T1)
                errors_T1_L1_allschemes.append(errL1_T1)
                errors_T1_L2_allschemes.append(errL2_T1)

                errors_T2_Linf_allschemes.append(errLinf_T2)
                errors_T2_L1_allschemes.append(errL1_T2)
                errors_T2_L2_allschemes.append(errL2_T2)

                # Convergence plots:
                vnv_plot1d_convergence(\
                    absc, errors_T1,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on T1 at $t=tf$ with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_flumetracer_{}_errors_tf_T1".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_T2,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on T2 at $t=tf$ with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_flumetracer_{}_errors_tf_T2".format(sc))

            # reference slopes:
            error1_ref = errors_T1_L2_allschemes[-5]
            error2_ref = errors_T1_L2_allschemes[-2]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            vnv_plot1d_convergence(\
                absc, errors_T1_L2_allschemes,
                fig_size=(9, 5),
                legend_labels=self.schemes,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on T1 with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_flumetracer_errors_tf_T1_L2_allsc")

            # reference slopes:
            error1_ref = errors_T2_L2_allschemes[-5]
            error2_ref = errors_T2_L2_allschemes[-2]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            vnv_plot1d_convergence(\
                absc, errors_T2_L2_allschemes,
                fig_size=(9, 5),
                legend_labels=self.schemes,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on T2 with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_flumetracer_errors_tf_T2_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels+1):
                ns = len(errors_T1_L2_allschemes)
                errors_meshj = [\
                    [errors_T1_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_T1_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_T1_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on T1 at $t=t_f$',
                    fig_name="img/t2d_flumetracer_T1_errors_tf_mesh{}"\
                    .format(j),
                    annotate=True)

            for j in range(self.refinement_levels+1):
                ns = len(errors_T2_L2_allschemes)
                errors_meshj = [\
                    [errors_T2_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_T2_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_T2_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on T2 at $t=t_f$',
                    fig_name="img/t2d_flumetracer_T2_errors_tf_mesh{}"\
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
            errors_T1_Linf_allschemes = []
            errors_T1_L1_allschemes = []
            errors_T1_L2_allschemes = []
            errors_T2_Linf_allschemes = []
            errors_T2_L1_allschemes = []
            errors_T2_L2_allschemes = []

            # loop over schemes:
            for i, sc in enumerate(self.schemes):
                errLinf_T1 = []
                errLinf_T2 = []
                errL1_T1 = []
                errL1_T2 = []
                errL2_T1 = []
                errL2_T2 = []

                # Loop over refinment increments
                for j, res in enumerate(res_list):
                    # Compute only the selected scheme:
                    if res_labels[j].split('_')[0] == sc:

                        # Interpolation of solution on fine mesh
                        res_fine = res_list[-1]

                        T1 = res.get_data_value('TRACER 1', -1)
                        T2 = res.get_data_value('TRACER 2', -1)
                        T1_interp = mtri.LinearTriInterpolator(res.tri, T1)
                        T2_interp = mtri.LinearTriInterpolator(res.tri, T2)
                        T1_fine = T1_interp(res_fine.tri.x, res_fine.tri.y)
                        T2_fine = T2_interp(res_fine.tri.x, res_fine.tri.y)

                        T1_ref = res_fine.get_data_value('SOL T1', -1)
                        T2_ref = res_fine.get_data_value('SOL T2', -1)

                        # Get mass matrix (finite volume cells area):
                        massm = compute_fv_cell_area(res_fine.tri)

                        # Compute diff
                        T1_diff = compute_diff(T1_fine, T1_ref, relative=False)
                        T2_diff = compute_diff(T2_fine, T2_ref, relative=False)

                        # Compute Linf errors:
                        errLinf_T1.append(compute_norm(T1_diff, norm='linf', mass=massm))
                        errLinf_T2.append(compute_norm(T2_diff, norm='linf', mass=massm))
                        # Compute L1 errors:
                        errL1_T1.append(compute_norm(T1_diff, norm='l1', mass=massm))
                        errL1_T2.append(compute_norm(T2_diff, norm='l1', mass=massm))
                        # Compute L2 errors:
                        errL2_T1.append(compute_norm(T1_diff, norm='l2', mass=massm))
                        errL2_T2.append(compute_norm(T2_diff, norm='l2', mass=massm))

                errors_T1 = [errLinf_T1, errL1_T1, errL2_T1]
                errors_T2 = [errLinf_T2, errL1_T2, errL2_T2]

                errors_T1_Linf_allschemes.append(errLinf_T1)
                errors_T1_L1_allschemes.append(errL1_T1)
                errors_T1_L2_allschemes.append(errL2_T1)

                errors_T2_Linf_allschemes.append(errLinf_T2)
                errors_T2_L1_allschemes.append(errL1_T2)
                errors_T2_L2_allschemes.append(errL2_T2)

                # Convergence plots:
                vnv_plot1d_convergence(\
                    absc, errors_T1,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on T1 at $t=tf$ with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_flumetracer_{}_errors_tf_finemesh_T1".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_T2,
                    fig_size=(6, 3),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on T2 at $t=tf$ with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_flumetracer_{}_errors_tf_finemesh_T2".format(sc))

            # reference slopes:
            error1_ref = errors_T1_L2_allschemes[-5]
            error2_ref = errors_T1_L2_allschemes[-2]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            vnv_plot1d_convergence(\
                absc, errors_T1_L2_allschemes,
                fig_size=(9, 5),
                legend_labels=self.schemes,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on T1 with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_flumetracer_errors_tf_finemesh_T1_L2_allsc")

            # reference slopes:
            error1_ref = errors_T2_L2_allschemes[-5]
            error2_ref = errors_T2_L2_allschemes[-2]
            order1_slope = [error1_ref[0]*(absc[0]/absc[i])\
                            for i in range(len(absc))]
            order2_slope = [error2_ref[0]*((absc[0]/absc[i])**2)\
                            for i in range(len(absc))]

            # Convergence plot (only L2 error, and H) for all variables:
            vnv_plot1d_convergence(\
                absc, errors_T2_L2_allschemes,
                fig_size=(9, 5),
                legend_labels=self.schemes,
                y_label='$E_i$',
                x_label='$\\sqrt{N_i/N_0}$',
                fig_title='Errors $L_2$ on T2 with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_flumetracer_errors_tf_finemesh_T2_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels+1):
                ns = len(errors_T1_L2_allschemes)
                errors_meshj = [\
                    [errors_T1_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_T1_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_T1_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on T1 at $t=t_f$',
                    fig_name="img/t2d_flumetracer_T1_errors_tf_finemesh_mesh{}"\
                    .format(j),
                    annotate=True)

            for j in range(self.refinement_levels+1):
                ns = len(errors_T2_L2_allschemes)
                errors_meshj = [\
                    [errors_T2_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_T2_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_T2_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on T2 at $t=t_f$',
                    fig_name="img/t2d_flumetracer_T2_errors_tf_finemesh_mesh{}"\
                    .format(j),
                    annotate=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()


"""
Validation script for bumpcri
"""
from os import path, remove
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from analytic_sol import BumpAnalyticSol
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
        self.tags = ['telemac2d','fv']
        self.refinement_levels = 3
        self.temporary_files = []

        # Boundary conditions:
        self.q_in = 0.3
        self.h_out = 0.5 # not used when flow is critical

        # Analytic solution:
        self.sol = BumpAnalyticSol(\
            flow='cri', Q=self.q_in, hl=self.h_out, length=20.,\
            bottom_function='exponential', N=801)
        self.sol()
        self.sol_file = "ANALYTIC_SOL_CONVERGENCE.txt"
        if path.exists(self.sol_file):
            remove(self.sol_file)

        self.sol.savetxt(self.sol_file)

        # Duration:
        self.duration = 120.

        # Time discretization:
        # WARNING: variable timestep not fully fonctionnal with FE methods atm.
        self.variable_timestep = True
        self.CFL = 0.9
        #----------------------------------------------------------------------
        self.timestep = 0.003
        #print("CONSTANT TIME STEP FIXED TO : {}".format(self.timestep))
        #----------------------------------------------------------------------
        # Manual CFL condition computation to determine timestep:
        dx_0 = 0.5 # dx_0 ~= Lx/Nx0 = 20/40
        dx_min = dx_0/(2.**self.refinement_levels) #dx on the finest mesh
        um = np.max(self.sol.U)
        hm = np.max(self.sol.H)
        sigma = abs(um) + np.sqrt(9.81*hm)
        self.timestep = min(self.timestep, self.CFL*dx_min/sigma)
        #print("CFL: {} TIME STEP FIXED TO : {}"\
        #    .format(self.CFL, self.timestep))
        #----------------------------------------------------------------------

        # Numerical schemes tested:
        self.treatment_of_the_linear_system = 2
        self.FE_schemes = ['CHARAC', 'N', 'PSI', 'LIPS', 'NERD', 'ERIA']
        self.FV_schemes = ['ROE', 'KIN1', 'KIN2', 'HLLC1', 'HLLC2', 'WAF']
        self.schemes = self.FE_schemes + self.FV_schemes

    def set_bumpcri_values(self, cas, scheme):
        cas.set('PRESCRIBED FLOWRATES', [0., self.q_in])
        cas.set("FORMATTED DATA FILE 1", self.sol_file)
        cas.remove('PRESCRIBED ELEVATIONS')
        if scheme == 'CHARAC':
            cas.set('TYPE OF ADVECTION', [1, 5])
        elif scheme == 'N':
            cas.set('TYPE OF ADVECTION', [4, 5])
        elif scheme == 'PSI':
            cas.set('TYPE OF ADVECTION', [5, 5])
        elif scheme == 'LIPS':
            cas.set('TYPE OF ADVECTION', [5, 5])
            cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 4)
        elif scheme == 'NERD':
            cas.set('TYPE OF ADVECTION', [14, 5])
            cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
            cas.set('TREATMENT OF NEGATIVE DEPTHS', 2)
            cas.set('TIDAL FLATS', True)
            cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
            cas.set('MASS-LUMPING ON H', 1.)
            cas.set('SUPG OPTION', [2, 0])
        elif scheme == 'ERIA':
            cas.set('TYPE OF ADVECTION', [15, 5])
            cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
            cas.set('TREATMENT OF NEGATIVE DEPTHS', 3)
            cas.set('TIDAL FLATS', True)
            cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
            cas.set('MASS-LUMPING ON H', 1.)
            cas.set('SUPG OPTION', [2, 0])
        elif scheme == 'ROE':
            cas.set('FINITE VOLUME SCHEME', 0)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'KIN1':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'KIN2':
            cas.set('FINITE VOLUME SCHEME', 1)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
            cas.set('LIMITEUR DE FLUX POUR H PLUS Z', 1)
            cas.set('LIMITEUR DE FLUX POUR U ET V', 2)
        elif scheme == 'HLLC1':
            cas.set('FINITE VOLUME SCHEME', 5)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)
        elif scheme == 'HLLC2':
            cas.set('FINITE VOLUME SCHEME', 5)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
            cas.set('FINITE VOLUME SCHEME TIME ORDER', 1)
            cas.set('LIMITEUR DE FLUX POUR H PLUS Z', 1)
            cas.set('LIMITEUR DE FLUX POUR U ET V', 2)
        elif scheme == 'WAF':
            cas.set('FINITE VOLUME SCHEME', 6)
            cas.set('FINITE VOLUME SCHEME SPACE ORDER', 1)


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
            geo_file = "geo_bump_tor0.slf"
            bnd_file = "geo_bump_tor0.cli"

            # set base study:
            if sc in self.FE_schemes:
                cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
            elif sc in self.FV_schemes:
                cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))

            # Set default values
            self.set_bumpcri_values(cas, sc)

            # Set values specific of convergence case
            cas.set('GEOMETRY FILE', geo_file)
            cas.set('BOUNDARY CONDITIONS FILE', bnd_file)
            cas.set('RESULTS FILE', "r2d_bump_0.slf")
            cas.set('TREATMENT OF THE LINEAR SYSTEM',\
                self.treatment_of_the_linear_system)
            cas.set('TIME STEP', self.timestep)

            # timestep options
            if sc in self.FV_schemes:
                cas.remove('NUMBER OF TIME STEPS')
                cas.set('DURATION', self.duration)
                cas.set('DESIRED COURANT NUMBER', self.CFL)
                cas.set('VARIABLE TIME-STEP', self.variable_timestep)
            elif sc in self.FE_schemes:
                cas.set('NUMBER OF TIME STEPS', int(self.duration/self.timestep))

            # add study
            sc = sc.lower()
            self.add_study('{}_mesh0'.format(sc), 'telemac2d',\
                           't2d_bump-{}.cas'.format(sc), cas=cas)

            # generate refined geometries:
            input_file = geo_file

            for i in range(self.refinement_levels):
                # refine previous mesh
                output_file = 'geo_bump_tor{}'.format(i+1)

                # only refine once
                if ids == 0:
                    run_refine(input_file, output_file, root_dir, bnd_file)

                    self.temporary_files.append(output_file+".slf")
                    self.temporary_files.append(output_file+".cli")

                # add run i
                cas.set('GEOMETRY FILE', output_file+".slf")
                cas.set('BOUNDARY CONDITIONS FILE', output_file+".cli")
                cas.set('RESULTS FILE', "r2d_bump_{}.slf".format(i+1))

                self.add_study('{}_mesh{}'.format(sc, i+1), 'telemac2d',
                               't2d_bump-{}_{}.cas'.format(sc, i+1), cas=cas)

                # reset input mesh for next refinment
                input_file = output_file+".slf"
                bnd_file = 'geo_bump_tor{}.cli'.format(i+1)

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
        from postel.plot_vnv import vnv_plot2d, \
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

                        # Interpolation of analytic sol on mesh
                        npoint = res.npoin2
                        H_interp = interp1d(self.sol.x, self.sol.H, kind='linear')
                        U_interp = interp1d(self.sol.x, self.sol.U, kind='linear')
                        V_interp = interp1d(self.sol.x, self.sol.V, kind='linear')

                        H_ref = np.zeros((npoint))
                        U_ref = np.zeros((npoint))
                        V_ref = np.zeros((npoint))

                        for k in range(npoint):
                            xk = res.tri.x[k]
                            H_ref[k] = H_interp(xk)
                            U_ref[k] = U_interp(xk)
                            V_ref[k] = V_interp(xk)

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

                        # Compute diff between ref and solution
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
                            massm_file = self.get_study_file(name+':T2DRFO')
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
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on H at $t=t_f$ with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_bumpcri_{}_errors_tf_H".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_U,
                    fig_size=(6, 3),
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=t_f$ with {}'\
                    .format(sc, U_label, time_label),
                    fig_name="img/t2d_bumpcri_{}_errors_tf_U".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_V,
                    fig_size=(6, 3),
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=t_f$ with {}'\
                    .format(sc, V_label, time_label),
                    fig_name="img/t2d_bumpcri_{}_errors_tf_V".format(sc))

            # reference slopes:
            error1_ref = errors_H_L2_allschemes[-3]
            error2_ref = errors_H_L2_allschemes[-2]
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
                fig_title='Errors $L^2$ on H with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_bumpcri_errors_tf_H_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels+1):
                ns = len(errors_H_L2_allschemes)
                errors_meshj = [\
                    [errors_H_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on H at $t=t_f$',
                    fig_name="img/t2d_bumpcri_errors_tf_mesh{}"\
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

                        # Interpolation of solution on fine mesh
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

                        # Interpolation of analytic sol on fine mesh
                        npoint = res_fine.npoin2
                        H_interp = interp1d(self.sol.x, self.sol.H, kind='linear')
                        U_interp = interp1d(self.sol.x, self.sol.U, kind='linear')
                        V_interp = interp1d(self.sol.x, self.sol.V, kind='linear')

                        H_ref = np.zeros((npoint))
                        U_ref = np.zeros((npoint))
                        V_ref = np.zeros((npoint))

                        for k in range(npoint):
                            xk = res_fine.tri.x[k]
                            H_ref[k] = H_interp(xk)
                            U_ref[k] = U_interp(xk)
                            V_ref[k] = V_interp(xk)

                        # Compute diff between ref and solution
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
                            massm_file = self.get_study_file(name_fine+':T2DRFO')
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
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on H at $t=t_f$ with {}'\
                    .format(sc, time_label),
                    fig_name="img/t2d_bumpcri_{}_errors_tf_finemesh_H".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_U,
                    fig_size=(6, 3),
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=t_f$ with {}'\
                    .format(sc, U_label, time_label),
                    fig_name="img/t2d_bumpcri_{}_errors_tf_finemesh_U".format(sc))

                vnv_plot1d_convergence(\
                    absc, errors_V,
                    fig_size=(6, 3),
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    y_label='$E_i/E_0$',
                    x_label='$\\sqrt{N_i/N_0}$',
                    fig_title='{}: errors on {} at $t=t_f$ with {}'\
                    .format(sc, V_label, time_label),
                    fig_name="img/t2d_bumpcri_{}_errors_tf_finemesh_V".format(sc))

            # reference slopes:
            error1_ref = errors_H_L2_allschemes[-3]
            error2_ref = errors_H_L2_allschemes[-2]
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
                fig_title='Errors $L^2$ on H with {}'\
                .format(time_label),
                y_relative=False,
                plot_firstorder_slope=False,
                plot_secondorder_slope=False,
                reference_data=[order1_slope, order2_slope],
                reference_labels=['order 1', 'order 2'],
                reference_colors=['r', 'b'],
                fig_name="img/t2d_bumpcri_errors_tf_finemesh_H_L2_allsc")

            # Bar plot of error integrals for all variables for each mesh:
            # loop over meshes:
            for j in range(self.refinement_levels+1):
                ns = len(errors_H_L2_allschemes)
                errors_meshj = [\
                    [errors_H_Linf_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L1_allschemes[i][j] for i in range(ns)],\
                    [errors_H_L2_allschemes[i][j] for i in range(ns)]]

                vnv_plotbar(\
                    errors_meshj,
                    fig_size=(10, 5),
                    legend_labels=['$L^\\infty$', '$L^1$', '$L^2$'],
                    x_labels=self.schemes,
                    y_scale='log',
                    fig_title='Error on H at $t=t_f$',
                    fig_name="img/t2d_bumpcri_errors_tf_finemesh_mesh{}"\
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
                    fig_size=(10, 3),
                    fig_name='img/2dmap_H_converged_{}'\
                    .format(res_labels[i].split('_')[0]),
                    fig_title=res_labels[i].split('_')[0],
                    contours=True,
                    filled_contours=True)

                vnv_plot2d(\
                    'VELOCITY U',
                    res,
                    record=-1,
                    fig_size=(10, 3),
                    fig_name='img/2dmap_U_converged_{}'\
                    .format(res_labels[i].split('_')[0]),
                    fig_title=res_labels[i].split('_')[0],
                    contours=True,
                    filled_contours=True)

        #======================================================================
        # Closing files
        for res in res_list:
            res.close()

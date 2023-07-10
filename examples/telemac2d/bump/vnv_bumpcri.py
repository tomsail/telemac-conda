
"""
Validation script for bumpcri
"""
from os import path, remove
import numpy as np
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from analytic_sol import BumpAnalyticSol

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac2d','fv']

        # Boundary conditions:
        self.q_in = 0.3
        self.h_out = 0.5 # not used when flow is critical

        # Analytical solution:
        self.sol = BumpAnalyticSol(\
            flow='cri', Q=self.q_in, hl=self.h_out, length=20.,\
            bottom_function='exponential', N=801)
        self.sol_file = "ANALYTIC_SOL_BUMPCRI.txt"
        self.sol()
        if path.exists(self.sol_file):
            remove(self.sol_file)

        self.sol.savetxt(self.sol_file)

    def set_bumpcri_values(self, cas):
        """
        Setting bumcri specific values to TelemacCas
        """
        cas.set('BOUNDARY CONDITIONS FILE', "geo_bump_tor.cli", convert=True)
        cas.set('PRESCRIBED FLOWRATES', [0., self.q_in])
        cas.set('FORMATTED DATA FILE 1', self.sol_file)
        cas.remove('PRESCRIBED ELEVATIONS')

    def _pre(self):
        """
        Defining the studies
        """
        #======================================================================
        # CHAR run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [1, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        self.add_study('char_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # CHAR parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('char_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # N run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [4, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        self.add_study('n_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # N parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('n_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # PSI run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [5, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        self.add_study('psi_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # PSI parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('psi_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # PSI LIPS run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [5, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 4)
        self.add_study('lips_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # PSI LIPS parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('lips_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # NERD run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [14, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 2)
        cas.set('TIDAL FLATS', True)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        cas.set('MASS-LUMPING ON H', 1.)
        cas.set('SUPG OPTION', [2, 0])
        cas.set('TIME STEP', 0.005)
        self.add_study('nerd_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # NERD parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('nerd_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # ERIA run
        cas = TelemacCas('t2d_bump_FE.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('TYPE OF ADVECTION', [15, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 3)
        cas.set('TIDAL FLATS', True)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        cas.set('MASS-LUMPING ON H', 1.)
        cas.set('SUPG OPTION', [2, 0])
        cas.set('TIME STEP', 0.005)
        self.add_study('eria_seq', 'telemac2d', 't2d_bump_FE.cas', cas=cas)
        # ERIA parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('eria_par', 'telemac2d', 't2d_bump_FE_par.cas', cas=cas)
        del cas

        #======================================================================
        # KIN1 run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # KIN1 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin1_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # KIN2 run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('kin2_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # KIN2 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('kin2_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

        #======================================================================
        # HLLC run
        cas = TelemacCas('t2d_bump_FV.cas', get_dico('telemac2d'))
        self.set_bumpcri_values(cas)
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc_seq', 'telemac2d', 't2d_bump_FV.cas', cas=cas)
        # HLLC parallel mode
        cas.set('PARALLEL PROCESSORS', 4)
        self.add_study('hllc_par', 'telemac2d', 't2d_bump_FV_par.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        self.check_epsilons('char_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('char_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('char_seq:T2DRES', 'char_par:T2DRES', eps=[1.e-13])

        self.check_epsilons('n_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('n_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('n_seq:T2DRES', 'n_par:T2DRES', eps=[1.e-9])

        self.check_epsilons('psi_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('psi_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('psi_seq:T2DRES', 'psi_par:T2DRES', eps=[1.e-11])

        self.check_epsilons('lips_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.6])
        self.check_epsilons('lips_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.6])
        self.check_epsilons('lips_seq:T2DRES', 'lips_par:T2DRES', eps=[1.e-13])

        self.check_epsilons('nerd_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('nerd_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.4])
        self.check_epsilons('nerd_seq:T2DRES', 'nerd_par:T2DRES', eps=[1.e-13])

        self.check_epsilons('eria_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.6])
        self.check_epsilons('eria_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.6])
        self.check_epsilons('eria_seq:T2DRES', 'eria_par:T2DRES', eps=[1.e-13])

        self.check_epsilons('kin1_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.3])
        self.check_epsilons('kin1_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.3])
        self.check_epsilons('kin1_seq:T2DRES', 'kin1_par:T2DRES', eps=[1.e-14])

        self.check_epsilons('kin2_seq:T2DRES', 'f2d_bumpcri.slf', eps=[0.7])
        self.check_epsilons('kin2_par:T2DRES', 'f2d_bumpcri.slf', eps=[0.7])
        self.check_epsilons('kin2_seq:T2DRES', 'kin2_par:T2DRES', eps=[6.e-4])

        self.check_epsilons('hllc_seq:T2DRES', 'f2d_bumpcri.slf', eps=[1e-8])
        self.check_epsilons('hllc_par:T2DRES', 'f2d_bumpcri.slf', eps=[1e-8])
        self.check_epsilons('hllc_seq:T2DRES', 'hllc_par:T2DRES', eps=[1.e-14])

    def _post(self):
        """
        Post-treatment processes
        """
        from scipy.interpolate import interp1d
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, \
                vnv_plotbar, vnv_plotbar_cpu_times
        from vvytel.vnv_tools import compute_norm, compute_diff
        from data_manip.computation.volume import compute_fv_cell_area

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        geom, _ = self.get_study_res('hllc_seq:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('hllc_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D', whitelist=['seq'])

        #======================================================================
        # DESCRIPTION PLOTS:
        #
        # Plot bathy:
        vnv_plot1d_polylines(\
            'BOTTOM',
            res,
            '',
            fig_size=(8, 2),
            record=0,
            fig_name='img/bumpcri_bathy',
            plot_bottom=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumpcri_mesh0',
            annotate_bnd=False,
            plot_mesh=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumpcri_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        #======================================================================
        # FIRST OBSERVATION RESULTS - 1D PLOTS
        #
        # Plot free surface:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            'Elevation',
            fig_size=(5, 4),
            record=0,
            ref_data=np.c_[self.sol.x, self.sol.E],
            ref_label='Analytical',
            fig_name='img/bumpcri_free_surface_0',
            ylim=[0., 0.6],
            plot_bottom=True)

        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            'Elevation',
            fig_size=(5, 4),
            record=-1,
            ref_data=np.c_[self.sol.x, self.sol.E],
            ref_label='Analytical',
            fig_name='img/bumpcri_free_surface',
            ylim=[0., 0.6],
            plot_bottom=True)

        # Plot froud number:
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res,
            'Froude number',
            fig_size=(5, 4),
            record=0,
            ref_data=np.c_[self.sol.x, self.sol.F],
            ref_label='Analytical',
            fig_name='img/bumpcri_froude_number_0',
            y_label='Fr',
            plot_bottom=False)

        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res,
            'Froude number',
            fig_size=(5, 4),
            record=-1,
            ref_data=np.c_[self.sol.x, self.sol.F],
            ref_label='Analytical',
            fig_name='img/bumpcri_froude_number',
            y_label='Fr',
            plot_bottom=False)

        #======================================================================
        # FIRST OBSERVATION RESULTS - 2D PLOTS
        #
        # Plot depth:
        vnv_plot2d(\
            'FREE SURFACE',
            res,
            record=-1,
            fig_size=(10, 2),
            fig_name='img/bumpcri_elevation_2d',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True)

        # Plot velocity:
        vnv_plot2d(\
            'VELOCITY',
            res,
            record=-1,
            fig_size=(10, 2),
            fig_name='img/bumpcri_velocity_2d_vector',
            cbar_label='Velocity (m/s)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True,
            vectors=True, vectors_scale=30,
            grid_resolution=[10, 10])

        #======================================================================
        # COMPARISON OF NUMERICAL SCHEMES:
        #
        #----------------------------------------------------------------------
        # Computation time:
        vnv_plotbar_cpu_times(\
            self.action_time,
            fig_size=(7, 2.5),
            fig_name='img/bumpcri_cpu_times')

        #----------------------------------------------------------------------
        # Accuracy of free surface (1D slice):
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            res_labels,
            record=-1,
            fig_size=(6, 5),
            ref_data=np.c_[self.sol.x, self.sol.E],
            ref_label='Analytical',
            fig_name='img/bumpcri_elevation_1dslice_comparison_tf',
            markers=True,
            markevery=15,
            plot_bottom=False)

        # Accuracy of velocity (1D slice):
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res_list,
            res_labels,
            record=-1,
            ref_data=np.c_[self.sol.x, self.sol.F],
            ref_label='Analytical',
            fig_size=(6, 5),
            fig_name='img/bumpcri_froud_1dslice_comparison_tf',
            markers=True,
            markevery=15,
            y_label='Fr',
            plot_bottom=False)

        #----------------------------------------------------------------------
        # Error at t=tf (computed from mass matrix file):
        #
        # Compute errors at final time for each case:
        errLinf_H = [] # error Linf on H at tf
        errLinf_U = [] # error Linf on U at tf
        errL1_H = [] # error L1 on H at tf
        errL1_U = [] # error L1 on U at tf
        errL2_H = [] # error L2 on H at tf
        errL2_U = [] # error L2 on U at tf

        idx = 0
        for name, study in self.studies.items():
            if 'seq' in name:
                # Mass matrix at final time
                massm_file = self.get_study_file(name+':T2DRFO')
                massm = np.genfromtxt(massm_file)

                # Projection of analytic sol on mesh
                npoint = res_list[idx].npoin2
                H_interp = interp1d(self.sol.x, self.sol.H, kind='linear')
                U_interp = interp1d(self.sol.x, self.sol.U, kind='linear')
                H_ref = np.zeros((npoint))
                U_ref = np.zeros((npoint))
                for i in range(npoint):
                    xi = res_list[idx].tri.x[i]
                    H_ref[i] = H_interp(xi)
                    U_ref[i] = U_interp(xi)

                # Compute diff
                H = res_list[idx].get_data_value('WATER DEPTH', -1)
                U = res_list[idx].get_data_value('VELOCITY U', -1)
                H_diff = compute_diff(H, H_ref, relative=False)
                U_diff = compute_diff(U, U_ref, relative=False)

                # Compute Linf errors:
                errLinf_H.append(compute_norm(H_diff, norm='linf', mass=massm))
                errLinf_U.append(compute_norm(U_diff, norm='linf', mass=massm))
                # Compute L1 errors:
                errL1_H.append(compute_norm(H_diff, norm='l1', mass=massm))
                errL1_U.append(compute_norm(U_diff, norm='l1', mass=massm))
                # Compute L2 errors:
                errL2_H.append(compute_norm(H_diff, norm='l2', mass=massm))
                errL2_U.append(compute_norm(U_diff, norm='l2', mass=massm))

                idx += 1

        errors_H_tf = [errLinf_H, errL1_H, errL2_H]
        errors_U_tf = [errLinf_U, errL1_U, errL2_U]

        # Bar plots of errors at final time
        vnv_plotbar(\
            errors_H_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on H at $t=t_f$',
            y_scale='log',
            fig_name="img/bumpcri_errors_H_tf",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U at $t=t_f$',
            y_scale='log',
            fig_name="img/bumpcri_errors_U_tf",
            annotate=True)

        #======================================================================
        # Closing files
        geom.close()
        res.close()

        for res in res_list:
            res.close()

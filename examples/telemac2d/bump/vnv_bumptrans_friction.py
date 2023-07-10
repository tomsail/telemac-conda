
"""
Validation script for bumptrans
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
        self.rank = 1
        self.tags = ['telemac2d','fv']

    def _pre(self):
        """
        Defining the studies
        """
        # bumptrans p1 scalar mode
        self.add_study('vnv_bumptrans_scal',
                       'telemac2d',
                       't2d_bumptrans_friction_FE.cas')

        #======================================================================
        # CHAR run
        cas = TelemacCas('t2d_bumptrans_friction_FE.cas', get_dico('telemac2d'))
        cas.set('TYPE OF ADVECTION', [1, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        self.add_study('char_seq', 'telemac2d',\
            't2d_bumptrans_friction_FE.cas', cas=cas)
        del cas

        #======================================================================
        # N run
        cas = TelemacCas('t2d_bumptrans_friction_FE.cas', get_dico('telemac2d'))
        cas.set('TYPE OF ADVECTION', [4, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        self.add_study('n_seq', 'telemac2d',\
            't2d_bumptrans_friction_FE.cas', cas=cas)
        del cas

        #======================================================================
        # PSI run
        cas = TelemacCas('t2d_bumptrans_friction_FE.cas', get_dico('telemac2d'))
        cas.set('TYPE OF ADVECTION', [5, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        self.add_study('psi_seq', 'telemac2d',\
            't2d_bumptrans_friction_FE.cas', cas=cas)
        del cas

        #======================================================================
        # PSI LIPS run
        cas = TelemacCas('t2d_bumptrans_friction_FE.cas', get_dico('telemac2d'))
        cas.set('TYPE OF ADVECTION', [5, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        # SCHEME OPTION FOR ADVECTION OF VELOCITY:
        # 1: NONE / 2:COR1 / 3:COR2 / 4:LIPS
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 4)
        self.add_study('lips_seq', 'telemac2d',\
            't2d_bumptrans_friction_FE.cas', cas=cas)
        del cas

        #======================================================================
        # NERD run
        cas = TelemacCas('t2d_bumptrans_friction_FE.cas', get_dico('telemac2d'))
        cas.set('TYPE OF ADVECTION', [14, 5])
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 2)
        cas.set('TIDAL FLATS', True)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        cas.set('MASS-LUMPING ON H', 1.)
        cas.set('SUPG OPTION', [2, 0])
        self.add_study('nerd_seq', 'telemac2d',\
            't2d_bumptrans_friction_FE.cas', cas=cas)
        del cas

        #======================================================================
        # ERIA run
        cas = TelemacCas('t2d_bumptrans_friction_FE.cas', get_dico('telemac2d'))
        cas.set('TYPE OF ADVECTION', [15, 5])
        # MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES changed to 40
        # to prevent from multiple warning messages
        cas.set('MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES', 40)
        cas.set('TREATMENT OF THE LINEAR SYSTEM', 2)
        cas.set('TREATMENT OF NEGATIVE DEPTHS', 3)
        cas.set('TIDAL FLATS', True)
        cas.set('OPTION FOR THE TREATMENT OF TIDAL FLATS', 1)
        cas.set('MASS-LUMPING ON H', 1.)
        cas.set('SUPG OPTION', [2, 0])
        self.add_study('eria_seq', 'telemac2d',\
            't2d_bumptrans_friction_FE.cas', cas=cas)
        del cas

        #======================================================================
        # KIN1 run
        cas = TelemacCas('t2d_bumptrans_friction_FV.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        self.add_study('kin1_seq', 'telemac2d',\
            't2d_bumptrans_friction_FV.cas', cas=cas)
        del cas

        #======================================================================
        # KIN2 run
        cas = TelemacCas('t2d_bumptrans_friction_FV.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 1)
        cas.set('FINITE VOLUME SCHEME SPACE ORDER', 2)
        self.add_study('kin2_seq', 'telemac2d',\
            't2d_bumptrans_friction_FV.cas', cas=cas)
        del cas

        #======================================================================
        # HLLC run
        cas = TelemacCas('t2d_bumptrans_friction_FV.cas', get_dico('telemac2d'))
        cas.set('FINITE VOLUME SCHEME', 5)
        self.add_study('hllc_seq', 'telemac2d',\
            't2d_bumptrans_friction_FV.cas', cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        pass

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        from os import path
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d, \
                vnv_plotbar, vnv_plotbar_cpu_times

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
            fig_name='img/bumptransfric_bathy',
            plot_bottom=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumptransfric_mesh0',
            annotate_bnd=False,
            plot_mesh=True)

        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/bumptransfric_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        #======================================================================
        # FIRST OBSERVATION OF NERD RESULTS:
        #
        # Plot free surface:
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res,
            'Elevation',
            fig_size=(5, 4),
            record=-1,
            ref_name='EXACT ELEVATION',
            fig_name='img/bumptransfric_free_surface',
            ylim=[-0.2, .8],
            plot_bottom=True)

        # Plot froud number:
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res,
            'Froude number',
            fig_size=(5, 4),
            record=-1,
            fig_name='img/bumptransfric_froude_number',
            y_label='Fr',
            plot_bottom=False)

        # Plot velocity:
        vnv_plot2d(\
            'VELOCITY',
            res,
            record=-1,
            fig_size=(10, 2),
            fig_name='img/bumptransfric_velocity_vector',
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
            fig_size=(7, 3),
            fig_name='img/bumptransfric_cpu_times')

        #----------------------------------------------------------------------
        # Accuracy of free surface (1D slice):
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            res_labels,
            record=-1,
            fig_size=(6, 5),
            ref_name='EXACT ELEVATION',
            fig_name='img/bumptransfric_elevation_1dslice_comparison_tf',
            markers=True,
            markevery=15,
            plot_bottom=False)

        # Accuracy of velocity (1D slice):
        vnv_plot1d_polylines(\
            'FROUDE NUMBER',
            res_list,
            res_labels,
            record=-1,
            fig_size=(6, 5),
            fig_name='img/bumptransfric_froud_1dslice_comparison_tf',
            markers=True,
            markevery=15,
            y_label='Fr',
            plot_bottom=False)

        #----------------------------------------------------------------------
        # Error at t=tf (computed from mass matrix file):
        #
        # Compute errors at final time for each case:
        errLinf_Htf_list = [] # error Linf on H at tf
        errLinf_Utf_list = [] # error Linf on U at tf

        errL1_Htf_list = [] # error L1 on H at tf
        errL1_Utf_list = [] # error L1 on U at tf

        errL2_Htf_list = [] # error L2 on H at tf
        errL2_Utf_list = [] # error L2 on U at tf

        idx = 0
        for name, study in self.studies.items():
            if 'seq' in name:
                # Mass matrix at final time
                massm_file = self.get_study_file(name+':T2DRFO')
                massm = np.genfromtxt(massm_file)

                # Linf errors:
                errLinf_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='EXACT DEPTH',
                    record=-1,
                    norm='linf'))

                errLinf_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='EXACT VELOCITY',
                    record=-1,
                    norm='linf'))

                # L1 errors:
                errL1_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='EXACT DEPTH',
                    record=-1,
                    norm='l1',
                    mass=massm))

                errL1_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='EXACT VELOCITY',
                    record=-1,
                    norm='l1',
                    mass=massm))

                # L2 errors:
                errL2_Htf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='WATER DEPTH',
                    var2='EXACT DEPTH',
                    record=-1,
                    norm='l2',
                    mass=massm))

                errL2_Utf_list.append(self.compute_errors(
                    res1=res_list[idx],
                    var1='VELOCITY U',
                    var2='EXACT VELOCITY',
                    record=-1,
                    norm='l2',
                    mass=massm))

                idx += 1

        errors_H_tf = [errLinf_Htf_list, errL1_Htf_list, errL2_Htf_list]
        errors_U_tf = [errLinf_Utf_list, errL1_Utf_list, errL2_Utf_list]

        # Bar plots of errors at final time
        vnv_plotbar(\
            errors_H_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on H at $t=t_f$',
            y_scale='log',
            fig_name="img/bumptransfric_errors_H_tf",
            annotate=True)

        vnv_plotbar(\
            errors_U_tf,
            fig_size=(10, 4),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels,
            fig_title='Errors on U at $t=t_f$',
            y_scale='log',
            fig_name="img/bumptransfric_errors_U_tf",
            annotate=True)

        #======================================================================
        # Closing files
        geom.close()
        res.close()
        for res in res_list:
            res.close()


"""
Validation script for cone
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """
        # rotating cone in 2D
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_cone.cas')

        # rotating cone parallel mode
        cas = TelemacCas('t2d_cone.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_cone_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_cone.slf',
                            eps=[1e-9, 1e-9, 1e-9, 1e-9, 1e-9, \
                                 1e-9, 1e-9, 1e-9, 1e-7, 1e-9, \
                                 1e-9, 1e-4, 1e-8, 1e-4, 1e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_cone.slf',
                            eps=[1e-9, 1e-9, 1e-9, 1e-9, 1e-9, \
                                 1e-9, 1e-9, 1e-9, 1e-7, 1e-9, \
                                 1e-9, 1e-4, 1e-8, 1e-4, 1e-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-9, 1e-9, 1e-9, 1e-9, 1e-9, \
                                 1e-9, 1e-9, 1e-9, 1e-9, 1e-9, \
                                 1e-9, 1e-4, 1e-8, 1e-4, 1e-9])

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines, vnv_plotbar
        from os import path
        import numpy as np
        #======================================================================
        # Getting files
        geom_res, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        #======================================================================
        # Plotting 1d slice
        res_list = [res_vnv_1_t2dres for i in range(11)]
        var_list = [
            'CHARACT_STRONG',
            'N_SCHEME', 'LIMP N_SCHEME',
            'N_SCHEME   COR1', 'N_SCHEME   COR2',
            'NERD SCHEME', 'CHARACT_WEAK',
            'PSI_SCHEME', 'LIPS_SCHEME',
            'PSI_SCHEME COR1', 'PSI_SCHEME COR2']
        res_labels = [
            'STRONG CHARACTERISTICS',
            'N', 'N LIPS', 'N PC1', 'N PC2',
            'NERD', 'WEAK CHARACTERISTICS',
            'PSI', 'PSI LIPS', 'PSI PC1', 'PSI PC2']
        res_labels_short = [
            'SCHAR',
            'N', 'N LIPS', 'N PC1', 'N PC2',
            'NERD', 'WCHAR',
            'PSI', 'PSI LIPS', 'PSI PC1', 'PSI PC2']

        vnv_plot1d_polylines(\
            var_list, res_list,
            legend_labels=res_labels,
            record=-1,
            fig_size=(8, 7),
            ref_name='ANALYTIC SOL T',
            y_label='tracer',
            x_label='x (m)',
            fig_name='img/figure_1d_T',
            markers=True,
            markevery=15)

        vnv_plot1d_polylines(\
            var_list, res_list,
            legend_labels=res_labels,
            record=32,
            fig_size=(8, 7),
            ref_name='ANALYTIC SOL T',
            y_label='tracer',
            x_label='x (m)',
            fig_name='img/figure_1d_0,5T',
            markers=True,
            markevery=15)

        #======================================================================
        # Plot 2d mesh and boundaries:
        vnv_plot2d(\
            '',
            res_vnv_1_t2dres,
            record=0,
            fig_size=(6, 6),
            fig_name="img/cone_mesh",
            plot_mesh=True)

        vnv_plot2d(\
            '',
            geom_res,
            record=0,
            fig_size=(6, 6),
            fig_name="img/cone_mesh_bnd",
            annotate_bnd=True,
            plot_mesh=True)

        #======================================================================
        # Plot 2d maps:
        vnv_plot2d(\
            'ANALYTIC SOL T',
            res_vnv_1_t2dres,
            record=-1,
            fig_size=(8, 6.5),
            fig_name='img/figure_EX',
            fig_title='EXACT',
            vmin=0.0,
            vmax=1.0,
            nv=11,
            contours=True,
            filled_contours=True)

        for idx, var in enumerate(var_list):
            vnv_plot2d(\
                var,
                res_vnv_1_t2dres,
                record=-1,
                fig_size=(8, 6.5),
                fig_name='img/figure_{}'.format(res_labels_short[idx].replace(' ', '')),
                fig_title=res_labels[idx],
                vmin=0.0,
                vmax=1.0,
                nv=11,
                contours=True,
                filled_contours=True)

        #======================================================================
        # Maximum principle and positivity:
        #
        max_vars = []
        min_vars = []

        for idx, var in enumerate(var_list):
            data = res_vnv_1_t2dres.get_data_value(var, -1)
            max_vars.append(np.max(data))
            min_vars.append(np.min(data))

        vnv_plotbar(\
            [min_vars],
            fig_size=(10, 3),
            legend_labels=['min(T)'],
            x_labels=res_labels_short,
            y_scale='linear',
            fig_title='Minimum values of tracer at $t=T$',
            fig_name="img/t2d_cone_minT",
            bar_width=.5,
            annotate=True,
            annotate_threshold=-1.e-12)

        vnv_plotbar(\
            [max_vars],
            fig_size=(10, 3),
            legend_labels=['max(T)'],
            x_labels=res_labels_short,
            y_scale='linear',
            fig_title='Maximum values of tracer at $t=T$',
            fig_name="img/t2d_cone_maxT",
            bar_width=.5,
            annotate=True)

        #======================================================================
        # Error at final time (computed from mass matrix file):
        #
        # Compute errors at final time for each case:
        errLinf_Ttf_list = []
        errL1_Ttf_list = []
        errL2_Ttf_list = []

        # Mass matrix at final time
        massm_file = self.get_study_file('vnv_1:T2DRFO')
        massm = np.genfromtxt(massm_file)

        for idx, var in enumerate(var_list):
            # Linf errors:
            errLinf_Ttf_list.append(self.compute_errors(\
                res1=res_vnv_1_t2dres,
                var1=var,
                var2='ANALYTIC SOL T',
                record=-1,
                norm='linf'))

            # L1 errors:
            errL1_Ttf_list.append(self.compute_errors(\
                res1=res_vnv_1_t2dres,
                var1=var,
                var2='ANALYTIC SOL T',
                record=-1,
                norm='l1',
                mass=massm))

            # L2 errors:
            errL2_Ttf_list.append(self.compute_errors(\
                res1=res_vnv_1_t2dres,
                var1=var,
                var2='ANALYTIC SOL T',
                record=-1,
                norm='l2',
                mass=massm))

        # Bar plots of errors at final time
        errors_T_tf = [errLinf_Ttf_list, errL1_Ttf_list, errL2_Ttf_list]

        vnv_plotbar(\
            errors_T_tf,
            fig_size=(10, 5),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels_short,
            y_scale='log',
            fig_title='Errors on tracer at $t=T$',
            fig_name="img/t2d_cone_errors_tf",
            annotate=True)

        #======================================================================
        # Error time integrals:
        #
        errLinf_Ttf_list = []
        errL1_Ttf_list = []
        errL2_Ttf_list = []

        # Mass matrix at final time
        massm_file = self.get_study_file('vnv_1:T2DRFO')
        massm = np.genfromtxt(massm_file)

        for idx, var in enumerate(var_list):
            # Compute time integrals
            time_incrs = np.zeros(res_vnv_1_t2dres.ntimestep)
            errLi = 0.
            errL1 = 0.
            errL2 = 0.

            for ite in range(res_vnv_1_t2dres.ntimestep):
                # build timestep for time integrals
                if ite == 0:
                    time_incrs[ite] = res_vnv_1_t2dres.times[ite]
                else:
                    time_incrs[ite] = res_vnv_1_t2dres.times[ite]\
                                    - res_vnv_1_t2dres.times[ite-1]

                # Linf errors:
                errLi += time_incrs[ite]*self.compute_errors(\
                    res1=res_vnv_1_t2dres,
                    var1=var,
                    var2='ANALYTIC SOL T',
                    record=ite,
                    norm='linf')

                # L1 errors:
                errL1 += time_incrs[ite]*self.compute_errors(\
                    res1=res_vnv_1_t2dres,
                    var1=var,
                    var2='ANALYTIC SOL T',
                    record=ite,
                    norm='l1',
                    mass=massm)

                # L2 errors:
                errL2 += time_incrs[ite]*self.compute_errors(\
                    res1=res_vnv_1_t2dres,
                    var1=var,
                    var2='ANALYTIC SOL T',
                    record=ite,
                    norm='l2',
                    mass=massm)

            # Errors:
            errLinf_Ttf_list.append(errLi/res_vnv_1_t2dres.times[-1])
            errL1_Ttf_list.append(errL1/res_vnv_1_t2dres.times[-1])
            errL2_Ttf_list.append(errL2/res_vnv_1_t2dres.times[-1])

        # Bar plots of errors at final time
        errors_T_tf = [errLinf_Ttf_list, errL1_Ttf_list, errL2_Ttf_list]

        vnv_plotbar(\
            errors_T_tf,
            fig_size=(10, 5),
            legend_labels=['$L_\\infty$', '$L_1$', '$L_2$'],
            x_labels=res_labels_short,
            y_scale='log',
            fig_title='Error time integrals: $\\frac{1}{T} \\int_{0}^{T} E(t) dt$',
            fig_name="img/t2d_cone_errors_timeintegrals",
            annotate=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()
        res_vnv_1_t2dres.close()
        geom_res.close()

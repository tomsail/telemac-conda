
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # rotating cone in 3D
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_cone.cas')


        # rotating cone in 3D in parallel
        cas = TelemacCas('t3d_cone.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_cone_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_cone.slf',
                            eps=[1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-3, 1.E-11, 1.E-11, 1.E-3, 1.E-11, 1.E-11, 1.E-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_cone.slf',
                            eps=[1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-3, 1.E-11, 1.E-11, 1.E-3, 1.E-11, 1.E-11, 1.E-11])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 1.E-11, 7.E-4, 1.E-11, 1.E-11, 1.E-5, 1.E-11, 1.E-11, 1.E-11])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines, vnv_plotbar
        from os import path
        import numpy as np
        #======================================================================
        # Getting files
        geom_res, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        res_vnv_1_t3dres, _ = self.get_study_res('vnv_1:T3DRES')
        res_vnv_1_t3dhyd, _ = self.get_study_res('vnv_1:T3DHYD')

        #======================================================================
        # Plotting 1d slice
        res_list = [res_vnv_1_t3dhyd for i in range(11)]
        var_list = [
            'WEAK CHARACT','SUPG','LPO',
            'NSC', 'N PR-COR OP2', 'N PR-COR OP3',
            'PSI', 'PSI PR-COR OP2', 'PSI PR-COR OP3', 'LIPS',
            'LPO_TF', 'NSC_TF']
        res_labels = [
            'WEAK CHARACTERISTICS','SUPG','LPO',
            'N', 'N PC1', 'N PC2',
            'PSI', 'PSI PC1', 'PSI PC2', 'LIPS',
            'NERD LPO', 'NERD N']
        res_labels_short = [
            'WCHAR','SUPG','LPO',
            'N', 'N PC1', 'N PC2',
            'PSI', 'PSI PC1', 'PSI PC2', 'LIPS',
            'NERD13', 'NERD14']

        vnv_plot1d_polylines(\
            var_list, res_list,
            legend_labels=res_labels,
            record=-1,
            fig_size=(8, 7),
            ref_name='ANALYTICAL SOL T',
            y_label='tracer',
            x_label='x (m)',
            fig_name='img/figure_1d_T',
            markers=True,
            markevery=15)

        vnv_plot1d_polylines(\
            var_list, res_list,
            legend_labels=res_labels,
            record=4,
            fig_size=(8, 7),
            ref_name='ANALYTICAL SOL T',
            y_label='tracer',
            x_label='x (m)',
            fig_name='img/figure_1d_halfT',
            markers=True,
            markevery=15)

        #======================================================================
        # Plot 2d mesh and boundaries:
        vnv_plot2d(\
            '',
            res_vnv_1_t3dres,
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
#            annotate_bnd=True,
            plot_mesh=True)

        # Plot vertical mesh at initial time step
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0, 10], [0, 20]],
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(12, 2),
                   fig_name='img/cone_meshV')

        #======================================================================
        # Plot 2d maps:
        vnv_plot2d(\
            'PRIVE 1',
            res_vnv_1_t3dres,
            record=-1,
            plane=res_vnv_1_t3dres.nplan-1,
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
                res_vnv_1_t3dres,
                record=-1,
                plane=res_vnv_1_t3dres.nplan-1,
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
            data = res_vnv_1_t3dres.get_data_value(var, -1)
            max_vars.append(np.max(data))
            min_vars.append(np.min(data))

        vnv_plotbar(\
            [min_vars],
            fig_size=(11, 3),
            legend_labels=['min(T)'],
            x_labels=res_labels_short,
            y_scale='linear',
            fig_title='Minimum values of tracer at $t=T$',
            fig_name="img/t3d_cone_minT",
            bar_width=.5,
            annotate=True,
            annotate_threshold=-1.e-12)

        vnv_plotbar(\
            [max_vars],
            fig_size=(11, 3),
            legend_labels=['max(T)'],
            x_labels=res_labels_short,
            y_scale='linear',
            fig_title='Maximum values of tracer at $t=T$',
            fig_name="img/t3d_cone_maxT",
            bar_width=.5,
            annotate=True)

        #======================================================================
        # Delete results
        for res in res_list:
            res.close()
        res_vnv_1_t3dres.close()
        geom_res.close()

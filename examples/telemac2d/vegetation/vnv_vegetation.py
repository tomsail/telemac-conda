
"""
Validation script for vegetation
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # vegetation T2D scalar mode
        cas = TelemacCas('t2d_veg1.cas', get_dico('telemac2d'))
        cas.set('TIME STEP', 0.5)
        cas.set('NUMBER OF TIME STEPS', 7200)
        self.add_study('vnv_veg1_seq',
                       'telemac2d',
                       't2d_veg1.cas',
                       cas=cas)
        del cas

        # vegetation T2D parallel mode
        cas = TelemacCas('t2d_veg1.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('TIME STEP', 0.5)
        cas.set('NUMBER OF TIME STEPS', 7200)
        self.add_study('vnv_veg1_par',
                       'telemac2d',
                       't2d_veg1_par.cas',
                       cas=cas)
        del cas


# veg_law 1: Lindner, 2: Jaervelae, 3: Whittaker, 4: Baptist, 5: Huthoff
# 6: van Velzen, 7: Luhar Nepf, 8: Vaestilae, 9: hybrid
        # vegetation T2D scalar mode
        for veg_law in [2, 3, 4, 5, 6, 7, 8, 9]:
            t2d_steering_file = 't2d_veg{}.cas'.format(veg_law)
            veg_file = 'friction{}.tbl'.format(veg_law)

            t2d_cas = TelemacCas('t2d_veg1.cas', get_dico('telemac2d'))
            t2d_cas.set('FRICTION DATA FILE', veg_file)

            self.add_study('vnv_veg{}_seq'.format(veg_law),
                           'telemac2d',
                           t2d_steering_file,
                           cas=t2d_cas)
            del t2d_cas

            # vegetation T2D parallel mode
            t2d_steering_file_par = 't2d_veg{}_par.cas'.format(veg_law)
            cas = TelemacCas('t2d_veg1.cas', get_dico('telemac2d'))
            cas.set('FRICTION DATA FILE', veg_file)
            cas.set('PARALLEL PROCESSORS', 4)
            self.add_study('vnv_veg{}_par'.format(veg_law),
                           'telemac2d',
                           t2d_steering_file_par,
                           cas=cas)
            del cas


    def _check_results(self):
        """
        Post-treatment processes
        """


        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg1_seq:T2DRES',
                            'f2d_veg1.slf',
                            eps=[5e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg1_par:T2DRES',
                            'f2d_veg1.slf',
                            eps=[3e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg1_seq:T2DRES',
                            'vnv_veg1_par:T2DRES',
                            eps=[5e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg2_seq:T2DRES',
                            'f2d_veg2.slf',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg2_par:T2DRES',
                            'f2d_veg2.slf',
                            eps=[2e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg2_seq:T2DRES',
                            'vnv_veg2_par:T2DRES',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg3_seq:T2DRES',
                            'f2d_veg3.slf',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg3_par:T2DRES',
                            'f2d_veg3.slf',
                            eps=[3e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg3_seq:T2DRES',
                            'vnv_veg3_par:T2DRES',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg4_seq:T2DRES',
                            'f2d_veg4.slf',
                            eps=[3e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg4_par:T2DRES',
                            'f2d_veg4.slf',
                            eps=[5e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg4_seq:T2DRES',
                            'vnv_veg4_par:T2DRES',
                            eps=[5e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg5_seq:T2DRES',
                            'f2d_veg5.slf',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg5_par:T2DRES',
                            'f2d_veg5.slf',
                            eps=[4.1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg5_seq:T2DRES',
                            'vnv_veg5_par:T2DRES',
                            eps=[3e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg6_seq:T2DRES',
                            'f2d_veg6.slf',
                            eps=[3e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg6_par:T2DRES',
                            'f2d_veg6.slf',
                            eps=[4.1e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg6_seq:T2DRES',
                            'vnv_veg6_par:T2DRES',
                            eps=[3e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg7_seq:T2DRES',
                            'f2d_veg7.slf',
                            eps=[3e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg7_par:T2DRES',
                            'f2d_veg7.slf',
                            eps=[5e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg7_seq:T2DRES',
                            'vnv_veg7_par:T2DRES',
                            eps=[5e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg8_seq:T2DRES',
                            'f2d_veg8.slf',
                            eps=[3e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg8_par:T2DRES',
                            'f2d_veg8.slf',
                            eps=[3e-3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg8_seq:T2DRES',
                            'vnv_veg8_par:T2DRES',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg9_seq:T2DRES',
                            'f2d_veg9.slf',
                            eps=[2e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_veg9_par:T2DRES',
                            'f2d_veg9.slf',
                            eps=[3e-3])

       # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_veg9_seq:T2DRES',
                            'vnv_veg9_par:T2DRES',
                            eps=[3e-3])

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
        geom, _ = self.get_study_res('vnv_veg1_seq:T2DGEO', load_bnd=True)
        res, _ = self.get_study_res('vnv_veg1_seq:T2DRES')

        # Load all results as a list:
        res_list, res_labels = self.get_study_res(module='T2D')
#        print ("res_list", res_list)
        print ("res_labels", res_labels)
        # Plot bathy longitudinal section:
        vnv_plot1d_polylines(\
            'BOTTOM',
            res,
            '',
            fig_size=(8, 2),
            record=0,
            fig_name='img/vegetation_bathy',
            plot_bottom=True)

        # Plot bathy cross section:
        vnv_plot1d_polylines(\
            'BOTTOM',
            res,
            poly=[[30, 0], [30, 5]],
            fig_size=(8, 2),
            record=0,
            fig_name='img/vegetation_bathy_cross',
            ylim=[0., 0.15],
            plot_bottom=True)


        # Plot mesh
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/vegetation_mesh0',
            annotate_bnd=False,
            plot_mesh=True)

        # Plot mesh with boundaries
        vnv_plot2d(\
            '',
            geom,
            fig_size=(10, 2),
            fig_name='img/vegetation_mesh',
            annotate_bnd=True,
            plot_mesh=True)

        #----------------------------------------------------------------------
        # Comparison of free surface (1D slice):
        vnv_plot1d_polylines(\
            'FREE SURFACE',
            res_list,
            res_labels,
            record=-1,
            fig_size=(8, 5),
            fig_name='img/vegetation_free_surface',
            markers=True,
            markevery=15,
            y_label='Free surface (m)',
            plot_bottom=False)

        # Comparison of free CF (1D slice):
        vnv_plot1d_polylines(\
            'CF',
            res_list,
            res_labels,
            record=-1,
            fig_size=(8, 5),
            fig_name='img/vegetation_cf',
            markers=True,
            markevery=15,
            ylim=[0., 0.4],
            y_label='CF',
            plot_bottom=False)

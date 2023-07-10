
"""
Validation script for fetch_lim
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot_vnv import vnv_plot2d

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['tomawac']
        # For vnv_6p on eole debug
        self.walltime = '14:00:00'

    def _pre(self):
        """
        Defining the studies
        """
        # fetch_lim scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_fetch_lim_id.cas')

        # fetch_lim parallel mode
        cas = TelemacCas('tom_fetch_lim_id.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_fetch_lim_id_par.cas',
                       cas=cas)
        del cas

        # fetch_lim scalar mode
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_fetch_lim_fd.cas')

        # fetch_lim parallel mode
        cas = TelemacCas('tom_fetch_lim_fd.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'tomawac',
                       'tom_fetch_lim_fd_par.cas',
                       cas=cas)
        del cas


        # fetch_lim scalar mode
        self.add_study('vnv_5',
                       'tomawac',
                       'tom_test4.cas')


        # fetch_lim parallel mode
        cas = TelemacCas('tom_test4.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_5p',
                       'tomawac',
                       'tom_test4_par.cas',
                       cas=cas)
        del cas

        # fetch_lim scalar mode
        self.add_study('vnv_6',
                       'tomawac',
                       'tom_test6.cas')


        # fetch_lim parallel mode
        cas = TelemacCas('tom_test6.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6p',
                       'tomawac',
                       'tom_test6_par.cas',
                       cas=cas)

        del cas

        # fetch_lim scalar mode
        self.add_study('vnv_7',
                       'tomawac',
                       'tom_test6bis.cas')

        # fetch_lim parallel mode
        cas = TelemacCas('tom_test6bis.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_7p',
                       'tomawac',
                       'tom_test6bis_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_fetch_lim_id.slf',
                            eps=[5e-7, 2e-5, 8e-6, 1e-10, 1e-10, 5e-8, 1e-10,\
                                 5e-8, 1e-10, 1e-6, 1e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_fetch_lim_id.slf',
                            eps=[5e-7, 2e-5, 8e-6, 1e-10, 1e-10, 5e-8, 1e-10,\
                                 5e-8, 1e-10, 5e-7, 1e-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[0.03, 0.2, 0.3, 1e-10, 1e-10, 0.005, 0.009,\
                                 0.002, 1e-10, 0.07, 0.05])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_fetch_lim_fd.slf',
                            eps=[5e-7, 8e-6, 2e-6, 1e-10, 1e-10, 1e-10, 2e-8,\
                                 1e-10, 2e-8, 1e-10, 5e-7, 5e-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'fom_fetch_lim_fd.slf',
                            eps=[0.006, 0.09, 0.2, 1e-10, 1e-10, 1e-10, 0.002,\
                                 0.02, 0.0003, 1e-10, 0.02, 5e-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[0.006, 0.09, 0.2, 1e-10, 1e-10, 1e-10, 0.002,\
                                 0.02, 0.0003, 1e-10, 0.02, 0.008])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:WACRES',
                            'fom_fetch_lim_test4.slf',
                            eps=[0.05, 0.61, 0.7, 1e-10, 1e-10, 0.006, 0.012,\
                                 0.002, 1e-10, 0.053, 0.07, 1e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:WACRES',
                            'vnv_5p:WACRES',
                            eps=[0.04, 0.3, 0.3, 1e-10, 1e-10, 0.004, 0.02,\
                                 0.002, 1e-10, 0.03, 0.06, 4e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:WACRES',
                            'fom_fetch_lim_test6.slf',
                            eps=[1e-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_6:WACRES',
                            'vnv_6p:WACRES',
                            eps=[0.006, 0.09, 0.2, 1e-10, 1e-10, 0.002, 0.02,\
                                 0.0003, 1e-10, 0.02, 0.008])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:WACRES',
                            'fom_fetch_lim_test6b.slf',
                            eps=[1e-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:WACRES',
                            'vnv_7p:WACRES',
                            eps=[0.006, 0.09, 0.2, 1e-10, 1e-10, 0.002, 0.02,\
                                 0.0003, 1e-10, 0.02, 0.008])

    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_5_wacres = self.get_study_file('vnv_5:WACRES')
        res_vnv_5_wacres = TelemacFile(vnv_5_wacres)
        vnv_7_wacres = self.get_study_file('vnv_7:WACRES')
        res_vnv_7_wacres = TelemacFile(vnv_7_wacres)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)
        vnv_6_wacres = self.get_study_file('vnv_6:WACRES')
        res_vnv_6_wacres = TelemacFile(vnv_6_wacres)
        vnv_3_wacgeo = self.get_study_file('vnv_3:WACGEO')
        res_vnv_3_wacgeo = TelemacFile(vnv_3_wacgeo)
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_3_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='Purples',
                   fig_size=(12, 7),
                   fig_name='img/resultsinfinite')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='Purples',
                   fig_size=(12, 7),
                   fig_name='img/resultsfinite')


        cmap = 'bone'
        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_5_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/resultstest4')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_6_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/resultstest6')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_7_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/resultstest6b')

        # Closing files
        res_vnv_5_wacres.close()
        res_vnv_7_wacres.close()
        res_vnv_3_wacres.close()
        res_vnv_6_wacres.close()
        res_vnv_3_wacgeo.close()
        res_vnv_1_wacres.close()

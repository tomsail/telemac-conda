
"""
Validation script for triplet
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
        self.rank = 2
        self.tags = ['tomawac']

    def _pre(self):
        """
        Defining the studies
        """
        # Triplets scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_triplet1.cas')

        # Triplets parallel mode
        cas = TelemacCas('tom_triplet1.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_triplet1_par.cas',
                       cas=cas)
        del cas

        # Triplets scalar mode
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_triplet2.cas')

        # Triplets parallel mode
        cas = TelemacCas('tom_triplet2.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'tomawac',
                       'tom_triplet2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison between sequential TRIA 1  and parallel TRIA1 run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-9, 1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison between sequential TRIA 1  and reference.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_triplet1.slf',
                            eps=[1e-9, 1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison between sequential TRIA 2  and parallel TRIA2 run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-9, 1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison between sequential TRIA 2  and reference.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_triplet2.slf',
                            eps=[1e-9, 1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison between sequential TRIA 1  and sequential TRIA2 .
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_3:WACRES',
                            eps=[1])

    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('',
                   res_vnv_1_wacres,
                   record=-1,
                   plot_mesh=True,
                   aspect_ratio='equal',
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/results')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/results2')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_3_wacres.close()

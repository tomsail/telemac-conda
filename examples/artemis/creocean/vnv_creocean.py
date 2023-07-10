
"""
Validation script for creocean
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
        self.rank = 1
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # creocean boundaries in cli
        cas = TelemacCas('art_creocean.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_1',
                       'artemis',
                       'art_creocean_par.cas',
                       cas=cas)

        del cas


        # creocean boundaries in BORH
        cas = TelemacCas('art_creocean_2.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_creocean_2_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_creocean.9366.slf',
                            eps=[1.e-8, 1.e-8, 360., 1.e-8])

        # Comparison with boundary conditions in cli file.
        self.check_epsilons('vnv_2:ARTRES',
                            'vnv_1:ARTRES',
                            eps=[1.e-8, 1.e-8, 2.e-7, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)

        #Plotting mesh
        vnv_plot2d(\
                'BOTTOM',
                res_vnv_1_artres,
                plot_mesh=True,
                fig_size=(12, 8),
                fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d(\
                'BOTTOM',
                res_vnv_1_artres,
                record=0,
                filled_contours=True,
                fig_size=(12, 8),
                fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d(\
                 'WAVE HEIGHT',
                 res_vnv_1_artres,
                 record=0,
                 filled_contours=True,
                 fig_size=(12, 8),
                 fig_name='img/WaveHeight')


        # Plotting QB at 0
        vnv_plot2d(\
                 'QB',
                 res_vnv_1_artres,
                 record=0,
                 filled_contours=True,
                 fig_size=(12, 8),
                 fig_name='img/Breaking')

        res_vnv_1_artres.close()

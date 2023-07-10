
"""
Validation script for kostense
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

        # KOSTENSE VORTEX CURRENT CASE
        self.add_study('vnv_1',
                       'artemis',
                       'art_kostense.cas')


        # KOSTENSE VORTEX CURRENT CASE, parallel mode
        cas = TelemacCas('art_kostense.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_kostense_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_kostense.slf',
                            eps=[1.e-5, 1.e-5, 1.e-5, 1.e-8, 1.e-5, 1.e-5, 1.e-5, 1.e-5, 1.e-5, 0.0002, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_kostense.slf',
                            eps=[0.002, 10.0, 0.001, 1.e-8, 0.1, 0.1, 1.e-4, 0.01, 0.01, 20.0, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[0.002, 10.0, 0.001, 1.e-8, 0.1, 0.1, 1.e-4, 0.01, 0.01, 20.0, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)

        #Plotting mesh
        vnv_plot2d(\
                'BOTTOM',
                res_vnv_1_artgeo,
                plot_mesh=True,
                fig_size=(8, 8),
                fig_name='img/Mesh')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d(\
                 'WAVE HEIGHT',
                 res_vnv_1_artres,
                 record=0,
                 filled_contours=True,
                 fig_size=(8, 8),
                 fig_name='img/WaveHeight')

        res_vnv_1_artres.close()
        res_vnv_1_artgeo.close()

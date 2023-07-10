
"""
Validation script for reso
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

        # Resonance in a flume
        self.add_study('vnv_1',
                       'artemis',
                       'art_reso.cas')


        # Resonance in a flume, parallel mode
        cas = TelemacCas('art_reso.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_reso_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_reso.slf',
                            eps=[1.e-12])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_reso.slf',
                            eps=[1.e-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-12])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot_mesh2d, plot_var
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)

        #Plotting mesh
        vnv_plot2d(\
                'MAILLAGE',
                res_vnv_1_artgeo,
                plot_mesh=True,
                fig_size=(7, 10),
                fig_name='img/Mesh')


        # Plotting WAVE HEIGHT at 85
        vnv_plot2d(\
                 'WAVE HEIGHT',
                 res_vnv_1_artres,
                 record=85,
                 filled_contours=True,
                 fig_size=(7, 10),
                 fig_name='img/Wave_height')

        res_vnv_1_artgeo.close()
        res_vnv_1_artres.close()


"""
Validation script for culvert
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # culvert scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_culvert.cas')


        # culvert parallel mode
        cas = TelemacCas('t3d_culvert.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_culvert_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_culvert.slf',
                            eps=[7.E-4, 1.E-6, 1.E-6, 1.E-5, 2.8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_culvert.slf',
                            eps=[6.E-4, 1.E-6, 1.E-6, 1.E-5, 4.2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[7.E-4, 1.E-6, 1.E-6, 1.E-5, 6.3])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_1_t3dgeo = self.get_study_file('vnv_1:T3DGEO')
        res_vnv_1_t3dgeo = TelemacFile(vnv_1_t3dgeo)

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        res_vnv_1_t3dgeo.close()

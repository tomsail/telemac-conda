
"""
Validation script for Next_Comput
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

        # Next_Comput scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_next_comput.cas')


        # Next_Comput parallel mode
        cas = TelemacCas('tom_next_comput.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_next_comput_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'ref.slf',
                            eps=[1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'ref.slf',
                            eps=[1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-9, 361, 1e-9, 1e-9, 1e-9, 1e-9, 1e-9])


    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)

        # Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacres,
                   record=-1,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')
        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/results')

        # Closing files
        res_vnv_1_wacres.close()

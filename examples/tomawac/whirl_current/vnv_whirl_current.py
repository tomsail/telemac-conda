
"""
Validation script for whirl_current
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
        self.rank = 1
        self.tags = ['tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # whirl_current scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_whirl.cas')

        # whirl_current parallel mode
        cas = TelemacCas('tom_whirl.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_whirl_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_whirl.slf',
                            eps=[1e-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_whirl.slf',
                            eps=[1e-6])

        # BLA BLA
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-6])


    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   aspect_ratio='equal',
                   fig_name='img/mesh')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
#                   cmap_name='viridis',
                   aspect_ratio='equal',
                   fig_name='img/results')

        # Closing files
        res_vnv_1_wacgeo.close()
        res_vnv_1_wacres.close()

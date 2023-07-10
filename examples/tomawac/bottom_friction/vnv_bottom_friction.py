"""
Validation script for bottom_friction
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

        # bottom_friction scalar mode
        self.add_study('vnv_5',
                       'tomawac',
                       'tom_friction.cas')


        # bottom_friction parallel mode
        cas = TelemacCas('tom_friction.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'tomawac',
                       'tom_friction_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:WACRES',
                            'fom_frot_T_d5-f0.1-h1.slf',
                            eps=[1e-5, 0.1, 1e-4, 1e-4, 1e-4, 1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:WACRES',
                            'fom_frot_T_d5-f0.1-h1.slf',
                            eps=[1e-5, 0.1, 1e-4, 1e-4, 1e-4, 1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:WACRES',
                            'vnv_6:WACRES',
                            eps=[1e-4])


    def _post(self):
        """
        Post-treatment processes
        """
                # Getting files
        vnv_5_wacres = self.get_study_file('vnv_5:WACRES')
        res_vnv_5_wacres = TelemacFile(vnv_5_wacres)
        vnv_5_wacgeo = self.get_study_file('vnv_5:WACGEO')
        res_vnv_5_wacgeo = TelemacFile(vnv_5_wacgeo)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_5_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting MEAN FREQ FM01 at -1
        vnv_plot2d('MEAN FREQ FM01',
                   res_vnv_5_wacres,
                   record=-1,
                   cbar_label='Mean Frequency Fm01 (Hz)',
                   filled_contours=True,
                   cmap_name="viridis",
                   fig_size=(9, 5),
                   fig_name='img/results')

        # Closing files
        res_vnv_5_wacres.close()
        res_vnv_5_wacgeo.close()

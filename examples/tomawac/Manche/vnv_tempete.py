
"""
Validation script for tempete
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

    def _pre(self):
        """
        Defining the studies
        """

        # Storm : calculted geometry
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_manche.cas')

        # Storm : calculted geometry
        cas = TelemacCas('tom_manche.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_1p',
                       'tomawac',
                       'tom_manche_par.cas',
                       cas=cas)
        del cas


        # Storm : read geometry (simple in a file)
        self.add_study('vnv_2',
                       'tomawac',
                       'tom_manchelim.cas')

        # Storm : read geometry (simple in a file)
        cas = TelemacCas('tom_manchelim.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2p',
                       'tomawac',
                       'tom_manchelim_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison between results from calculated geometry and reference value.
        self.check_epsilons('vnv_1:WACRES',
                            'ref_tom_calc.slf',
                            eps=[0.02, 19, 1e-8, 1e-8, 1e-8, 0.008, 0.01, 0.03, 1.E-7])

        # Comparison between scalar and parallel.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_1p:WACRES',
                            eps=[1e-7])

        # Comparison between results from read geometry and reference run.
        self.check_epsilons('vnv_2:WACRES',
                            'ref_tom_spher.slf',
                            eps=[0.05, 0.2, 1.E-7, 1.E-7, 1.E-7, 0.009, 0.009, 1.E-7, 1.E-5])

        # Comparison between  scalar and parallel.
        self.check_epsilons('vnv_2:WACRES',
                            'vnv_2p:WACRES',
                            eps=[1e-7])

    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_2_wacres = self.get_study_file('vnv_2:WACRES')
        res_vnv_2_wacres = TelemacFile(vnv_2_wacres)
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_2_wacgeo = self.get_study_file('vnv_2:WACGEO')
        res_vnv_2_wacgeo = TelemacFile(vnv_2_wacgeo)
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)
        res_v1p3 = TelemacFile('r2d.V1P3.slf')
        ref = TelemacFile('ref_tom_calc.slf')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')

        # Plotting FOND at -1
        vnv_plot2d('FOND',
                   res_vnv_2_wacgeo,
                   record=-1,
                   cbar_label='Bathymetry (m)',
                   filled_contours=True,
                   cmap_name='Blues',
                   fig_size=(12, 7),
                   fig_name='img/bathy')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   fig_name='img/results')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_2_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   fig_name='img/results2')


        # Plotting HAUTEUR_HM0 at -1
        vnv_plot2d('HAUTEUR_HM0',
                   res_v1p3,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   fig_name='img/resultsV1P3')


        # Plotting MEAN DIRECTION at -1
        vnv_plot2d('MEAN DIRECTION',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='direction (°)',
                   filled_contours=True,
                   vmin=0,
                   vmax=360,
                   nv=13,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   fig_name='img/direction')


        # Plotting MEAN DIRECTION at -1
        vnv_plot2d('MEAN DIRECTION',
                   ref,
                   record=-1,
                   cbar_label='direction (°)',
                   filled_contours=True,
                   cmap_name='winter',
                   vmin=0,
                   vmax=360,
                   nv=13,
                   fig_size=(12, 7),
                   fig_name='img/directionref')

        # Plotting WHITE CAPPING RATE at -1
        vnv_plot2d('WHITE CAPING',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   fig_name='img/whitecaping')

        # Closing files
        res_vnv_2_wacres.close()
        res_vnv_1_wacres.close()
        res_vnv_2_wacgeo.close()
        res_vnv_1_wacgeo.close()
        res_v1p3.close()
        ref.close()


"""
Validation script for calais
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

        # Calais scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_calais.cas')

        # Calais parallel mode
        cas = TelemacCas('tom_calais.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_calais_par.cas',
                       cas=cas)
        del cas

        # Calais parallel mode
        cas = TelemacCas('tom_calais_concat.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_concat',
                       'tomawac',
                       'tom_calais_concat_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'ref_tom_calais.slf',
                            eps=[1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'ref_tom_calais.slf',
                            eps=[1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2:WACRES',
                            'vnv_concat:WACRES',
                            eps=[1e-12])


    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)
        res_v5p8 = TelemacFile('ref_tom_calais_v5p8.slf')
        res_v6p0 = TelemacFile('ref_tom_calais_v6p0.slf')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        cmap = 'Blues'
        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/hm0')


        # Plotting HAUTEUR HM0 at -1
        vnv_plot2d('HAUTEUR HM0',
                   res_v5p8,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/hm0v5p8')


        # Plotting HAUTEUR HM0 at -1
        vnv_plot2d('HAUTEUR HM0',
                   res_v6p0,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/hm0v6p0')


        # Plotting MEAN DIRECTION at -1
        vnv_plot2d('MEAN DIRECTION',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Mean direction (°)',
                   nv=13,
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/direction')


        # Plotting DIRECTION MOY at -1
        vnv_plot2d('DIRECTION MOY',
                   res_v6p0,
                   record=-1,
                   cbar_label='Mean direction (°)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/directionv6p0')


        # Plotting WAVE POWER at -1
        vnv_plot2d('WAVE POWER',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/power')


        # Plotting PUISSANCE HOULE at -1
        vnv_plot2d('PUISSANCE HOULE',
                   res_v5p8,
                   record=-1,
                   cbar_label='Power (W)',
                   filled_contours=True,
                   cmap_name=cmap,
                   fig_size=(12, 7),
                   fig_name='img/powerv5p8')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_1_wacgeo.close()
        res_v5p8.close()
        res_v6p0.close()

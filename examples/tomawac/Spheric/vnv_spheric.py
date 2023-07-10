
"""
Validation script for spheric
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
        # TestBaj scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_spheric.cas')

        # TestBaj parallel mode
        cas = TelemacCas('tom_spheric.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_spheric_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_spherique.slf',
                            eps=[0.02, 0.4, 0.08, 1e-12, 1e-12, 0.03, 0.03, 0.2])

        # Compariso between scalar and parrallel.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-12, 1e-12, 1e-12, 1e-12, 1e-12, 1e-12, 1e-12, 1e-12])


    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        res_vent = TelemacFile('vent.slf')
        res_courant = TelemacFile('courant.slf')

         # Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacres,
                   record=-1,
                   plot_mesh=True,
                   fig_size=(12, 7),
                   x_label='longitude',
                   y_label='lattitude',
                   fig_name='img/mesh')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   x_label='longitude',
                   y_label='lattitude',
                   fig_name='img/hm0')


        # Plotting VENT_X at 0
        vnv_plot2d('VENT_X',
                   res_vent,
                   record=0,
                   cbar_label='Wind along X (m/s)',
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   x_label='longitude',
                   y_label='lattitude',
                   fig_name='img/vent')


        # Plotting VITESSE U at 0
        vnv_plot2d('VITESSE U',
                   res_courant,
                   record=0,
                   cbar_label='Velocity U (m/s)',
                   filled_contours=True,
                   cmap_name='winter',
                   fig_size=(12, 7),
                   x_label='longitude',
                   y_label='lattitude',
                   fig_name='img/courant')

        # Closing files
        res_vnv_1_wacres.close()
        res_vent.close()
        res_courant.close()


"""
Validation script for reflection
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot_vnv import vnv_plot2d
from postel.plot_actions import plot1d
import matplotlib.pyplot as plt

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """
    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['tomawac', 'med']

    def _pre(self):
        """
        Defining the studies
        """
        # shoal scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_refl.cas')

        # shoal parallel mode
        cas = TelemacCas('tom_refl.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_refl_par.cas',
                       cas=cas)
        del cas

        # shoal scalar mode
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_refl2.cas')

        # shoal parallel mode
        cas = TelemacCas('tom_refl2.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'tomawac',
                       'tom_refl2_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_refl.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_refl.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_refl2.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'fom_refl2.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-9])

    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        bnd_wacgeo = self.get_study_file('vnv_1:WACCLI')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo, bnd_file=bnd_wacgeo)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)
        vnv_3_wacgeo = self.get_study_file('vnv_3:WACGEO')
        bnd_wacgeo = self.get_study_file('vnv_3:WACCLI')
        res_vnv_3_wacgeo = TelemacFile(vnv_3_wacgeo, bnd_file=bnd_wacgeo)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   aspect_ratio='equal',
                   fig_size=(10, 3),
                   fig_name='img/mesh')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_3_wacgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   aspect_ratio='equal',
                   fig_name='img/mesh2')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(10, 3),
                   aspect_ratio='equal',
                   cbar_label='WAVE HEIGHT HM0 (m)',
                   fig_name='img/results')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   filled_contours=True,
                   aspect_ratio='equal',
                   cbar_label='WAVE HEIGHT HM0 (m)',
                   fig_name='img/results2')

        time = res_vnv_1_wacres.times.T
        _, axe = plt.subplots()
        point = [[0,0]]
        plat = res_vnv_1_wacres.get_timeseries_on_points('WAVE HEIGHT HM0',point)
        plot1d(axe, time, plat[0,:],
                   x_label='time(s)',
                   y_label='WAVE HEIGHT HM0',
                   plot_label='canal pt [0,0] ',
                   marker='+')

        point = [[0,200]]
        tourne = res_vnv_3_wacres.get_timeseries_on_points('WAVE HEIGHT HM0',point)
        plot1d(axe, time, tourne[0,:],
                   x_label='time(s)',
                   y_label='WAVE HEIGHT HM0',
                   plot_label='rotated canal pt [0,200]')
        axe.legend()
        fig_name = 'img/waveheight'
        plt.savefig(fig_name)
        plt.close('all')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_1_wacgeo.close()

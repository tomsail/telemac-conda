"""
Validation script for angular_spread
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from postel.plot1d import plot1d
from postel.plot_vnv import vnv_plot2d
import matplotlib.pyplot as plt

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Thompson
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
        # shoal scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_angularspread.cas')

        # shoal parallel mode
        cas = TelemacCas('tom_angularspread.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_angularspread_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison between sequential reference
        self.check_epsilons('fom_ang.slf',
                            'vnv_2:WACRES',
                            eps=[1e-8])

        # Comparison between sequential and spectre
        self.check_epsilons('fom_ang.spe',
                            'vnv_2:WACLEO',
                            eps=[1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacgeo = self.get_study_file('vnv_1:WACGEO')
        res_vnv_1_wacgeo = TelemacFile(vnv_1_wacgeo)
        vnv_1_wacleo = self.get_study_file('vnv_1:WACLEO')
        spe = TelemacFile(vnv_1_wacleo)
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_wacgeo,
                   plot_mesh=True,
                   cbar_label='Bathymetry (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/mesh')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   cbar_label='Wave height (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/hm0')


        points = spe.get_list_spectrum_points()

        record = -1

        # Plotting F01 PT2D000101 at -1
        _, axe = plt.subplots(figsize=(7, 7))

        # Adding plot of spectrum for each point
        for point in points:
            # Getting list of frequencies and spectrum value
            freq, spectrum = spe.get_spectrum(point, record)
            # Plotting it
            plot1d(axe, freq, spectrum,
                   plot_label='point {:04d}'.format(point),
                   x_label='Frequencies',
                   y_label='Spectrum')

        axe.legend()

        fig_name = "img/spectrum"
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res_vnv_1_wacgeo.close()
        res_vnv_1_wacres.close()
        spe.close()

        res_vnv_1_wacgeo.close()
        res_vnv_1_wacres.close()
        spe.close()

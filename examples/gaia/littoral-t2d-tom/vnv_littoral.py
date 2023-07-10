
"""
Validation script for littoral
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
        self.tags = ['telemac2d', 'tomawac', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral T2D+TOM+GAI scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_littoral.cas')


        # littoral T2D+TOM+GAI scalar mode
        cas = TelemacCas('t2d_littoral.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_littoral_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_littoral.slf',
                            eps=[1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9, 2.e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_littoral.slf',
                            eps=[1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9, 2.e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_littoral.slf',
                            eps=[1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_littoral.slf',
                            eps=[1.E-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'tom_ref_littoral.slf',
                            eps=[1.E-9, 361., 1.E-9, 1.E-9, 1.E-9, 1.E-9, 2.E-9, 4.E-9, 1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'tom_ref_littoral.slf',
                            eps=[1.E-9, 361., 1.E-9, 1.E-9, 1.E-9, 1.E-9, 2.E-9, 4.E-9, 1.E-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1.E-9, 361., 1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9, 1.E-9])


    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_gaires = self.get_study_file('vnv_1:GAIRES')
        res_vnv_1_gaires = TelemacFile(vnv_1_gaires)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(9, 4),
                   fig_name='img/fond')


        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   cmap_name="autumn",
                   fig_size=(12, 7),
                   fig_name='img/resultsT2D')


        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_1_gaires,
                   record=-1,
                   filled_contours=True,
                   cmap_name="copper",
                   fig_size=(12, 7),
                   fig_name='img/resultsGAI')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   cmap_name="winter",
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_1_gaires.close()
        res_vnv_1_t2dres.close()
        res_vnv_1_t2dgeo.close()

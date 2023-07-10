
"""
Validation script for 3Dcoupling
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
        self.tags = ['telemac3d', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral3D T3D+TOM scalar mode 2D coupling
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_littoral.cas')


        # littoral3D T3D+TOM parallel mode 2D coupling
        cas = TelemacCas('t3d_littoral.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_littoral_par.cas',
                       cas=cas)

        del cas


        # littoral3D T3D+TOM scalar mode 3D coupling
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_3Dcoupling.cas')


        # littoral3D T3D+TOM parallel mode 3D coupling
        cas = TelemacCas('t3d_3Dcoupling.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_3Dcoupling_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_littoral.slf',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1e-8, 1e-8, 1e-8, 1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_littoral3D.slf',
                            eps=[1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_littoral3D.slf',
                            eps=[1e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-8])

        # 3D COUPLING Comparison with a reference for T3D.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_3Dcoupling.slf',
                            eps=[1e-8, 2e-7, 1e-7, 1e-7, 1e-8, 1e-8, 1e-8])

        # 3D COUPLING Comparison with a reference for WAC
        self.check_epsilons('vnv_3:WACRES',
                            'fom_3Dcoupling.slf',
                            eps=[1e-8])

        # 3D COUPLING Comparison between sequential and parallel for T3D.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[2e-7])

        # NEW COUPLING Comparison between sequential and parallel for WAC.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-8])



    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)


         # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   plot_mesh=True,
                   record=-1,
                   cbar_label='Wave height hm0 (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/hm0_3dlittoral')


        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   plot_mesh=True,
                   record=-1,
                   cbar_label='Wave height hm0 (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/hm0_3dcoupling')


        res_vnv_1_wacres.close()
        res_vnv_1_t3dres.close()
        res_vnv_3_t3dres.close()
        res_vnv_3_wacres.close()

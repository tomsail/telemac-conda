
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
        self.rank = 0
        self.tags = ['telemac2d', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral T2D+TOM+SIS scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_same.cas')

        # littoral T2D+TOM+SIS parallel mode
        cas = TelemacCas('t2d_same.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_same_par.cas',
                       cas=cas)
        del cas

        # littoral T2D+TOM+SIS scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_different.cas')

        # littoral T2D+TOM+SIS parallel mode
        cas = TelemacCas('t2d_different.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_different_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_same.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_same.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_same.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_same.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_different.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_different.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_different.slf',
                            eps=[1e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'fom_different.slf',
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
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)
        vnv_3_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv_3_t2dres)

        # Plotting WAVE HEIGHT HM0 at -1 cas same
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   plot_mesh=True,
                   record=-1,
                   cbar_label='Wave height hm0 (m)',
                   filled_contours=True,
                   xlim=[-200, 1200],
                   fig_size=(12, 7),
                   fig_name='img/hm0_same')

        # Plotting WAVE HEIGHT HM0 at -1 cas same
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   plot_mesh=True,
                   record=-1,
                   cbar_label='Wave height hm0 (m)',
                   filled_contours=True,
                   xlim=[-200, 1200],
                   fig_size=(12, 7),
                   fig_name='img/hm0_different')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_1_t2dres.close()
        res_vnv_3_wacres.close()
        res_vnv_3_t2dres.close()

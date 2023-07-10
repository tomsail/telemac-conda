
"""
Validation script for solit
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # solit scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_solit_p2.cas')


        # solit parallel mode
        cas = TelemacCas('t3d_solit_p2.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_solit_p2_par.cas',
                       cas=cas)

        del cas


        # solit scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_solit_p3.cas')


        # solit parallel mode
        cas = TelemacCas('t3d_solit_p3.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_solit_p3_par.cas',
                       cas=cas)

        del cas


        # solit scalar mode
        self.add_study('vnv_5',
                       'telemac3d',
                       't3d_solit_p4.cas')


        # solit parallel mode
        cas = TelemacCas('t3d_solit_p4.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac3d',
                       't3d_solit_p4_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_solit_p2.slf',
                            eps=[1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_solit_p2.slf',
                            eps=[1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_solit_p3.slf',
                            eps=[1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_solit_p3.slf',
                            eps=[1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T3DRES',
                            'f3d_solit_p4.slf',
                            eps=[1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T3DRES',
                            'f3d_solit_p4.slf',
                            eps=[1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T3DRES',
                            'vnv_6:T3DRES',
                            eps=[1.E-10])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
                # Getting files
        vnv_5_t3dres = self.get_study_file('vnv_5:T3DRES')
        res_vnv_5_t3dres = TelemacFile(vnv_5_t3dres)
        vnv_3_t3dres = self.get_study_file('vnv_3:T3DRES')
        res_vnv_3_t3dres = TelemacFile(vnv_3_t3dres)
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)

        # Plotting vertical split
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[0, 3], [600, 3]],
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   xlim=(1, 600),
                   ylim=(-10, 0),
                   fig_size=(20, 4),
                   fig_name='img/solit_2planes')

        # Plotting vertical split
        vnv_plot2d('VELOCITY',
                   res_vnv_3_t3dres,
                   poly=[[0, 3], [600, 3]],
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   xlim=(1, 600),
                   ylim=(-10, 0),
                   fig_size=(20, 4),
                   fig_name='img/solit_3planes')

        # Plotting vertical split
        vnv_plot2d('VELOCITY',
                   res_vnv_5_t3dres,
                   poly=[[0, 3], [600, 3]],
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   xlim=(1, 600),
                   ylim=(-10, 0),
                   fig_size=(20, 4),
                   fig_name='img/solit_4planes')

        res_vnv_1_t3dres.close()
        res_vnv_3_t3dres.close()
        res_vnv_5_t3dres.close()

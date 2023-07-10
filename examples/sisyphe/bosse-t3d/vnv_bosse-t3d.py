
"""
Validation script for bosse-t3d
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac3d', 'sisyphe']

    def _pre(self):
        """
        Defining the studies
        """

        # bosse-t3d scalar mode T3D+SIS
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_bosse-t3d.cas')


        # bosse-t3d T3D+SIS parallel mode
        cas = TelemacCas('t3d_bosse-t3d.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_bosse-t3d_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:SISRES',
                            'fis_bosse-t3d.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:SISRES',
                            'fis_bosse-t3d.slf',
                            eps=[1.E-6, 1.E-6, 1.E-6, 1.E-6, 1.E-6, 1.E-5, 1.E-10, 1.E-6, 1.E-8, 1.E-10])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:SISRES',
                            'vnv_2:SISRES',
                            eps=[1.E-6, 1.E-6, 1.E-6, 1.E-6, 1.E-6, 1.E-5, 1.E-10, 1.E-6, 1.E-8, 1.E-10])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_bosse-t3d.slf',
                            eps=[1.E-7, 1.E-6, 1.E-7, 1.E-6, 1.E-7, 1.E-7, 1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_bosse-t3d.slf',
                            eps=[1.E-6, 1.E-5, 1.E-6, 1.E-5, 1.E-7, 1.E-7, 1.E-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6, 1.E-5, 1.E-6, 1.E-5, 1.E-7, 1.E-7, 1.E-7])


    def _post(self):
        """
        Post-treatment processes
        """


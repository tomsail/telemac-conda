
"""
Validation script for lyn
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
        self.rank = 5
        self.tags = ['telemac3d', 'gaia']
        # For vnv_3
        self.walltime = '14:00:00'

    def _pre(self):
        """
        Defining the studies
        """

        # Lyn scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_lyn.cas')


        # Lyn parallel mode
        cas = TelemacCas('t3d_lyn.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_lyn_par.cas',
                       cas=cas)

        del cas


        # Lyn scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_lyn_4_classes.cas')


        # Lyn parallel mode
        cas = TelemacCas('t3d_lyn_4_classes.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_lyn_4_classes_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_lyn.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_lyn.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_lyn.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_lyn.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:GAIRES',
                            'gai_ref_lyn_4_classes.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:GAIRES',
                            'gai_ref_lyn_4_classes.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:GAIRES',
                            'vnv_4:GAIRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_lyn_4_classes.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_lyn_4_classes.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """


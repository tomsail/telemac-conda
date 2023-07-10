
"""
Validation script for Friction_med
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
        self.rank = 0
        self.tags = ['tomawac', 'med']

    def _pre(self):
        """
        Defining the studies
        """

        # friction med scalar mode
        self.add_study('vnv_1',
                       'tomawac',
                       'tom_friction_med.cas')

        # friction med parallel mode
        cas = TelemacCas('tom_friction_med.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'tomawac',
                       'tom_friction_med_par.cas',
                       cas=cas)
        del cas

        # friction med scalar mode
        self.add_study('vnv_3',
                       'tomawac',
                       'tom_repfri.cas')

        # friction med parallel mode
        cas = TelemacCas('tom_repfri.cas', get_dico('tomawac'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'tomawac',
                       'tom_repfri_par.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'fom_friction.med',
                            eps=[1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'fom_friction.med',
                            eps=[1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACLEO',
                            'fom_spe_friction.med',
                            eps=[1e-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACLEO',
                            'fom_spe_friction.med',
                            eps=[1e-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACLEO',
                            'vnv_2:WACLEO',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRBI',
                            'ini_fri.med',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'fom_r2dreprise.med',
                            eps=[360, 1e-4, 1e-4, 1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'fom_r2dreprise.med',
                            eps=[360, 1e-4, 1e-4, 1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[360, 1e-4, 1e-4, 1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACLEO',
                            'vnv_4:WACLEO',
                            eps=[1e-4])

    def _post(self):
        """
        Post-treatment processes
        """

"""
Validation script for gouttedo_med
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
        self.rank = 4
        self.tags = ['telemac2d', 'med']

    def _pre(self):
        """
        Defining the studies
        """
        # Default med run
        self.add_study('seq',
                       'telemac2d',
                       't2d_gouttedo_med.cas')

        cas = TelemacCas('t2d_gouttedo_med.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('par',
                       'telemac2d',
                       't2d_gouttedo_med_par.cas',
                       cas=cas)

        del cas

        # Concat med run
        cas = TelemacCas('t2d_gouttedo_med.cas', get_dico('telemac2d'))
        cas.set('CONCATENATE PARTEL OUTPUT', True)
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('concat',
                       'telemac2d',
                       't2d_gouttedo_med_concat.cas',
                       cas=cas)
        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        # Epsilons for default run
        self.check_epsilons('seq:T2DRES',
                            'f2d_gouttedo.slf',
                            eps=[1e-6])
        self.check_epsilons('par:T2DRES',
                            'f2d_gouttedo.slf',
                            eps=[1e-6])
        self.check_epsilons('seq:T2DRES',
                            'par:T2DRES',
                            eps=[1e-8])

        self.check_epsilons('concat:T2DRES',
                            'par:T2DRES',
                            eps=[1e-15])

    def _post(self):
        pass

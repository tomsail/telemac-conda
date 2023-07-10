"""
Validation script for api
"""
from vvytel.vnv_study import AbstractVnvStudy

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['api', 'python3', 'apistudy', 'api_mascaret', 'mascaret', 'telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # Mascaret api run of Test1: run from input files
        # Q in .loi varies from 1837 to 7000
        # Ks in .xcas = 40, 32, 33
        self.add_command('mascaret-run',
                         'cd mascaret; python3 test_run.py',
                         hpc=True)

        # Mascaret api run of Test2: run with Ks settings  modified by user
        # Ks in settings = 35, 35, 30
        self.add_command('mascaret-runchange_ks',
                         'cd mascaret; python3 test_runchange_ks.py',
                         hpc=True)

        # Mascaret api run of Test3: run with modified settings in python dico
        # Ks in dico is set to 30 in zone  = 2 (3rd zone)
        # Q in dico is set to modified values
        self.add_command('mascaret-run_change_ks_q',
                         'cd mascaret; python3 test_runchange_ks_q.py',
                         hpc=True)

        # Mascaret api run of Test4: run with modified settings in python dico
        # Q perturbed with Gaussian process
        self.add_command('mascaret-run_gp_sampler.',
                         'cd mascaret; python3 test_run_gp_sampler.py',
                         hpc=True)

        # Mascaret api run of Test5: Ensemble run with  modified settings in python dico
        # Ks and Q perturbed with sampling in U and BetaMuSigma pdfs in dico
        self.add_command('mascaret-ens',
                         'cd mascaret; python3 test_ens.py',
                         hpc=True)

        # Telemac2d api run of Test1: Two runs, the second with  modified settings in python dico
        # Ks and Q perturbed with values in dico
        self.add_command('telemac2d-run',
                         'cd telemac2d; python3 test_run.py 4',
                         hpc=True)

        # Telemac2d api run of Test2: Ensemble run with modified settings in python dico
        # Ks and Q perturbed with sampling in U and BetaMuSigma pdfs in dico
        self.add_command('telemac2d-ens',
                         'cd telemac2d; python3 test_ens.py 4',
                         hpc=True)

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """

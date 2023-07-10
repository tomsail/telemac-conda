
"""
Validation script for test12
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
        self.tags = ['mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        # Test12 unsteady subcritical kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'rezo.xcas')


        # Test12 transcritical implicit kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'mascaret_imp.xcas')


        # Test12 transcritical explicit kernel
        self.add_study('vnv_3',
                       'mascaret',
                       'mascaret_exp.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """


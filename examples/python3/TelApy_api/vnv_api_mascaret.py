
"""
Validation script for api
"""
from sys import platform

from vvytel.vnv_study import AbstractVnvStudy


class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['api', 'python3', 'api_mascaret', 'mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        python = 'python' if platform == 'win32' else 'python3'

        # Mascaret api run of Test1
        self.add_command('vnv_mascaret',
                         f'{python} mascaret.py',
                         hpc=True)

        # Mascaret api run of Test_Tracer
        self.add_command('vnv_mascaret_tracer',
                         f'{python} mascaret_tracer.py',
                         hpc=True)

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """

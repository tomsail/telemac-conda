
"""
Validation script for hermes
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
        self.tags = ['api', 'python3', 'med']

    def _pre(self):
        """
        Defining the studies
        """

        python = 'python' if platform == 'win32' else 'python3'

        # Telemac2d api run of gouttedo
        self.add_command('vnv_read_write',
                         f'{python} read_write.py')

        # Telemac2d api run of gouttedo
        self.add_command('vnv_read_write_format',
                         f'{python} read_write_format.py')

        # Telemac2d api run of gouttedo
        self.add_command('vnv_telemac_file',
                         f'{python} test_telemac_file.py')

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """

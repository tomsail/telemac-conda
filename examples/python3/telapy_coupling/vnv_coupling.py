"""
Validation script for coupling
"""
from os import environ
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
        self.tags = ['api', 'api_mascaret', 'python3',
                     'coupling', 'mascaret', 'telemac2d']

    def _pre(self):
        """
        Defining the studies
        """
        # Coupling run of Channel_Manning
        hometel_var = environ['HOMETEL']

        python = 'python' if platform == 'win32' else 'python3'

        # Simple channel coupling described by a Python file
        self.add_command('vnv_coupling_channel_manning',
                         'cd channel_manning && '
                         f'{python} {hometel_var}/scripts/python3/'
                         'run_cpl.py long',
                         hpc=True)

        # Simple channel with initial discontinuity described by a Python file
        self.add_command('vnv_coupling_channel_ic',
                         'cd channel_ic && '
                         f'{python} {hometel_var}/scripts/python3/'
                         'run_cpl.py long',
                         hpc=True)

        # Simple channel with varying upstream boundary condition described by
        # a Python file
        self.add_command('vnv_coupling_channel_bc',
                         'cd channel_bc && '
                         f'{python} {hometel_var}/scripts/python3/'
                         'run_cpl.py long',
                         hpc=True)

        # Real life coupling driven by a user defined procedure
        self.add_command('vnv_coupling_bayonne',
                         'cd bayonne && '
                         f'{python} driven_coupling.py',
                         hpc=True)

    def _check_results(self):
        """
        Post-treatment processes
        """

    def _post(self):
        """
        Post-treatment processes
        """

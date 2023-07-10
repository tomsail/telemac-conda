
"""
Validation script for pretel
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
        self.tags = ['python3']

    def _pre(self):
        """
        Defining the studies
        """

        # Scanning double precision big-endian file
        self.add_command('scan_big_double',
                         'run_telfile.py scan r2d_bowl_vf_gb_DoublePrecision_BigEndian.slf --data')


        # Scanning double precision little endian file
        self.add_command('scan_little_double',
                         'run_telfile.py scan r2d_bowl_vf_gb_DoublePrecision_LittleEndian.slf --data')


        # Scanning single precision big endian file
        self.add_command('scan_big_single',
                         'run_telfile.py scan r2d_bowl_vf_gb_SinglePrecision_BigEndian.slf --data')


        # Scanning single precision little endian file
        self.add_command('scan_little_single',
                         'run_telfile.py scan r2d_bowl_vf_gb_SinglePrecision_LittleEndian.slf --data')


        # Scanning of a spectral file (Tomawac output)
        self.add_command('spec',
                         'run_telfile.py scan opposing_cur.spe')


        # Remove one time step each 2
        self.add_command('alter-0',
                         'run_telfile.py alter r2d_gouttedo.slf r2d_gouttedo_chop.slf -f 1 -s -1 -d 2 --force')


        # Keeping only last time step in file
        self.add_command('alter-1',
                         'run_telfile.py alter r2d_gouttedo.slf r2d_gouttedo_last_time.slf -f -1 -s -1 -d 1 --force')


        # Switch endianess
        self.add_command('alter-2',
                         'run_telfile.py alter r2d_bowl_vf_gb_SinglePrecision_LittleEndian.slf r2d_bowl_BigEndian.slf --endian --force')


        # Increase water depth by 10m
        self.add_command('alter-3',
                         'run_telfile.py alter r2d_gouttedo.slf r2d_gouttedo_10m.slf --var?="WATER DEPTH" --var+? 10 --force')

        # Converting from latlong to utm
        self.add_command('alter-ll2utm',
                         'run_telfile.py alter bts3d-v7-latlon.slf r3d_utm.slf --ll2utm 00X --force' )

        # Converting from utm zone 24S to latlong
        self.add_command('alter-utm2ll',
                         'run_telfile.py alter bts3d-v7-utm.slf r3d_latlon.slf --utm2ll 28H --force')

        # Merge of an output from 0.0s to 1.8s with one from 2.0s to 4.0s
        self.add_command('merge-time',
                         'run_telfile.py merge --kind="time" r2d_gouttedo_part1.slf r2d_gouttedo_part2.slf r2d_gouttedo_full.slf --force')

        # Merge of an output with velocity and one with water depth
        self.add_command('merge-var',
                         'run_telfile.py merge --kind="var" r2d_gouttedo_part3.slf r2d_gouttedo_part4.slf r2d_gouttedo_full2.slf --force')


    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """


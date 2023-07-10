
"""
Validation script for canalalgae2
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # canalalgae2 scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_canal_algae2.cas')

        # canalalgae2 4 proc
        cas = TelemacCas('t2d_canal_algae2.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_canal_algae2_par.cas',
                       cas=cas)

        del cas

    def _check_results(self):
        """
        Post-treatment processes
        """
        import os

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_canalalgae2.slf',
                            eps=[1e-2])

        # Read reference text file
        ref_file = 'polygon_particles_ref.txt'
        f_ref = open(ref_file, 'r')
        lines_ref = f_ref.readlines()
        lines_ref = lines_ref[2:]

        working = self.get_vnv_working_dir('vnv_1')
        os.chdir(working)

        # Read output text file
        out_file = self.get_study_file('vnv_1:T2DRFO')
        f_out = open(out_file, 'r')
        lines_out = f_out.readlines()
        lines_out = lines_out[2:]

        # Compare text files
        i = 0
        diff_max = 0
        for line in lines_out:
            fields_out = line.split()
            fields_ref = lines_ref[i].split()
            for j in range(1, 5):
                diff = abs(int(fields_out[j])-int(fields_ref[j]))
                if diff > diff_max:
                    diff_max = diff
            i = i + 1

        # TODO: Test not valid results different on a different machine/compiler
        if diff_max < 1:
            print('Output text file matches reference file. Success.')
        else:
            print('Output text file different to reference file. Failure.')

    def _post(self):
        """
        Post-treatment processes
        """

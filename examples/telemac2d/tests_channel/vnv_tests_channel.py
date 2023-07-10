
"""
Validation script for tests_channel
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

        # tests_channel scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_tests_channel.cas')


        # tests_channel parallel mode
        cas = TelemacCas('t2d_tests_channel.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_tests_channel_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_tests_channel.slf',
                            eps=[1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_tests_channel.slf',
                            eps=[1.E-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1.E-7])


    def _post(self):
        """
        Post-treatment processes
        """
        from data_manip.extraction.telemac_file import TelemacFile
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
                # Getting files
        vnv_seq_t2dres = self.get_study_file('vnv_seq:T2DRES')
        res_vnv_seq_t2dres = TelemacFile(vnv_seq_t2dres)
        vnv_seq_t2dgeo = self.get_study_file('vnv_seq:T2DGEO')
        res_vnv_seq_t2dgeo = TelemacFile(vnv_seq_t2dgeo)

        # TODO: Plot figures from documentation

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_seq_t2dgeo,
                   plot_mesh=True,
                   fig_size=(12, 8),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_seq_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(12, 8),
                   fig_name='img/Bathy')

        # Closing files
        res_vnv_seq_t2dres.close()
        res_vnv_seq_t2dgeo.close()

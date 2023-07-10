
"""
Validation script for test2
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

        # Test2 permanent kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'sarap.xcas')


        # Test2 transcritical kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'mascaret.xcas')



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
        import matplotlib.pyplot as plt
        from data_manip.formats.mascaret_file import MascaretFile
        from postel.plot1d import plot1d
        from os import path

        # Getting files
        steady_res, _ = self.get_study_res('vnv_1:sarap_ecr.opt', 'mascaret')
        masc_res, _   = self.get_study_res('vnv_2:mascaret_ecr.opt', 'mascaret')
        #
        var_pos_sarap = steady_res.get_position_var_abbr('Y')
        var_pos_masc  = masc_res.get_position_var_abbr('Y')
        values_sarap  = steady_res.get_values_at_reach(-1, 1, [var_pos_sarap])
        values_masc   = masc_res.get_values_at_reach(-1, 1, [var_pos_masc])
        #
        fig, ax = plt.subplots(figsize=(10, 8))
        plot1d(ax, steady_res.reaches[1].get_section_pk_list(), values_sarap,
               plot_label='Steady Kernel',
               x_label='Abscissae (m)',
               y_label='Water height (m)')
        plot1d(ax, masc_res.reaches[1].get_section_pk_list(), values_masc,
               plot_label='Transcritical kernel',
               x_label='Abscissae (m)',
               y_label='Water height (m)')
        # Displaying legend
        ax.legend()
        # Showing figure
        plt.savefig(path.join('.', 'img', 'pfl_long.png'))
        plt.close('all')

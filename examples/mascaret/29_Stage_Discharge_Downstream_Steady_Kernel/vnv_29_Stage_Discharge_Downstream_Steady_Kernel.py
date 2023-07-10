
"""
Validation script for Test 29
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
        self.rank = 0
        self.tags = ['mascaret']

    def _pre(self):
        """
        Defining the studies
        """

        # Test network computation
        self.add_study('vnv_1',
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
        """
        import matplotlib.pyplot as plt
        import data_manip.extraction.parser_csv as CSV
        from data_manip.formats.mascaret_file import MascaretFile
        from postel.plot1d import plot1d
        from os import path

        # Getting Measurement
        mesure = CSV.CSV(path.join('.', 'ref', 'mesure.csv'),';')
        # Getting files
        masc_file_bous, _ = self.get_study_res('vnv_1:mascaret.rub', 'mascaret')
        masc_file_nob,  _ = self.get_study_res('vnv_2:mascaret.rub', 'mascaret')
        #
        varname = 'Cote de l eau'
        reach_id = 1
        section_id = 560
        var_pos_bous = masc_file_bous.get_position_var(varname)
        var_pos_nob  = masc_file_nob.get_position_var(varname)
        values_bous  = masc_file_bous.get_series(reach_id, section_id, [var_pos_bous])
        values_nob   = masc_file_nob.get_series(reach_id, section_id, [var_pos_nob])
        #
        fig, ax = plt.subplots(figsize=(10, 8))
        plot1d(ax, mesure.colcore[0], mesure.colcore[1],
               plot_label='Mesure',
               x_label='Temps (s)',
               y_label='Cote (m)',
               marker='+', markersize=8,
               linestyle='None')
        plot1d(ax, masc_file_nob.times, values_nob,
               plot_label='Calcul Saint-Venant',
               x_label='Temps (s)',
               y_label='Cote (m)')
        plot1d(ax, masc_file_bous.times, values_bous,
               plot_label='Calcul non hydrostatique',
               x_label='Temps (s)',
               y_label='Cote (m)')
        # Displaying legend
        ax.legend()
        # Showing figure
        plt.savefig(path.join('.', 'img', 'evol.png'))
        plt.close('all')
        """

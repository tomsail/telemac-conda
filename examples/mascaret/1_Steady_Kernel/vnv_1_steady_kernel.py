
"""
Validation script for test1
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

        # Test1 steady kernel
        self.add_study('vnv_1',
                       'mascaret',
                       'sarap.xcas')

        # Test1 explicit transcritical kernel
        self.add_study('vnv_2',
                       'mascaret',
                       'mascaret_exp.xcas')

        # Test1 implicit transcritical kernel
        self.add_study('vnv_3',
                       'mascaret',
                       'mascaret_imp.xcas')


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

        # Compute analytical solution RK4, step = 10m
        xc = 10000
        x  = [10000]
        y0 = 3.0
        y  = [y0]
        dx = 10
        q  = 10
        K  = 30.6
        I  = 0.0005
        g  = 9.81
        while xc > 0:
            xc = xc - dx
            x.append(xc)
            #calcul des coefficients RK4
            k1 = (I-q**2/(K**2*(y0)**(10/3.0)))     /(1-q**2/(g*(y0)**3))     *dx
            k2 = (I-q**2/(K**2*(y0+k1/2)**(10/3.0)))/(1-q**2/(g*(y0+k1/2)**3))*dx
            k3 = (I-q**2/(K**2*(y0+k2/2)**(10/3.0)))/(1-q**2/(g*(y0+k2/2)**3))*dx
            k4 = (I-q**2/(K**2*(y0+k3)**(10/3.0)))  /(1-q**2/(g*(y0+k3)**3))  *dx
            y1 = y0 - (1/6.0)*(k1 + 2*k2 + 2*k3 + k4)
            y0 = y1
            y.append(round(y1,6))

        # Getting files
        steady_res, _ = self.get_study_res('vnv_1:sarap_ecr.opt', 'mascaret')
        masc_exp_res,  _ = self.get_study_res('vnv_2:mascaret_exp_ecr.opt', 'mascaret')
        masc_imp_res,  _ = self.get_study_res('vnv_3:mascaret_imp_ecr.opt', 'mascaret')
        #
        var_pos_sarap = steady_res.get_position_var_abbr('Y')
        var_pos_exp   = masc_exp_res.get_position_var_abbr('Y')
        var_pos_imp   = masc_imp_res.get_position_var_abbr('Y')
        values_sarap  = steady_res.get_values_at_reach(-1, 1, [var_pos_sarap])
        values_exp    = masc_exp_res.get_values_at_reach(-1, 1, [var_pos_exp])
        values_imp    = masc_imp_res.get_values_at_reach(-1, 1, [var_pos_imp])
        #
        fig, ax = plt.subplots(figsize=(10, 8))
        plot1d(ax, x, y,
               plot_label='Analytical solution',
               x_label='Abscissae (m)',
               y_label='Water height (m)')
        plot1d(ax, steady_res.reaches[1].get_section_pk_list(), values_sarap,
               plot_label='Permanent',
               x_label='Abscissae (m)',
               y_label='Water height (m)')
        plot1d(ax, masc_exp_res.reaches[1].get_section_pk_list(), values_exp,
               plot_label='Transcritical kernel Expl. (Nc=0.8)',
               x_label='Abscissae (m)',
               y_label='Water height (m)')
        plot1d(ax, masc_imp_res.reaches[1].get_section_pk_list(), values_imp,
               plot_label='Transcritical kernel Impl. (Nc=2)',
               x_label='Abscissae (m)',
               y_label='Water height (m)',
               linestyle=':')
        # Displaying legend
        ax.legend()
        # Showing figure
        plt.savefig(path.join('.', 'img', 'long.png'))
        ax.set_xlim(8800,10200)
        ax.set_ylim(3,4.4)
        plt.savefig(path.join('.', 'img', 'zoom.png'))
        plt.close('all')
        # Printing table of results
        xz = x[::-100]
        yz = y[::-100]
        vsarap = values_sarap[::10]
        vexp   = values_exp[::10]
        vimp   = values_imp[::10]
        file_path = path.join('.', 'img', 'table.txt')
        with open(file_path, 'w') as outfile:
            for i, _ in enumerate(xz):
                outfile.write('%6i' % xz[i])
                outfile.write('& %6.3f' % yz[i])
                outfile.write('& %6.3f' % vsarap[i])
                outfile.write('& %6.3f' % vexp[i])
                outfile.write('& %6.3f \\\\ \n' % vimp[i])

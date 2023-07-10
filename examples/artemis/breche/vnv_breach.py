
"""
Validation script for breach
"""
from vvytel.vnv_study import AbstractVnvStudy
from vvytel.vnv_tools import compute_norm, compute_diff
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from utils.exceptions import TelemacException
import numpy as np

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # breach scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_breach.cas')


        # breach scalar mode
        cas = TelemacCas('art_breach.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_breach_par.cas',
                       cas=cas)

        del cas


        # breach scalar mode
        cas = TelemacCas('art_breach_concat.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_concat',
                       'artemis',
                       'art_breach_concat_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_breach.slf',
                            eps=[1.e-12])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_breach.slf',
                            eps=[1.e-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2:ARTRES',
                            'vnv_concat:ARTRES',
                            eps=[1.e-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2:ARTRES',
                            'vnv_concat:ARTRES',
                            eps=[1.e-12])

        # Checking against experiment
        # TODO: merge with code below
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res = TelemacFile(vnv_1_artres)

        exp = np.loadtxt('experiment.csv', comments='#', delimiter=',',
                         skiprows=4)

        poly = [[0., 915.], [1800., 915.]]

        poly_coord, _, values  = \
                res.get_timeseries_on_polyline('WAVE HEIGHT',
                                               poly)
        from scipy.interpolate import interp1d
        x1 = exp[:, 0]
        y1 = exp[:, 1]
        x2 = poly_coord[:, 0]
        f = interp1d(x1, y1, kind=3)
        ExpInterp = f(x2[10:])

        print(" "*8+"+> checking epsilon between files {} and {}:"\
              .format('experiment.csv',
                      vnv_1_artres.replace(self.case_dir, '.')))

        diff = compute_diff(values[10:, 0], ExpInterp)

        err = compute_norm(diff)

        eps = 0.4
        print(" "*10 + "- Difference for variable {}: {} (eps={})"\
              .format('WAVE HEIGHT', err, eps))

        if err > eps:
            raise TelemacException(\
                "Epsilon reached in {} vs {}"\
                .format('experiment.csv', vnv_1_artres))

        res.close()

    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_actions import plot1d
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res = TelemacFile(vnv_1_artres)

        exp = np.loadtxt('experiment.csv', comments='#', delimiter=',',
                         skiprows=4)

        poly = [[0., 915.], [1800., 915.]]

        poly_coord, abs_curv, values  = \
                res.get_timeseries_on_polyline('WAVE HEIGHT',
                                               poly)
        from scipy.interpolate import interp1d
        x1 = exp[:, 0]
        y1 = exp[:, 1]
        x2 = poly_coord[:, 0]
        f = interp1d(x1, y1, kind=3)
        ExpInterp = f(x2[10:])

        fig, ax = plt.subplots(1, 1, figsize=(10, 7))

        plot1d(ax, abs_curv[10:], ExpInterp,
               plot_label='Interpolated Experiment',
               color='green')

        ax.plot(exp[:, 0], exp[:, 1], color='red', marker='o',
                label='Experiment', ls='')

        plot1d(ax, abs_curv, values,
               plot_label='Model')

        ax.legend()

        fig_name = 'img/Section'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)

        plt.close('all')

        #Plotting mesh
        vnv_plot2d('',
                   res,
                   plot_mesh=True,
                   fig_size=(10, 10),
                   fig_name='img/Mesh')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 10),
                   fig_name='img/WaveHeight')


        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 10),
                   fig_name='img/Free_Surface')

        # Closing files
        res.close()

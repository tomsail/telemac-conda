
"""
Validation script for pluie
"""
from vvytel.vnv_study import AbstractVnvStudy
from vvytel.vnv_tools import compute_norm, compute_diff
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from utils.exceptions import TelemacException

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 1
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # pluie parallel mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_pluie.cas')

        # pluie parallel mode
        self.add_study('p3d',
                       'postel3d',
                       'p3d_pluie.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_pluie.slf',
                            eps=[1.E-15])
        # Building Analytic solution
        res = TelemacFile(self.get_study_file('vnv_1:T3DRES'))

        node = res.get_closest_node([0, 0], plane=res.nplan-1)
        data = res.get_timeseries_on_nodes('ELEVATION Z', [node])[0]
        times = res.times
        anal_sol = 10. + times*864000.*1.e-3/86400.

        diff = compute_diff(anal_sol, data, relative=False)
        err = compute_norm(diff, norm='linf')

        # Check epsilon
        eps = 1e-6
        print(" " * 10 + "- Difference for variables {}/{}: {} (eps={})"\
              .format('results', 'analytic solution', err, eps))

        if err > eps:
            raise TelemacException(\
                    "Epsilon reached in results vs analytic solution{}")

        # Checking that we have to current velocity
        vel = res.get_data_value('VELOCITY U', -1)
        err = compute_norm(vel, norm='linf')

        # Check epsilon
        eps = 1e-6
        print(" " * 10 + "- Absence of velocity : {} (eps={})"\
              .format(err, eps))

        if err > eps:
            raise TelemacException(\
                    "Epsilon reached in results vs analytic solution{}")

        res.close()


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot2d import plot2d_scalar_filled_contour
        from data_manip.computation.triangulation import triangulation_from_data
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        import matplotlib.pyplot as plt
        import numpy as np
                # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        node = res.get_closest_node([0, 0], plane=0)
        vnv_plot1d_history(\
                'SALINITY',
                res,
                'SALINITY',
                nodes=[node +i*res.npoin2 for i in range(res.nplan)],
                nodes_labels=["plane {}".format(i) for i in range(res.nplan)],
                x_label='t (s) ', y_label='Salinity (g/L) ',
                fig_size=(12, 7),
                fig_name='img/profil_time3')

        vnv_plot2d('ELEVATION Z',
                   res,
                   plot_mesh=True,
                   fig_name='img/Mesh')

        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[-5, 0], [5, 0]],
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_name='img/MeshV')

        # Plotting salinity at multiple time steps
        #TODO: Colobar not as it should
        fig, ax = plt.subplots(4, 2,
                               gridspec_kw={'width_ratios':[1, 0.15]})

        poly = [[-5, 0], [5, 0]]

        poly_number = res.discretize_polyline(poly)

        for record in [0, 1, 2, 3]:
            _, abs_curv, poly_z = res.get_data_on_vertical_plane(\
                                      'ELEVATION Z', record, poly, poly_number)

            _, _, data = res.get_data_on_vertical_plane(\
                               'SALINITY', record, poly, poly_number)

            mesh = triangulation_from_data(abs_curv, poly_z)

            img = plot2d_scalar_filled_contour(fig, ax[record, 0], mesh, data.flatten(),
                                         levels=np.linspace(28, 32, 9),
                                         colorbar=False)

            ax[record, 0].set_title('t={}s'.format(res.times[record]))
            ax[record, 0].set_xlim(0, 10)
            ax[record, 0].set_ylim(9, 10.25)

        for axe in ax[:, 1]:
            axe.axis('off')

        cax = fig.add_axes([0.80, 0.15, 0.05, 0.7])
        cbar = plt.colorbar(img, cax=cax, ticks=np.linspace(28, 32, 9))
        cbar.set_label('Salinity')

        fig_name = 'img/res_Sal'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res.close()

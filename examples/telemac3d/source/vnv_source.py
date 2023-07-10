
"""
Validation script for gouttedo
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation of Thompson
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 2
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # source scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_source.cas')


        # source parallel mode
        cas = TelemacCas('t3d_source.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_source_par.cas',
                       cas=cas)

        del cas


        # source (given by node numbers) scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_source_nn.cas')


        # source parallel mode
        cas = TelemacCas('t3d_source_nn.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_source_nn_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_source.slf',
                            eps=[1.E-7, 1.E-6, 1.E-6, 1.E-7, 1.E-5, 1.E-5, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_source.slf',
                            eps=[1.E-7, 1.E-6, 1.E-6, 1.E-7, 1.E-5, 1.E-5, 1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-7, 1.E-6, 1.E-6, 1.E-7, 1.E-5, 1.E-5, 1.E-5])

        # Comparison with the last time frame of the result file of ref 1.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_1:T3DRES',
                            eps=[1.E-15])

        # Comparison with the last time frame of the result file of ref 2.
        self.check_epsilons('vnv_4:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-15])

        # Comparison between sequential and parallel run of cases 3-4.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-7, 1.E-6, 1.E-6, 1.E-7, 1.E-5, 1.E-5, 1.E-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from data_manip.extraction.telemac_file import TelemacFile
        from postel.plot_vnv import vnv_plot2d
        from postel.plot2d import plot2d_scalar_filled_contour
        from data_manip.computation.triangulation import triangulation_from_data
        from postel.plot2d import plot2d_vectors
        import matplotlib.pyplot as plt
        import numpy as np
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)
        vnv_1_t3dhyd = self.get_study_file('vnv_1:T3DHYD')
        geo = TelemacFile(vnv_1_t3dhyd)

        # Plot mesh from 2d geom
        #TODO: Add annotate to mesh to show the source points
        vnv_plot2d('BOTTOM',
                   geo,
                   plot_mesh=True,
                   fig_size=(10, 5),
                   fig_name='img/Mesh')

        ####
        # Plot vertical mesh
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[-50, 0], [50, 0]],
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(10, 5),
                   fig_name='img/MeshV')

        ####
        # Plot velocity vectors
        fig, ax = plt.subplots(1, 1, figsize=(15, 10))

        plane = 2
        record = 5
        vel_x = res.get_data_on_horizontal_plane(\
                'VELOCITY U', record, plane)
        vel_y = res.get_data_on_horizontal_plane(\
                'VELOCITY V', record, plane)

        norm = np.sqrt(vel_x**2 + vel_y**2)

        img = plot2d_scalar_filled_contour(\
                fig,
                ax,
                geo.tri,
                norm,
                data_name='Velocity [m/s]',
                colorbar=False)

        plot2d_vectors(\
                fig, ax,
                geo.tri,
                vel_x,
                vel_y,
                grid_resolution=[20, 20],
                data_name='velocity',
                color='k')

        # Building custom colorbar
        # Emptying second axe

        cbar = fig.colorbar(img, ticks=np.linspace(0, 1, 11),
                            orientation='horizontal')
        cbar.set_label('Velocity [m/s]')

        fig_name = 'img/velo_vec'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')


        ####
        # Plotting tracer values over a plane

        fig, ax = plt.subplots(3, 2, figsize=(10, 15),
                               gridspec_kw={'width_ratios':[1, 0.15]})


        mesh = geo.tri
        plane = 0
        record = -1

        for tracer in range(1, 4):

            var_name = 'TRACER {}'.format(tracer)
            name = 'Tracer {}'.format(tracer)

            data = res.get_data_on_horizontal_plane(\
                         var_name, record, plane)

            img = plot2d_scalar_filled_contour(\
                    fig, ax[tracer-1, 0], mesh,
                    data.flatten(),
                    data_name=name,
                    levels=np.linspace(0, 10, 11),
                    colorbar=False)

            ax[tracer-1, 0].set_title(name)
            ax[tracer-1, 0].set_xlabel('X [m]')
            ax[tracer-1, 0].set_xlabel('Z [m]')

        for axe in ax[:, 1]:
            axe.axis('off')

        cax = fig.add_axes([0.85, 0.15, 0.05, 0.7])
        cbar = plt.colorbar(img, cax=cax, ticks=np.linspace(0, 10, 11))
        cbar.set_label('Tracer')

        fig_name = 'img/plumTr_H'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #####
        # Plotting tracer values over a polyline
        fig, ax = plt.subplots(3, 2, figsize=(10, 15),
                               gridspec_kw={'width_ratios':[1, 0.15]})

        poly = [[-50, 0], [50, 0]]

        _, abs_curv, poly_z0 = res.get_data_on_vertical_plane(\
                                   'ELEVATION Z', -1, poly)

        mesh0 = triangulation_from_data(abs_curv, poly_z0)

        for tracer in range(1, 4):

            var_name = 'TRACER {}'.format(tracer)
            name = 'Tracer {}'.format(tracer)

            _, _, data0 = res.get_data_on_vertical_plane(\
                               var_name, -1, poly)
            img = plot2d_scalar_filled_contour(\
                    fig, ax[tracer-1, 0], mesh0,
                    data0.flatten(),
                    data_name=name,
                    levels=np.linspace(0, 10, 11),
                    colorbar=False)

            ax[tracer-1, 0].set_title(name)
            ax[tracer-1, 0].set_xlabel('X [m]')
            ax[tracer-1, 0].set_xlabel('Z [m]')

        for axe in ax[:, 1]:
            axe.axis('off')

        cax = fig.add_axes([0.85, 0.15, 0.05, 0.7])
        cbar = plt.colorbar(img, cax=cax, ticks=np.linspace(0, 10, 11))
        cbar.set_label('Tracer')

        fig_name = 'img/plumTr_V'
        print(" "*8+"~> Plotting "+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        res.close()
        geo.close()

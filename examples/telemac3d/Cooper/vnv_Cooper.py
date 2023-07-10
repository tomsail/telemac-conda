
"""
Validation script for Cooper
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 3
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # Cooper scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_cooper.cas')


        # Cooper parallel mode
        cas = TelemacCas('t3d_cooper.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_cooper_par.cas',
                       cas=cas)

        del cas

        # Post-treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_cooper.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_cooper.slf',
                            eps=[1.E-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_cooper.slf',
                            eps=[1.E-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-9])


    def _post(self):
        """
        Post-treatment processes
        """
        import matplotlib.pyplot as plt
        import numpy as np
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        from data_manip.computation.triangulation import triangulation_from_data
        from postel.plot2d import plot2d_scalar_map, plot2d_triangle_mesh, plot2d_vectors, plot2d_scalar_filled_contour
        vnv_1_t2dcli = self.get_study_file('vnv_1:T3DCLI')
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        vnv_1_t2dres = self.get_study_file('vnv_1:T3DHYD')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres,bnd_file=vnv_1_t2dcli)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dres,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_name='img/mesh')
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   filled_contours=True,
                   cbar_label='Bathymetry (m)',
                   fig_name='img/bathy')
        #Plotting vertical mesh
        points=[[0,2000],[4000,2000]]
        poly_number=[50]
        vnv_plot2d('ELEVATION Z',res_vnv_1_t3dres,
                   plot_mesh=True,
                   y_label='z (m)(multiply by 50)',
                   y_factor=50.0,
                   poly=points,
                   fig_size=(12,3),
                   poly_number=poly_number,
                   aspect_ratio='equal',
                   fig_name='img/vert_mesh')
        
        vnv_plot2d('TRACER 1',res_vnv_1_t3dres,
                   filled_contours=True,
                   plot_mesh=True,
                   y_label='z (m)(multiply by 50)',
                   y_factor=50.0,
                   poly=points,
                   cbar_label='Tracer',
                   fig_size=(12,3),
                   poly_number=poly_number,
                   vmin=0, vmax=1e-14,
                   aspect_ratio='equal',
                   fig_name='img/tracer_i')
        vnv_plot2d('TRACER 1',res_vnv_1_t3dres,
                   filled_contours=True,
                   plot_mesh=True,
                   y_label='z (m)(multiply by 50)',
                   y_factor=50.0,
                   poly=points,
                   record=-1,
                   fig_size=(12,3),
                   cbar_label='Tracer',
                   poly_number=poly_number,
                   aspect_ratio='equal',
                   fig_name='img/tracer_f')

        vnv_plot2d('VELOCITY',res_vnv_1_t3dres,
                   filled_contours=True,
                   plot_mesh=True,
                   y_label='z (m)(multiply by 50)',
                   y_factor=50.0,
                   poly=points,
                   vmin=0, vmax=1e-14,
                   fig_size=(12,3),
                   cbar_label='Velocity (m/s)',
                   poly_number=poly_number,
                   aspect_ratio='equal',
                   fig_name='img/velocity_i')
        
        vnv_plot2d('VELOCITY',res_vnv_1_t3dres,
                   filled_contours=True,
                   plot_mesh=True,
                   y_label='z (m)(multiply by 50)',
                   y_factor=50.0,
                   poly=points,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   fig_size=(12,3),
                   poly_number=poly_number,
                   aspect_ratio='equal',
                   fig_name='img/velocity_f')
        
        vnv_plot2d('VELOCITY',res_vnv_1_t3dres,
#                   filled_contours=True,
#                   y_label='z (m)(multiply by 50)',
#                   y_factor=50.0,
                   vectors=True,
                   vectors_scale=4,
                   xlim=[1500, 2500],
#                   ylim=[-500, 0],
                   ylim=[-10, 0],
                   grid_resolution=[100, 50],
                   poly=points,
                   record=-1,
#                   fig_size=(12,3),
                   poly_number=poly_number,
#                   aspect_ratio='equal',
                   fig_name='img/velocity_vec')

        _, abs_curv, values_poly_Z=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('ELEVATION Z', -1, points, poly_number)
        mesh = triangulation_from_data(abs_curv, values_poly_Z)
        fig, axes = plt.subplots(figsize=(20, 8))
        plot2d_triangle_mesh(axes, mesh, color='k', linewidth=0.1)
        plt.savefig('img/verticalmesh')
        _, abs_curv2, velox=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('VELOCITY U', -1, points, poly_number)
        _, abs_curv2, veloy=\
        res_vnv_1_t3dres.get_data_on_vertical_plane('VELOCITY W', -1, points, poly_number)
        fig2, axes2 = plt.subplots(figsize=(20, 8))
        velocity = np.sqrt(velox.flatten()**2 + veloy.flatten()**2)
        plot2d_scalar_filled_contour(fig2, axes2, mesh, velocity, data_name='Velocity (m/s)',\
                                 x_label="Distance (m)", y_label="Z (m)")
        plot2d_vectors(fig2, axes2, mesh, velox.flatten(), veloy.flatten(), grid_resolution=[29, 29],\
                       data_name='Velocity', color='k', grid_xlim=[1500, 2500], grid_ylim=[-10, 0], scale = 50)
        axes2.set(xlim=[1500, 2500], ylim=[-10, 0])

        filefig='img/vectorial'
        print('        ~> Plotting',filefig)
        plt.savefig(filefig)

        # Closing files
        res_vnv_1_t3dres.close()
        res_vnv_1_t2dres.close()

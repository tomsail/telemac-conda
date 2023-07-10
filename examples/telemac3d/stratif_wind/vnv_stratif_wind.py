
"""
Validation script for stratif_wind
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # static equilibrium over a bump on the bottom scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_stratif_wind.cas')


        # static equilibrium over a bump on the bottom parallel mode
        cas = TelemacCas('t3d_stratif_wind.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_stratif_wind_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_stratif_wind.slf',
                            eps=[1.E-4, 1.E-3, 1.E-3, 1.E-8, 1.1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_stratif_wind.slf',
                            eps=[1.E-4, 1.E-3, 1.E-3, 1.E-8, 1.3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-4, 1.E-3, 1.E-3, 1.E-8, 1.3])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res = TelemacFile(vnv_1_t3dres)

        #Plotting mesh
        vnv_plot2d('',
                   res,
                   plot_mesh=True,
                   fig_size=(7.5, 2.5),
                   fig_name='img/res_mesh')

        # Plotting 3D mesh section (vertical mesh)
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0., 1.], [10., 1.]],
                   record=-1,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(12, 7),
                   fig_name='img/res_mesh_section')

        # Plotting bottom elevation Z at 0 (initial time step)
        # Useless as flat bottom
#        vnv_plot2d('ELEVATION Z',
#                   res,
#                   record=0,
#                   plane=0,
#                   cbar_label='$z_f$ (m)',
#                   filled_contours=True,
#                   fig_size=(7.5, 5),
#                   fig_name='img/res_z_bottom_map')

        # Plotting initial condition for temperature, vmax = 26 (ligthly > 25)
        vnv_plot2d('TEMPERATURE',
                   res,
                   poly=[[0., 1.], [10., 1.]],
                   record=0,
                   filled_contours=True,
                   vmin=6, vmax=26,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='$T$ ($^{\circ}$C)',
                   fig_size=(12, 4),
                   fig_name='img/res_temp_IC_section')

        # Plotting final condition for temperature (at -1)
        vnv_plot2d('TEMPERATURE',
                   res,
                   poly=[[0., 1.], [10., 1.]],
                   record=-1,
                   filled_contours=True,
                   vmin=6, vmax=26,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='$T$ ($^{\circ}$C)',
                   fig_size=(12, 4),
                   fig_name='img/res_temp_section')

        # Plotting final velocity vectors (at -1)
        vnv_plot2d('VELOCITY',
                   res,
                   poly=[[0., 1.], [10., 1.]],
                   record=-1,
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=0.02,
                   var_type='vector_3d',
#                   grid_resolution=[80, 80],
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   cbar_label='Scalar velocity (m/s)',
                   fig_size=(12, 4),
                   fig_name='img/res_velocity_vectors_section')

        res.close()

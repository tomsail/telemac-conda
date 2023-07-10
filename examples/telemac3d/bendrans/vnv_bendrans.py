
"""
Validation script for bendrans
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
        self.rank = 3
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # bendrans scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_bendrans.cas')


        # bendrans parallel mode
        cas = TelemacCas('t3d_bendrans.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_bendrans_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_bendrans.slf',
                            eps=[1.E-7])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_bendrans.slf',
                            eps=[1.E-6, 1.E-3, 1.E-4, 1.E-4, 1.E-5, 1.E-4, 1.E-5, 1.E-7])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6, 1.E-3, 1.E-4, 1.E-4, 1.E-5, 1.E-4, 1.E-5, 1.E-7])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        # Getting files
        geom, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        res_vnv_1_t3dres, _ = self.get_study_res('vnv_1:T3DRES')

        # Plotting mesh + write boundary conditions
        vnv_plot2d('',
                   geom,
                   fig_size=(8, 9),
                   fig_name='img/MeshH',
                   annotate_bnd=True,
                   plot_mesh=True)

        #Plotting vertical mesh at initial time step
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[1, 0], [1, 0.72]],
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(12, 4),
                   fig_name='img/MeshV')

        # Plotting horizontal slice of bottom elevation
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=1,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(8, 8),
                   fig_name='img/Bottom')

        # Plotting horizontal slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=5,
                   fig_size=(8, 8),
                   fig_name='img/VelocityH')

        # Plotting horizontal slice of free surface
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(8, 8),
                   fig_name='img/FreeSurface')

        # Plotting horizontal slice of free surface
        vnv_plot2d('NUX FOR VELOCITY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Diffusion along $x$ for velocity (m$^2$/s)',
                   filled_contours=True,
                   fig_size=(8, 8),
                   fig_name='img/NuxVelo')

        # Plotting horizontal slice of free surface
        vnv_plot2d('TURBULENT ENERGY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Turbulent Kinetic Energy (m$^2$/s$^2$)',
                   filled_contours=True,
                   fig_size=(8, 8),
                   fig_name='img/TKE')

        # Plotting horizontal slice of free surface
        vnv_plot2d('DISSIPATION',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Dissipation (m$^2$/s$^3$)',
                   filled_contours=True,
                   fig_size=(8, 8),
                   fig_name='img/Dissipation')

        res_vnv_1_t3dres.close()

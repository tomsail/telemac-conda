
"""
Validation script for pildepon
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
        self.tags = ['telemac3d', 'postel3d']

    def _pre(self):
        """
        Defining the studies
        """

        # pildepon scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_piledepon.cas')


        # pildepon parallel mode
        cas = TelemacCas('t3d_piledepon.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_piledepon_par.cas',
                       cas=cas)

        del cas


        # pildepon scalar mode
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_piledepon-nonhydro.cas')


        # pildepon parallel mode
        cas = TelemacCas('t3d_piledepon-nonhydro.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_piledepon-nonhydro_par.cas',
                       cas=cas)

        del cas

        # pildepon scalar mode
        self.add_study('p3d',
                       'postel3d',
                       'p3d_piledepon.cas')

        # pildepon scalar mode
        self.add_study('p3d2',
                       'postel3d',
                       'p3d_piledepon-nonhydro.cas')



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_piledepon.slf',
                            eps=[1.E-1, 2.E0, 1.5E0, 1.E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_piledepon.slf',
                            eps=[1.E-1, 2.E0, 1.5E0, 1.1E0])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-1, 2.E0, 1.5E0, 1.1E0])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_piledepon-nonhydro.slf',
                            eps=[0.04, 1.5, 1., 0.4, 0.2, 0.3, 0.1, 0.2, 0.2, 0.1, 0.1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_piledepon-nonhydro.slf',
                            eps=[0.2, 1.7, 1.1, 0.4, 0.3, 0.6, 0.3, 0.3, 0.4, 0.3, 0.3])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[0.2, 1.7, 1.1, 0.4, 0.3, 0.6, 0.3, 0.3, 0.4, 0.3, 0.3])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        # Getting files
        res_vnv_1_t3dgeo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        res_vnv_1_t3dres, _ = self.get_study_res('vnv_1:T3DRES')
        res_vnv_3_t3dres, _ = self.get_study_res('vnv_3:T3DRES')

        #Plotting mesh
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(12, 8),
                   fig_name='img/MeshH')

        #Plotting vertical mesh at initial time step
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[-14, 0], [14.5, 0]],
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
                   fig_size=(12, 7),
                   fig_name='img/Bottom')

        # Plotting horizontal slice of vectors velocity
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=30,
                   fig_size=(12, 7),
                   fig_name='img/VelocityHHydro')

        # Plotting horizontal slice of free surface
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/FreeSurfaceHydro')

        # Plotting horizontal slice of vectors velocity (NH)
        vnv_plot2d('VELOCITY',
                   res_vnv_3_t3dres,
                   plane=res_vnv_3_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=30,
                   fig_size=(12, 7),
                   fig_name='img/VelocityHNH')

        # Plotting horizontal slice of free surface (NH)
        vnv_plot2d('ELEVATION Z',
                   res_vnv_3_t3dres,
                   plane=res_vnv_3_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/FreeSurfaceNH')

        #Plotting Z on a node at the surface (res.nplan-1)
        vnv_plot1d_history('VELOCITY V',
                           res_vnv_1_t3dres,
                           'Velocity along y',
                           nodes=[711],
                           fig_size=(12, 7),
                           fig_name='img/res_timeseries_surfaceHydro')

        #Plotting Z on a node at the surface (res.nplan-1)
        vnv_plot1d_history('VELOCITY V',
                           res_vnv_3_t3dres,
                           'Velocity along y',
                           nodes=[711],
                           fig_size=(12, 7),
                           fig_name='img/res_timeseries_surfaceNH')

        res_vnv_1_t3dgeo.close()
        res_vnv_1_t3dres.close()
        res_vnv_3_t3dres.close()

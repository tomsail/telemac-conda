
"""
Validation script for vasque
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
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # vasque scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_vasque.cas')


        # vasque parallel mode
        cas = TelemacCas('t3d_vasque.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_vasque_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_vasque.slf',
                            eps=[1.E-11])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_vasque.slf',
                            eps=[1.E-11])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-11])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        res, _ = self.get_study_res('vnv_1:T3DRES')
        res_vnv_1_t3dhyd, _ = self.get_study_res('vnv_1:T3DHYD')
        res_vnv_1_t3dgeo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")

        vnv_plot1d_polylines('BOTTOM',
                             res_vnv_1_t3dhyd,
                             poly=[[0, 4.5], [46, 4.5]],
                             record=1,
                             legend_labels='bottom',
                             x_label='$x$ (m)', y_label='$z$ (m)',
#                             ylim=[-0.7, 0.1],             
                             fig_size=(12, 4),
                             fig_name='img/Bottom')

        # Plotting FREE SURFACE over polyline over records range(0, res_vnv_1_t3dhyd.ntimestep)
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t3dhyd,
                             poly=[[0, 4.5], [46, 4.5]],
                             record=[i for i in range(0, res_vnv_1_t3dhyd.ntimestep)],
                             plot_bottom=True,
                             legend_labels=['$t$ = 0~min', '$t$ = 1~min', '$t$ = 2~min', '$t$ = 3~min', '$t$ = 4~min', '$t$ = 5~min'],
                             x_label='$x$ (m)', y_label='$z$ (m)',
#                             ylim=[-0.7, 0.1],             
                             fig_size=(12, 4),
                             fig_name='img/FreeSurface_Y5')

        # Plotting WATER DEPTH over polyline over records range(0, res_vnv_1_t3dhyd.ntimestep)
        vnv_plot1d_polylines('WATER DEPTH',
                             res_vnv_1_t3dhyd,
                             poly=[[0, 4.5], [46, 4.5]],
                             legend_labels=['$t$ = 0~min', '$t$ = 1~min', '$t$ = 2~min', '$t$ = 3~min', '$t$ = 4~min', '$t$ = 5~min'],
                             x_label='$x$ (m)', y_label='$h$ (m)',
                             record=[i for i in range(0, res_vnv_1_t3dhyd.ntimestep)],
                             fig_size=(12, 4),
                             fig_name='img/WaterDepth_Y5')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(12, 4),
                   fig_name='img/MeshH')

        # Plotting 3D mesh section (vertical mesh)
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0., 4.5], [46., 4.5]],
                   record=0,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(12, 4),
                   fig_name='img/MeshV')

        # Plotting bottom elevation Z at 0 (initial time step)
        vnv_plot2d('ELEVATION Z',
                   res,
                   record=0,
                   plane=0,
                   cbar_label='$z$ (m)',
                   filled_contours=True,
                   fig_size=(12, 4),
                   fig_name='img/BottomMap')

        # Plotting initial condition for free surface elevation
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0., 4.5], [46., 4.5]],
                   record=0,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(12, 4),
                   fig_name='img/FreeSurfaceInit')

        # Plotting final condition for free surface elevation (at -1)
        vnv_plot2d('ELEVATION Z',
                   res,
                   poly=[[0., 4.5], [46., 4.5]],
                   record=-1,
                   plot_mesh=True,
                   x_label='$x$ (m)', y_label='$z$ (m)',
                   fig_size=(12, 4),
                   fig_name='img/FreeSurface')

        res.close()
        res_vnv_1_t3dhyd.close()
        res_vnv_1_t3dgeo.close()

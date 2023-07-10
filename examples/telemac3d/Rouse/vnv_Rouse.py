
"""
Validation script for Rouse
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
        self.rank = 1
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # Rouse parallel mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_rouse.cas')


        # Rouse parallel mode
        cas = TelemacCas('t3d_rouse.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_rouse_par.cas',
                       cas=cas)

        del cas

        # postel3d post_treatment
        self.add_study('p3d',
                       'postel3d',
                       'p3d_rouse.cas')


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_rouse.slf',
                            eps=[1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_rouse.slf',
                            eps=[1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-5])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_history
        # Getting files
        vnv_1_t3dres = self.get_study_file('vnv_1:T3DRES')
        res_vnv_1_t3dres = TelemacFile(vnv_1_t3dres)
        res_vnv_1_t3dgeo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        res_vnv_1_t3dhyd, _ = self.get_study_res('vnv_1:T3DHYD')

        #Plotting Z on [250, 50] over records range(0, res_vnv_1_t3dres.ntimestep)
        nplan = res_vnv_1_t3dres.nplan
        node = res_vnv_1_t3dres.get_closest_node([250, 50], plane=0)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res_vnv_1_t3dres,
                'ELEVATION Z',
                nodes=[node +i*res_vnv_1_t3dres.npoin2 for i in range(nplan)],
                nodes_labels=["plane {}".format(i) for i in range(nplan)],
                fig_size=(12, 7),
                fig_name='img/timeseries')

        #TODO: Add offset to axes we cannont see the first and last plane (ax.set_aspect ?)
        node1 = res_vnv_1_t3dres.get_closest_node([250, 50], plane=nplan-1)
        vnv_plot1d_history(\
                'ELEVATION Z',
                res_vnv_1_t3dres,
                'ELEVATION Z',
                nodes=[node, node1],
                nodes_labels=['first plane', 'last plane'],
                fig_size=(12, 7),
                fig_name='img/timeseries_p1')


        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(12, 3),
                   fig_name='img/MeshH',
                   annotate_bnd=True)

        #Plotting vertical mesh at initial time step along y
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(12, 3.5),
                   fig_name='img/MeshV')

        # Plotting VELOCITY at initial time step
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=0,
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=30,
                   cbar_label='Velocity (m/s)',
                   fig_size=(12, 3.5),
                   fig_name='img/Velocityt0')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=-1,
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=30,
                   cbar_label='Velocity (m/s)',
                   fig_size=(12, 3.5),
                   fig_name='img/Velocitytf')

        # Plotting NUZ FOR VELOCITY at initial time step
        vnv_plot2d('NUZ FOR VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=0,
                   filled_contours=True,
                   cbar_label='Viscosity for velocity (m$^2$/s)',
                   fig_size=(12, 3.5),
                   fig_name='img/ViscVt0')

        # Plotting NUZ FOR VELOCITY at final time step
        vnv_plot2d('NUZ FOR VELOCITY',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=-1,
                   filled_contours=True,
                   cbar_label='Viscosity for velocity (m$^2$/s)',
                   fig_size=(12, 3.5),
                   fig_name='img/ViscVtf')

        # Plotting NUZ TRACER 1 at initial time step
        vnv_plot2d('NUZ TRACER 1',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=0,
                   filled_contours=True,
                   cbar_label='Viscosity for tracer (m$^2$/s)',
                   fig_size=(12, 3.5),
                   fig_name='img/ViscTt0')

        # Plotting NUZ TRACER 1 at final time step
        vnv_plot2d('NUZ TRACER 1',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=-1,
                   filled_contours=True,
                   cbar_label='Viscosity for tracer (m$^2$/s)',
                   fig_size=(12, 3.5),
                   fig_name='img/ViscTtf')

        # Plotting Sediment at initial time step
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=0,
                   filled_contours=True,
                   vmin=0,
                   vmax=0.105,
                   nv=8,
                   cbar_label='Sediment concentration (g/L)',
                   fig_size=(12, 3.5),
                   fig_name='img/Sedit0')

        # Plotting Sediment at final time step
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t3dres,
                   poly=[[0, 50], [500, 50]],
                   y_label='z (m)',
                   record=-1,
                   filled_contours=True,
                   cbar_label='Sediment concentration (g/L)',
                   fig_size=(12, 3.5),
                   fig_name='img/Seditf')

        # Plotting vertical split
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t3dres,
                   poly=[[450, 0], [450, 100]],
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/tracer')

        # Plotting vertical split
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t3dres,
                   poly=[[450, 0], [450, 100]],
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/velocityU_section')

        # Plotting vertical split
        vnv_plot2d('VELOCITY V',
                   res_vnv_1_t3dres,
                   poly=[[450, 0], [450, 100]],
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/velocityV_section')

        # Plotting TRACER 1 at -1
        vnv_plot2d('TRACER 1',
                   res_vnv_1_t3dres,
                   record=-1,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 7),
                   fig_name='img/tracer_tf')

        res_vnv_1_t3dres.close()
        res_vnv_1_t3dgeo.close()
        res_vnv_1_t3dhyd.close()

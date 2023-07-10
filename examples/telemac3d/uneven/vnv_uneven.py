
"""
Validation script for uneven
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
        self.rank = 4
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # uneven scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_uneven.cas')


        # uneven parallel mode
        cas = TelemacCas('t3d_uneven.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_uneven_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_uneven.slf',
                            eps=[1.E-12])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_uneven.slf',
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
        res_vnv_1_t3dres, _ = self.get_study_res('vnv_1:T3DRES')
        res_vnv_1_t3dgeo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")
        res_vnv_1_t3dhyd, _ = self.get_study_res('vnv_1:T3DHYD')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dgeo,
                   plot_mesh=True,
                   fig_size=(12, 3.5),
                   fig_name='img/MeshH',
                   annotate_bnd=True)

        #Plotting vertical mesh at initial time step along y
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0, 3], [600, 3]],
#                   x_label='y (m)',
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(12, 3.5),
                   fig_name='img/MeshV')

        # Plotting BOTTOM over polyline
        vnv_plot1d_polylines('BOTTOM',
                             res_vnv_1_t3dhyd,
                             legend_labels='bottom',
                             poly=[[0, 3], [600, 3]],
                             record=-1,
                             fig_size=(12, 3.5),
                             fig_name='img/Profil')

        # Plotting horizontal slice of bottom elevation
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=0,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(12, 3.5),
                   fig_name='img/Bottom')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 3.5),
                   fig_name='img/FreeSurface')

        #Plotting vertical mesh at initial time step along y
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0, 3], [600, 3]],
#                   x_label='y (m)',
                   y_label='z (m)',
                   record=-1,
                   plot_mesh=True,
#                   filled_contours=True,
                   fig_size=(12, 3.5),
                   fig_name='img/FreeSurfaceV')

        # Closing files
        res_vnv_1_t3dres.close()
        res_vnv_1_t3dgeo.close()
        res_vnv_1_t3dhyd.close()

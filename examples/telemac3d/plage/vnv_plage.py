
"""
Validation script for plage
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
        self.rank = 2
        self.tags = ['telemac3d']

    def _pre(self):
        """
        Defining the studies
        """

        # plage parallel mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_plage.cas')


        # plage parallel mode
        cas = TelemacCas('t3d_plage.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_plage_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_plage.slf',
                            eps=[1.E-6, 1.E-3, 1.E-4, 1.E-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_plage.slf',
                            eps=[1.E-6, 1.E-3, 1.E-4, 1.E-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6, 1.E-3, 1.E-4, 1.E-4])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d
        # Getting files
        res_vnv_1_t3dres, _ = self.get_study_res('vnv_1:T3DRES')
        res_vnv_1_t3dgeo, _ = self.get_study_res('vnv_1:T3DGEO', load_bnd=True, module="T3D")

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
                   poly=[[15, 0], [15, 5.9]],
                   x_label='y (m)',
                   y_label='z (m)',
                   record=0,
                   plot_mesh=True,
                   fig_size=(12, 3.5),
                   fig_name='img/MeshV')

        # Plotting horizontal slice of bottom elevation
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=1,
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

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 3.5),
                   fig_name='img/Velocity',
                   cbar_label='Velocity (m/s)',
                   vectors=True,
                   vectors_scale=3)

        # Closing files
        res_vnv_1_t3dres.close()
        res_vnv_1_t3dgeo.close()

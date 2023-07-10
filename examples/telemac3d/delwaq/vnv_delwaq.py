
"""
Validation script for delwaq
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

        # delwaq scalar mode
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_delwaq.cas')


        # delwaq scalar mode
        cas = TelemacCas('t3d_delwaq.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_delwaq_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_delwaq.slf',
                            eps=[1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_delwaq.slf',
                            eps=[1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d
        # Getting files
        res_vnv_1_t3dres, _ = self.get_study_res('vnv_1:T3DRES')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t3dres,
                   plot_mesh=True,
                   fig_size=(12, 8),
                   fig_name='img/MeshH')

        #Plotting vertical mesh at initial time step
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   poly=[[0, -10], [0, 10]],
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
                   vectors_scale=10,
                   fig_size=(12, 7),
                   fig_name='img/VelocityH')

        # Plotting horizontal slice of free surface
        vnv_plot2d('ELEVATION Z',
                   res_vnv_1_t3dres,
                   plane=res_vnv_1_t3dres.nplan-1,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/FreeSurface')

        res_vnv_1_t3dres.close()


"""
Validation script for friction
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
        self.tags = ['telemac2d', 'fv']

    def _pre(self):
        """
        Defining the studies
        """

        # friction channel in 2D finite volume
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_friction.cas')


        # friction channel finite volume parallel mode
        cas = TelemacCas('t2d_friction.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_friction_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_friction.slf',
                            eps=[1e-12])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_friction.slf',
                            eps=[1e-12])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-12])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot1d_polylines, vnv_plot2d
        # Getting files
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        res_vnv_1_t2dres, _ = self.get_study_res('vnv_1:T2DRES')

        # Plot water depth 1D:
        vnv_plot1d_polylines(\
            'WATER DEPTH', res_vnv_1_t2dres, 'water depth',
            fig_size=(12, 3),
            record=-1,
            poly=[[0.0, 225.0], [41000.0, 225.0]],
            ref_name='EXACT DEPTH',
            fig_name='img/figure1')

        # Plot free surface 1D:
        vnv_plot1d_polylines(\
            'FREE SURFACE', res_vnv_1_t2dres, 'elevation',
            fig_size=(13, 3),
            record=-1,
            poly=[[0.0, 225.0], [41000.0, 225.0]],
            ref_name='EXACT ELEVATION',
            fig_name='img/figure2')

        #Plotting mesh (zoom inlet)
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   xlim=[0, 10000],
                   ylim=[0, 450],
                   plot_mesh=True,
                   fig_size=(12, 2),
                   fig_name='img/Meshin')

        #Plotting mesh (zoom outlet)
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   xlim=[400000, 410000],
                   ylim=[0, 450],
                   plot_mesh=True,
                   fig_size=(12, 2),
                   fig_name='img/Meshout')

        #Plotting boundary conditions without mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
#                   plot_mesh=True,
                   fig_size=(12, 3),
                   fig_name='img/BC',
                   annotate_bnd=True)

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(12, 3),
                   fig_name='img/Bottom')

        # Plotting FREE SURFACE at final time step
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(12, 3),
                   fig_name='img/FreeSurface')

        # Plotting VELOCITY at final time step
        vnv_plot2d('VELOCITY',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Velocity (m/s)',
                   filled_contours=True,
                   vectors=True,
                   vectors_scale=35,
                   fig_size=(12, 3),
                   fig_name='img/Velocity')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()

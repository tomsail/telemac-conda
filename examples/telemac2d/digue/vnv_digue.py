
"""
Validation script for digue
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # digue scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_digue.cas')


        # digue parallel mode
        cas = TelemacCas('t2d_digue.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_digue_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_digue.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_digue.slf',
                            eps=[1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-6])


    def _post(self):
        """
        Post-treatment processes
        """

        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
#        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
#        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        res_vnv_1_t2dgeo, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(10, 8),
                   fig_name='img/Mesh')
        
        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/Bathy')

        # Plotting FREE SURFACE at 0
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurface_t0')

        # Plotting WATER DEPTH at 0
        vnv_plot2d('WATER DEPTH',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Water depth (m)',
                   filled_contours=True,
                   plot_mesh=True,
                   fig_size=(10, 8),
                   fig_name='img/WaterDepth_t0')

        # Plotting FREE SURFACE over polyline over record 0
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t2dres,
                             poly=[[0, -200], [1250, -200]],
                             record=0,
                             fig_size=(12, 5),
                             fig_name='img/FreeSurfaceSection',
                             plot_bottom=True)

        # Plotting FREE SURFACE at -1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=-1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurface_tf')

        # Plotting FREE SURFACE at time 12000
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   time=12000,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   streamlines=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurface_12000s')

        # Plotting FREE SURFACE at time 14000
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   time=14000,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   streamlines=True,
                   fig_size=(10, 8),
                   fig_name='img/FreeSurface_14000s')

        # Closing files
        res_vnv_1_t2dgeo.close()
        res_vnv_1_t2dres.close()

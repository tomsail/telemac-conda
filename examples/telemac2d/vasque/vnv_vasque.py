
"""
Validation script for vasque
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
        self.rank = 4
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # vasque scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_vasque.cas')


        # vasque parallel mode
        cas = TelemacCas('t2d_vasque.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_vasque_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_vasque.slf',
                            eps=[2.E-4, 4.E-5, 8.E-6, 8.E-6, 1.E-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_vasque.slf',
                            eps=[2.E-4, 4.E-5, 1.E-5, 1.E-5, 1.E-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[2.E-4, 4.E-5, 1.E-5, 1.E-5, 1.E-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
                # Getting files
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        vnv_1_t2dcli = self.get_study_file('vnv_1:T2DCLI')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo, bnd_file=vnv_1_t2dcli)

        vnv_plot1d_polylines(\
                'BOTTOM',
                res_vnv_1_t2dres,
                poly=[[0, 5], [46, 5]],
                record=1,
                legend_labels='bottom',
                ylim=[-0.7, 0.1],
                fig_size=(12, 5),
                fig_name='img/Bottom')

        # Plotting FREE SURFACE over polyline over records range(0, res_vnv_1_t2dres.ntimestep)
        vnv_plot1d_polylines(\
                'FREE SURFACE',
                res_vnv_1_t2dres,
                poly=[[0, 5], [46, 5]],
                record=[i for i in range(0, res_vnv_1_t2dres.ntimestep)],
                plot_bottom=True,
                legend_labels=['t=0s', 't=100s', 't=200s', 't=300s'],
                ylim=[-0.7, 0.1],
                fig_size=(12, 5),
                fig_name='img/FreeSurface_Y5')

        # Plotting WATER DEPTH over polyline over records range(0, res_vnv_1_t2dres.ntimestep)
        vnv_plot1d_polylines(\
                'WATER DEPTH',
                res_vnv_1_t2dres,
                poly=[[0, 5], [46, 5]],
                record=[i for i in range(0, res_vnv_1_t2dres.ntimestep)],
                fig_size=(12, 5),
                fig_name='img/WaterDepth_Y5')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   annotate_bnd=True,
                   fig_size=(10, 3),
                   fig_name='img/Mesh')

        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_t2dres,
                   record=0,
                   cbar_label='Bottom elevation (m)',
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/Bathy')

        # Plotting initial FREE SURFACE over polyline
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t2dres,
                             poly=[[0, 4.5], [46, 4.5]],
                             record=0,
                             ylim=[-0.7, 0.1],
                             plot_bottom=True,
                             fig_size=(12, 5),
                             fig_name='img/FreeSurface_t0')

        # Plotting final FREE SURFACE over polyline
        vnv_plot1d_polylines('FREE SURFACE',
                             res_vnv_1_t2dres,
                             poly=[[0, 4.5], [46, 4.5]],
                             record=-1,
                             ylim=[-0.7, 0.1],
                             plot_bottom=True,
                             fig_size=(12, 5),
                             fig_name='img/FreeSurface_tf')

        # Plotting FREE SURFACE at 1
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=1,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/FreeSurface_t1')

        # Plotting FREE SURFACE at 2
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=2,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/FreeSurface_t2')

        # Plotting FREE SURFACE at 3
        vnv_plot2d('FREE SURFACE',
                   res_vnv_1_t2dres,
                   record=3,
                   cbar_label='Free surface (m)',
                   filled_contours=True,
                   fig_size=(10, 3),
                   fig_name='img/FreeSurface_t3')

        # Closing files
        res_vnv_1_t2dres.close()
        res_vnv_1_t2dgeo.close()

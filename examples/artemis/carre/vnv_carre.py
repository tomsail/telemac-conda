
"""
Validation script for carre
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
        self.tags = ['artemis']

    def _pre(self):
        """
        Defining the studies
        """

        # Automatic TETAP calculation in a square domain
        self.add_study('vnv_1',
                       'artemis',
                       'art_carre.cas')


        # Automatic TETAP calculation in a square domain, parallel mode
        cas = TelemacCas('art_carre.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_carre_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_carre.slf',
                            eps=[1.e-6, 1.e-6, 1.e-8, 1.e-8, 1.e-7, 1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_carre.slf',
                            eps=[1.e-6, 1.e-6, 1.e-8, 1.e-8, 0.2, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[1.e-6, 1.e-6, 1.e-8, 1.e-8, 0.2, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)

        polys = [[[217.413, 84.862], [363.825, 154.561]],
                 [[264.673, 206.254], [318.419, 173.821]],
                 [[352.705, 203.474], [418.498, 242.394]],
                 [[377.725, 175.674], [436.104, 203.474]]]

        for i, poly in enumerate(polys):
            # Plotting WAVE HEIGHT over polyline over records 0
            vnv_plot1d_polylines(\
                    'WAVE HEIGHT',
                    res_vnv_1_artres,
                    legend_labels='wave height at t=0.0s',
                    poly=poly,
                    fig_size=(10, 7),
                    fig_name='img/Section{}'.format(i+1))

        #Plotting mesh
        #TODO: plot polylines with mesh
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(12, 8),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/WaveHeight')


        # Plotting QB at 0
        vnv_plot2d('QB',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(12, 8),
                   fig_name='img/Breaking')

        # Closing files
        res_vnv_1_artres.close()
        res_vnv_1_artgeo.close()

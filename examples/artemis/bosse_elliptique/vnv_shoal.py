
"""
Validation script for shoal
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

        # elliptic shoal scalar mode
        self.add_study('vnv_1',
                       'artemis',
                       'art_shoal.cas')


        # elliptic shoal parallel mode
        cas = TelemacCas('art_shoal.cas', get_dico('artemis'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('SOLVER', 9)


        self.add_study('vnv_2',
                       'artemis',
                       'art_shoal_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:ARTRES',
                            'f2d_shoal.slf',
                            eps=[1.e-8])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:ARTRES',
                            'f2d_shoal.slf',
                            eps=[2.e-4, 10., 1.e-4, 1.e-8, 1.e-8, 1.e-8])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:ARTRES',
                            'vnv_2:ARTRES',
                            eps=[2.e-4, 10., 1.e-4, 1.e-8, 1.e-8, 1.e-8])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d, vnv_plot1d_polylines
        # Getting files
        vnv_1_artgeo = self.get_study_file('vnv_1:ARTGEO')
        res_vnv_1_artgeo = TelemacFile(vnv_1_artgeo)
        vnv_1_artres = self.get_study_file('vnv_1:ARTRES')
        res_vnv_1_artres = TelemacFile(vnv_1_artres)

        polys = [[[10, 18], [20, 18]],
                 [[10, 17], [20, 17]],
                 [[10, 16], [20, 16]],
                 [[10, 15], [0, 15]],
                 [[10, 14], [20, 14]],
                 [[14.75, 8], [14.75, 20]],
                 [[15.75, 8], [15.75, 20]],
                 [[16.75, 8], [16.75, 20]]]

        for i, poly in  enumerate(polys):
            # Plotting WAVE HEIGHT over polyline over records 0
            vnv_plot1d_polylines(\
                    'WAVE HEIGHT',
                    res_vnv_1_artres,
                    legend_labels="wave height at t=0.0s",
                    poly=poly,
                    fig_size=(10, 7),
                    fig_name='img/Section{}'.format(i+1))

        #Plotting mesh
        #TODO: Plot mesh and the polylines ?
        vnv_plot2d('',
                   res_vnv_1_artgeo,
                   plot_mesh=True,
                   fig_size=(10, 10),
                   fig_name='img/Mesh')


        # Plotting BOTTOM at 0
        vnv_plot2d('BOTTOM',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 10),
                   fig_name='img/Bathy')


        # Plotting WAVE HEIGHT at 0
        vnv_plot2d('WAVE HEIGHT',
                   res_vnv_1_artres,
                   record=0,
                   filled_contours=True,
                   cmap_name='viridis',
                   fig_size=(10, 10),
                   fig_name='img/WaveHeight')

        # Closing files
        res_vnv_1_artgeo.close()
        res_vnv_1_artres.close()
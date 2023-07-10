
"""
Validation script for pluie
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # pluie scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_pluie.cas')


        # pluie parallel mode
        cas = TelemacCas('t2d_pluie.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_pluie_par.cas',
                       cas=cas)
        del cas

        # rain with runoff model scalar mode
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_pluie_cn.cas')


        # rain with runoff model parallel mode
        cas = TelemacCas('t2d_pluie_cn.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_pluie_cn_par.cas',
                       cas=cas)
        del cas

        # rain with hyetopgraph model scalar mode
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_pluie_cn_geo_hyetograph.cas')


        # rain with hyetopgraph model parallel mode
        cas = TelemacCas('t2d_pluie_cn_geo_hyetograph.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_pluie_cn_geo_hyetograph_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_rain_uniform.slf',
                            eps=[1.E-15])


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d,vnv_plotbar
        from data_manip.computation.volume import volume_calculation

        #======================================================================
        # GET TELEMAC RESULT FILES:
        #
        res, _ = self.get_study_res('vnv_1:T2DRES')
        res2, _ = self.get_study_res('vnv_3:T2DRES')
        res3, _ = self.get_study_res('vnv_5:T2DRES')
        ikle = res.get_mesh_connectivity()
        depth = res.get_data_value('WATER DEPTH', -1)

        volume = volume_calculation(\
                ikle,
                depth,
                res.tri.x,
                res.tri.y)

        cas = TelemacCas('t2d_pluie.cas', get_dico('telemac2d'))
        rain = cas.get('RAIN OR EVAPORATION IN MM PER DAY')
        duration = cas.get('DURATION OF RAIN OR EVAPORATION IN HOURS')
        rain_mph = rain / 24000.
        area = 10000.
        total = rain_mph * duration * area
        error = abs(total-volume[0])

        #Plot the mesh
        vnv_plot2d(\
                '',res,
                plot_mesh=True,
                fig_name='img/mesh',
                fig_size=(5,4),
                x_label='x (m)',y_label='y (m)')

        vnv_plot2d(\
            'WATER DEPTH',
            res,
            record=-1,
            fig_size=(5,4),
            fig_name='img/water-depth1',
            cbar_label='Water depth (m)',
            x_label='x (m)', y_label='y (m)',
            fig_title='error = '+str(error)+'m\u00B3',
            filled_contours=True)

        vnv_plot2d(\
            'WATER DEPTH',
            res2,
            record=-1,
            fig_size=(5,4),
            fig_name='img/water-depth2',
            cbar_label='Water depth (m)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True)

        vnv_plot2d(\
            'WATER DEPTH',
            res3,
            record=-1,
            fig_size=(5,4),
            fig_name='img/water-depth3',
            cbar_label='Water depth (m)',
            x_label='x (m)', y_label='y (m)',
            filled_contours=True)


"""
Validation script for littoral
"""
from vvytel.vnv_study import AbstractVnvStudy
from execution.telemac_cas import TelemacCas, get_dico
from data_manip.extraction.telemac_file import TelemacFile
from pretel.compute_weight import connect_tel2tom
from postel.plot_vnv import vnv_plot2d

class VnvStudy(AbstractVnvStudy):
    """
    Class for validation
    """

    def _init(self):
        """
        Defines the general parameter
        """
        self.rank = 0
        self.tags = ['telemac2d', 'gaia', 'tomawac']

    def _pre(self):
        """
        Defining the studies
        """

        # littoral T2D+TOM+GAI tel2tom same mesh

        connect_tel2tom('geo_t2d_same.slf', 'geo_tom_same.slf',
                        tel_bnd='geo_t2d_littoral.cli' ,
                        tom_bnd='geo_tom_littoral.cli')
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_tel2tom_same.cas')

        cas = TelemacCas('t2d_tel2tom_same.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_tel2tom_same_par.cas',
                       cas=cas)
        del cas

        # littoral T2D+TOM+GAI tel2tom2 different mesh
        connect_tel2tom('geo_t2d_different.slf', 'geo_tom_different.slf',
                        tel_bnd='geo_t2d_different.cli' ,
                        tom_bnd='geo_tom_different.cli')
        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_tel2tom_different.cas')

        cas = TelemacCas('t2d_tel2tom_different.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_tel2tom_different_par.cas',
                       cas=cas)
        del cas

        # littoral T2D+TOM+GAI tel2tom2 different mesh contour telemac
        connect_tel2tom('geo_t2d_contTel.slf', 'geo_tom_contTel.slf',
                        tel_bnd='geo_t2d_contTel.cli' ,
                        tom_bnd='geo_tom_contTel.cli',
                        contour_tel='maskLineTelemac.i2s')
        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_tel2tom_contTel.cas')

        cas = TelemacCas('t2d_tel2tom_contTel.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_tel2tom_contTel_par.cas',
                       cas=cas)
        del cas

        # littoral T2D+TOM+GAI tel2tom2 different mesh contour tomawac
        connect_tel2tom('geo_t2d_contTom.slf', 'geo_tom_contTom.slf',
                        tel_bnd='geo_t2d_contTom.cli' ,
                        tom_bnd='geo_tom_contTom.cli',
                        contour_tom='maskLineTomawac.i2s')
        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_tel2tom_contTom.cas')

        cas = TelemacCas('t2d_tel2tom_contTom.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_tel2tom_contTom_par.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """
# comparison on same mesh

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:WACRES',
                            'tom_ref_littoral.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:WACRES',
                            'tom_ref_littoral.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:WACRES',
                            'vnv_2:WACRES',
                            eps=[1e-9])


# comparison on different mesh

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:GAIRES',
                            'ref_gai_different.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:GAIRES',
                            'ref_gai_different.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:GAIRES',
                            'vnv_4:GAIRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'ref_t2d_different.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'ref_t2d_different.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:WACRES',
                            'ref_tom_different.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:WACRES',
                            'ref_tom_different.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:WACRES',
                            'vnv_4:WACRES',
                            eps=[1e-9])


# Comparison with contour on Telemac


        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:GAIRES',
                            'ref_gai_contTel.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:GAIRES',
                            'ref_gai_contTel.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:GAIRES',
                            'vnv_6:GAIRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T2DRES',
                            'ref_t2d_contTel.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T2DRES',
                            'ref_t2d_contTel.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:WACRES',
                            'ref_tom_contTel.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:WACRES',
                            'ref_tom_contTel.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:WACRES',
                            'vnv_6:WACRES',
                            eps=[1e-9])

        # Comparison with contour on Tomawac

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:GAIRES',
                            'ref_gai_contTom.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:GAIRES',
                            'ref_gai_contTom.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:GAIRES',
                            'vnv_8:GAIRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:T2DRES',
                            'ref_t2d_contTom.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:T2DRES',
                            'ref_t2d_contTom.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:T2DRES',
                            'vnv_8:T2DRES',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:WACRES',
                            'ref_tom_contTom.slf',
                            eps=[1e-9])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:WACRES',
                            'ref_tom_contTom.slf',
                            eps=[1e-9])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:WACRES',
                            'vnv_8:WACRES',
                            eps=[1e-9])

    def _post(self):
        """
        Post-treatment processes
        """
        # Getting files
        vnv_1_wacres = self.get_study_file('vnv_1:WACRES')
        res_vnv_1_wacres = TelemacFile(vnv_1_wacres)
        vnv_1_sisres = self.get_study_file('vnv_1:GAIRES')
        res_vnv_1_sisres = TelemacFile(vnv_1_sisres)
        vnv_1_t2dgeo = self.get_study_file('vnv_1:T2DGEO')
        res_vnv_1_t2dgeo = TelemacFile(vnv_1_t2dgeo)
        vnv_1_t2dres = self.get_study_file('vnv_1:T2DRES')
        res_vnv_1_t2dres = TelemacFile(vnv_1_t2dres)

        vnv_3_t2dgeo = self.get_study_file('vnv_3:T2DGEO')
        res_vnv_3_t2dgeo = TelemacFile(vnv_3_t2dgeo)
        vnv_3_tomgeo = self.get_study_file('vnv_3:WACGEO')
        res_vnv_3_tomgeo = TelemacFile(vnv_3_tomgeo)
        vnv_3_wacres = self.get_study_file('vnv_3:WACRES')
        res_vnv_3_wacres = TelemacFile(vnv_3_wacres)
        vnv_3_sisres = self.get_study_file('vnv_3:GAIRES')
        res_vnv_3_sisres = TelemacFile(vnv_3_sisres)
        vnv_3_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv_3_t2dres)

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_1_t2dgeo,
                   plot_mesh=True,
                   fig_size=(9, 4),
                   fig_name='img/fond')

        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsT2D')

        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_1_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsGAI')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_3_t2dgeo,
                   plot_mesh=True,
                   xlim=[-200, 1200],
                   fig_size=(9, 4),
                   fig_name='img/fondTEL2TOM')

        #Plotting mesh
        vnv_plot2d('',
                   res_vnv_3_tomgeo,
                   plot_mesh=True,
                   xlim=[-200, 1200],
                   fig_size=(9, 4),
                   fig_name='img/fondTOM2TEL')

        # Plotting VELOCITY U at -1
        vnv_plot2d('VELOCITY U',
                   res_vnv_1_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTEL2TOM')


        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_1_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsGAITEL2TOM')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_1_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM2TEL')
        # Plotting VELOCITY U at -1

        vnv_plot2d('VELOCITY U',
                   res_vnv_3_t2dres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTEL2TOMdiff')

        # Plotting BED SHEAR STRESS at -1
        vnv_plot2d('BED SHEAR STRESS',
                   res_vnv_3_sisres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsGAITEL2TOMdiff')

        # Plotting WAVE HEIGHT HM0 at -1
        vnv_plot2d('WAVE HEIGHT HM0',
                   res_vnv_3_wacres,
                   record=-1,
                   filled_contours=True,
                   fig_size=(12, 7),
                   fig_name='img/resultsTOM2TELdiff')

        # Closing files
        res_vnv_1_wacres.close()
        res_vnv_1_sisres.close()
        res_vnv_1_t2dres.close()

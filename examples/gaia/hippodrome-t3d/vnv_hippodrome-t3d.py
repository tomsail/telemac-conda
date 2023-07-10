
"""
Validation script for hippodrome-t3d
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
        self.tags = ['telemac3d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_1COs.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1COs.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_1COs_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_1COs_consolidation.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1COs_consolidation.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_1COs_consolidation_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_5',
                       'telemac3d',
                       't3d_1NCObs_1CO.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1NCObs_1CO.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac3d',
                       't3d_1NCObs_1CO_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_7',
                       'telemac3d',
                       't3d_1NCObs_1CO_consolidation.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1NCObs_1CO_consolidation.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_8',
                       'telemac3d',
                       't3d_1NCObs_1CO_consolidation_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_9',
                       'telemac3d',
                       't3d_4NCObs_4CO_consolidation.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_4NCObs_4CO_consolidation.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_10',
                       'telemac3d',
                       't3d_4NCObs_4CO_consolidation_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_11',
                       'telemac3d',
                       't3d_1NCObs.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1NCObs.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_12',
                       'telemac3d',
                       't3d_1NCObs_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_13',
                       'telemac3d',
                       't3d_1NCOs_1CO.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1NCOs_1CO.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_14',
                       'telemac3d',
                       't3d_1NCOs_1CO_par.cas',
                       cas=cas)
        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_15',
                       'telemac3d',
                       't3d_1NCOs.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_1NCOs.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_16',
                       'telemac3d',
                       't3d_1NCOs_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_17',
                       'telemac3d',
                       't3d_4NCObs_4CO.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_4NCObs_4CO.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_18',
                       'telemac3d',
                       't3d_4NCObs_4CO_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_19',
                       'telemac3d',
                       't3d_4NCObs.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_4NCObs.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_20',
                       'telemac3d',
                       't3d_4NCObs_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_21',
                       'telemac3d',
                       't3d_4NCOs_4CO.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_4NCOs_4CO.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_22',
                       'telemac3d',
                       't3d_4NCOs_4CO_par.cas',
                       cas=cas)

        del cas


        # hippodrome scalar mode T3D+GAI
        self.add_study('vnv_23',
                       'telemac3d',
                       't3d_4NCOs.cas')


        # hippodrome parallel mode T3D+GAI
        cas = TelemacCas('t3d_4NCOs.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_24',
                       'telemac3d',
                       't3d_4NCOs_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:GAIRES',
                            'gai_ref_1COs.slf',
                            eps=[1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.1,
                                 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:GAIRES',
                            'gai_ref_1COs.slf',
                            eps=[1.E-5])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:GAIRES',
                            'vnv_2:GAIRES',
                            eps=[1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.1,
                                 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T3DRES',
                            'f3d_1COs.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T3DRES',
                            'f3d_1COs.slf',
                            eps=[1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T3DRES',
                            'vnv_2:T3DRES',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:GAIRES',
                            'gai_ref_1COs_consolidation.slf',
                            eps=[5.E-4])
        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:GAIRES',
                            'gai_ref_1COs_consolidation.slf',
                            eps=[5.E-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:GAIRES',
                            'vnv_4:GAIRES',
                            eps=[5.E-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T3DRES',
                            'f3d_1COs_consolidation.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T3DRES',
                            'f3d_1COs_consolidation.slf',
                            eps=[1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T3DRES',
                            'vnv_4:T3DRES',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:GAIRES',
                            'gai_ref_1NCObs_1CO.slf',
                            eps=[1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.6E-2,
                                 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5,
                                 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5, 1.E-5])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:GAIRES',
                            'gai_ref_1NCObs_1CO.slf',
                            eps=[1.E-1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:GAIRES',
                            'vnv_6:GAIRES',
                            eps=[1.E-1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_5:T3DRES',
                            'f3d_1NCObs_1CO.slf',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_6:T3DRES',
                            'f3d_1NCObs_1CO.slf',
                            eps=[1.E-6])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_5:T3DRES',
                            'vnv_6:T3DRES',
                            eps=[1.E-6])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:GAIRES',
                            'gai_ref_1NCObs_1CO_consolidation.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:GAIRES',
                            'gai_ref_1NCObs_1CO_consolidation.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:GAIRES',
                            'vnv_8:GAIRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_7:T3DRES',
                            'f3d_1NCObs_1CO_consolidation.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_8:T3DRES',
                            'f3d_1NCObs_1CO_consolidation.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_7:T3DRES',
                            'vnv_8:T3DRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_9:GAIRES',
                            'gai_ref_4NCObs_4CO_consolidation.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_10:GAIRES',
                            'gai_ref_4NCObs_4CO_consolidation.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_9:GAIRES',
                            'vnv_10:GAIRES',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_9:T3DRES',
                            'f3d_4NCObs_4CO_consolidation.slf',
                            eps=[])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_10:T3DRES',
                            'f3d_4NCObs_4CO_consolidation.slf',
                            eps=[])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_9:T3DRES',
                            'vnv_10:T3DRES',
                            eps=[])


    def _post(self):
        """
        Post-treatment processes
        """


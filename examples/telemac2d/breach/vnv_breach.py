
"""
Validation script for breach
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
        self.tags = ['telemac2d']

    def _pre(self):
        """
        Defining the studies
        """

        # breach scalar mode
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_breach.cas')


        # breach parallel mode
        cas = TelemacCas('t2d_breach.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_breach_par.cas',
                       cas=cas)

        # two steps scalar mode
        cas = TelemacCas('t2d_breach.cas', get_dico('telemac2d'))
        cas.set('BREACHES DATA FILE','breach_2steps.txt')

        self.add_study('vnv_2steps',
                       'telemac2d',
                       't2d_breach_2steps.cas',
                       cas=cas)

        # two steps parallel mode
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2steps_paral',
                       'telemac2d',
                       't2d_breach_2steps_par.cas',
                       cas=cas)

        # USBR and Von Thun scalar mode
        cas = TelemacCas('t2d_breach.cas', get_dico('telemac2d'))
        cas.set('BREACHES DATA FILE','breach_USBR_VonThun.txt')

        self.add_study('vnv_USBR_VonThun',
                       'telemac2d',
                       't2d_breach_USBR_VonThun.cas',
                       cas=cas)

        # USBR and Von Thun parallel mode
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_USBR_VonThun_paral',
                       'telemac2d',
                       't2d_breach_USBR_VonThun_par.cas',
                       cas=cas)

        # Verheij2002 and Verheij2003 scalar mode
        cas = TelemacCas('t2d_breach.cas', get_dico('telemac2d'))
        cas.set('BREACHES DATA FILE','breach_Verheij2002_Verheij2003.txt')

        self.add_study('vnv_Verheij2002_Verheij2003',
                       'telemac2d',
                       't2d_breach_Verheij2002_Verheij2003.cas',
                       cas=cas)

        # Verheij2002 and Verheij2003 parallel mode
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_Verheij2002_Verheij2003_paral',
                       'telemac2d',
                       't2d_breach_Verheij2002_Verheij2003_par.cas',
                       cas=cas)

        # Froehlich scalar mode
        cas = TelemacCas('t2d_breach.cas', get_dico('telemac2d'))
        cas.set('BREACHES DATA FILE','breach_Froehlich.txt')

        self.add_study('vnv_Froehlich',
                       'telemac2d',
                       't2d_breach_Froehlich.cas',
                       cas=cas)

        # Froehlich parallel mode
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_Froehlich_paral',
                       'telemac2d',
                       't2d_breach_Froehlich_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_breach.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2:T2DRES',
                            'f2d_breach.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2steps:T2DRES',
                            'f2d_breach_2steps.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_2steps_paral:T2DRES',
                            'f2d_breach_2steps.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_2steps:T2DRES',
                            'vnv_2steps_paral:T2DRES',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_USBR_VonThun:T2DRES',
                            'f2d_breach_USBR_VonThun.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_USBR_VonThun_paral:T2DRES',
                            'f2d_breach_USBR_VonThun.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_USBR_VonThun:T2DRES',
                            'vnv_USBR_VonThun_paral:T2DRES',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_Verheij2002_Verheij2003:T2DRES',
                            'f2d_breach_Verheij2002_Verheij2003.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_Verheij2002_Verheij2003_paral:T2DRES',
                            'f2d_breach_Verheij2002_Verheij2003.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_Verheij2002_Verheij2003:T2DRES',
                            'vnv_Verheij2002_Verheij2003_paral:T2DRES',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_Froehlich:T2DRES',
                            'f2d_breach_Froehlich.slf',
                            eps=[1e-4])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_Froehlich_paral:T2DRES',
                            'f2d_breach_Froehlich.slf',
                            eps=[1e-4])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_Froehlich:T2DRES',
                            'vnv_Froehlich_paral:T2DRES',
                            eps=[1e-4])

    def _post(self):
        """
        Post-treatment processes
        """


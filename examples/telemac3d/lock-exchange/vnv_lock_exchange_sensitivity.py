
"""
Validation script for gouttedo
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
        self.tags = ['telemac3d', 'full_valid']

    def _pre(self):
        """
        Defining the studies
        """

        # lock-exchange CARAC
        cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_CARAC.slf')
        cas.set('SCHEME FOR ADVECTION OF VELOCITIES', 1)
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 1)
        cas.remove('SCHEME OPTION FOR ADVECTION OF VELOCITIES')
        cas.remove('SCHEME OPTION FOR ADVECTION OF TRACERS')
        cas.remove('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES')
        cas.remove('DYNAMIC PRESSURE IN WAVE EQUATION')

        self.add_study('vnv_1',
                       'telemac3d',
                       't3d_lock_CARAC_par.cas',
                       cas=cas)

        del cas


        # lock-exchange SUPG
        cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_SUPG.slf')
        cas.set('SCHEME FOR ADVECTION OF VELOCITIES', 2)
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 2)
        cas.set('SUPG OPTION', [2, 0, 2])
        cas.remove('SCHEME OPTION FOR ADVECTION OF VELOCITIES')
        cas.remove('SCHEME OPTION FOR ADVECTION OF TRACERS')
        cas.remove('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES')
        cas.remove('DYNAMIC PRESSURE IN WAVE EQUATION')


        self.add_study('vnv_2',
                       'telemac3d',
                       't3d_lock_SUPG_par.cas',
                       cas=cas)

        del cas


        # lock-exchange LEOP
        cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_LEOP.slf')
        cas.set('SCHEME FOR ADVECTION OF VELOCITIES', 3)
        cas.set('SCHEME FOR ADVECTION OF TRACERS', 3)
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 1)
        cas.remove('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES')
        cas.remove('DYNAMIC PRESSURE IN WAVE EQUATION')

        self.add_study('vnv_3',
                       'telemac3d',
                       't3d_lock_LEOP_par.cas',
                       cas=cas)

        del cas


        # lock-exchange PSI1
        cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_PSI1.slf')
        cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 1)
        cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 1)
        cas.remove('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES')
        cas.set('DYNAMIC PRESSURE IN WAVE EQUATION', False)

        self.add_study('vnv_4',
                       'telemac3d',
                       't3d_lock_PSI1_par.cas',
                       cas=cas)

        del cas


        # lock-exchange PSI2 2-3-4-5 corrections
        for corr in range(2, 6):
            cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
            cas.set('PARALLEL PROCESSORS', 4)
            cas.set('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES', corr)
            cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_PSI2_{}corr.slf'.format(corr))
            cas.set('DYNAMIC PRESSURE IN WAVE EQUATION', False)

            self.add_study('vnv_{}'.format(corr+3),
                           'telemac3d',
                           't3d_lock_PSI2_{}corr_par.cas'.format(corr),
                           cas=cas)

            del cas

        # lock-exchange PSI3 2-3-4-5 corrections
        for corr in range(2, 6):
            cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
            cas.set('PARALLEL PROCESSORS', 4)
            cas.set('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES', corr)
            cas.set('SCHEME OPTION FOR ADVECTION OF VELOCITIES', 3)
            cas.set('SCHEME OPTION FOR ADVECTION OF TRACERS', 3)
            cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_PSI3_{}corr.slf'.format(corr))
            cas.set('DYNAMIC PRESSURE IN WAVE EQUATION', False)

            self.add_study('vnv_{}'.format(corr+7),
                           'telemac3d',
                           't3d_lock_PSI3_{}corr_par.cas'.format(corr),
                           cas=cas)

            del cas


        # lock-exchange DPWAVEQ=YES PSI2 2-3-4 corrections
        for corr in range(2, 5):
            cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
            cas.set('PARALLEL PROCESSORS', 4)
            cas.set('NUMBER OF CORRECTIONS OF DISTRIBUTIVE SCHEMES', corr)
            cas.set('3D RESULT FILE', 'r3d_lock-nonhydro_dpwaveq_PSI2_{}corr.slf'.format(corr))

            self.add_study('vnv_{}'.format(corr+11),
                           'telemac3d',
                           't3d_lock_dpwave_q_PSI2_{}corr_par.cas'.format(corr),
                           cas=cas)

            del cas

        # lock-exchange hydrostatic
        cas = TelemacCas('t3d_lock-exchange.cas', get_dico('telemac3d'))
        cas.set('PARALLEL PROCESSORS', 4)
        cas.set('3D RESULT FILE', 'r3d_lock-hydro.slf')
        cas.set('TITLE', 'Hydrostatic lock exchange')
        cas.remove('SOLVER FOR PPE')
        cas.remove('SOLVER FOR DIFFUSION OF TRACERS')
        cas.remove('SOLVER FOR DIFFUSION OF VELOCITIES')
        cas.remove('MAXIMUM NUMBER OF ITERATIONS FOR PPE')
        cas.remove('ACCURACY FOR PPE')
        cas.set('NON-HYDROSTATIC VERSION', False)
        cas.remove('DYNAMIC PRESSURE IN WAVE EQUATION')

        self.add_study('vnv_16',
                       'telemac3d',
                       't3d_lock-hydro_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """


    def _post(self):
        """
        Post-treatment processes
        """
        from postel.plot_vnv import vnv_plot2d

        for res_name, name in [('vnv_1:T3DRES', 'CARAC'),
                               ('vnv_2:T3DRES', 'SUPG'),
                               ('vnv_3:T3DRES', 'LEOP'),
                               ('vnv_4:T3DRES', 'PSI1'),
                               ('vnv_5:T3DRES', 'PSI2_2corr'),
                               ('vnv_6:T3DRES', 'PSI2_3corr'),
                               ('vnv_7:T3DRES', 'PSI2_4corr'),
                               ('vnv_8:T3DRES', 'PSI2_5corr'),
                               ('vnv_9:T3DRES', 'PSI3_2corr'),
                               ('vnv_10:T3DRES', 'PSI3_3corr'),
                               ('vnv_11:T3DRES', 'PSI3_4corr'),
                               ('vnv_12:T3DRES', 'PSI3_5corr'),
                               ('vnv_13:T3DRES', 'dpwaveq_PSI2_2corr'),
                               ('vnv_14:T3DRES', 'dpwaveq_PSI2_3corr'),
                               ('vnv_15:T3DRES', 'dpwaveq_PSI2_4corr'),
                               ('vnv_16:T3DRES', 'hydro')]:
            res = TelemacFile(self.get_study_file(res_name))

            # Plotting vertical split
            vnv_plot2d('SALINITY',
                       res,
                       poly=[[0, 1], [30, 1]],
                       y_label='z (m)',
                       record=-1,
                       cbar_label='Salinity (g/L)',
                       filled_contours=True,
                       fig_size=(20, 10),
                       xlim=(0, 30),
                       ylim=(-4, 0),
                       fig_name='img/lock-exchange_{}'.format(name))
            res.close()

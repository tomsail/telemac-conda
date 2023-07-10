"""
Validation script for guenter
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
        self.tags = ['telemac2d', 'gaia']

    def _pre(self):
        """
        Defining the studies
        """

        # Guenter exp 3 scalar mode
        self.add_study('vnv_seq',
                       'telemac2d',
                       't2d_guenter.cas')


        # Guenter exp 3 parallel mode
        cas = TelemacCas('t2d_guenter.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_par',
                       'telemac2d',
                       't2d_guenter_par.cas',
                       cas=cas)

        # Guenter exp 5 scalar mode
        cas = TelemacCas('t2d_guenter.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE','geo_guenter_5.slf')
        cas.set('PREVIOUS COMPUTATION FILE','guenter_t2d_init_5.slf')
        cas.set('PRESCRIBED FLOWRATES',[0.,0.0310])
        cas.set('PRESCRIBED ELEVATIONS',[1.0653,0.])
        sed_cas = TelemacCas('gai_guenter.cas', get_dico('gaia'))
        sed_cas.set('CLASSES SEDIMENT DIAMETERS',[3.54e-4,0.708e-3,1.135e-3,2.232e-3,5.534e-3])
        sed_cas.set('GEOMETRY FILE','geo_guenter_5.slf')
        sed_cas.write('gai_tmp.cas')
        cas.set('GAIA STEERING FILE','gai_tmp.cas')

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_guenter_exp5.cas',
                       cas=cas)

        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_guenter_exp5_par.cas',
                       cas=cas)

        del cas



    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:GAIRES',
                            'gai_ref_guenter.slf',
                            eps=[4e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:GAIRES',
                            'gai_ref_guenter.slf',
                            eps=[0.1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:GAIRES',
                            'vnv_par:GAIRES',
                            eps=[0.1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_seq:T2DRES',
                            'f2d_guenter.slf',
                            eps=[7e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_par:T2DRES',
                            'f2d_guenter.slf',
                            eps=[1e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_seq:T2DRES',
                            'vnv_par:T2DRES',
                            eps=[1e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:GAIRES',
                            'gai_ref_guenter_5.slf',
                            eps=[5e-2])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:GAIRES',
                            'gai_ref_guenter_5.slf',
                            eps=[0.1])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:GAIRES',
                            'vnv_4:GAIRES',
                            eps=[0.1])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_guenter_5.slf',
                            eps=[9e-3])

        # Comparison with the last time frame of the reference file.
        self.check_epsilons('vnv_4:T2DRES',
                            'f2d_guenter_5.slf',
                            eps=[1e-2])

        # Comparison between sequential and parallel run.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1e-2])


    def _post(self):
        """
        Post-treatment processes
        """


        from data_manip.extraction.telemac_file import TelemacFile
        from postel.plot_vnv import vnv_plot2d
        from postel.plot1d import plot1d
        import matplotlib.pyplot as plt
        import numpy as np

        vnv3_t2dres = self.get_study_file('vnv_seq:T2DRES')
        res_vnv_3_t2dres = TelemacFile(vnv3_t2dres)

        vnv3_gaires = self.get_study_file('vnv_seq:GAIRES')
        res_vnv_3_gaires = TelemacFile(vnv3_gaires)

        vnv5_t2dres = self.get_study_file('vnv_3:T2DRES')
        res_vnv_5_t2dres = TelemacFile(vnv5_t2dres)

        vnv5_gaires = self.get_study_file('vnv_3:GAIRES')
        res_vnv_5_gaires = TelemacFile(vnv5_gaires)

        exp = ['3','5']
        res_t2d = [res_vnv_3_t2dres,res_vnv_5_t2dres]
        res_gai = [res_vnv_3_gaires,res_vnv_5_gaires]
        cas_gai = ['gai_guenter.cas','gai_tmp.cas']
        slope = [2.327e-3,3.309e-3]
        x0 = [16.,55.]
        z0 = [None,None]

        for j in range(2):

             # Plotting bottom at record 0 with mesh
             vnv_plot2d('BOTTOM',
                        res_t2d[j],
                        record=0,
                        filled_contours=True,
                        plot_mesh=True,
                        fig_size=(15,3),
                        cbar_label='Bottom Elevation (m)',
                        fig_name='img/bottom'+exp[j])

             # Plotting bottom elevation at final time
             vnv_plot2d('BOTTOM',
                        res_t2d[j],
                        record=-1,
                        filled_contours=True,
                        fig_size=(15,3),
                        cbar_label='Bottom Elevation (m)',
                        fig_name='img/bottom_final'+exp[j])

             # Plotting bottom evolution at final time
             vnv_plot2d('CUMUL BED EVOL',
                        res_gai[j],
                        record=-1,
                        filled_contours=True,
                        fig_size=(15,3),
                        cbar_label='Bottom Evolution (m)',
                        fig_name='img/bottom_evol'+exp[j])

             #Plotting elevation of the bottom at the end of the simulation vs measurements
             fig, ax = plt.subplots(figsize=(10,3))

             poly_points = [[16.,0.5],[55,0.5]]

             poly_number =[50]

             poly_coord, abs_curv,bottom=res_t2d[j].get_timeseries_on_polyline('BOTTOM', poly_points, poly_number)
             poly_coord, abs_curv,water_depth=res_t2d[j].get_timeseries_on_polyline('WATER DEPTH', poly_points, poly_number)
             water_elevation=bottom+water_depth

             poly_coord, abs_curv,bottom=res_t2d[j].get_timeseries_on_polyline('BOTTOM', poly_points, poly_number)

             plot1d(ax, poly_coord[:,0],bottom[:,0],
                     plot_label='Initial bed elevation')

             plot1d(ax, poly_coord[:,0],bottom[:,-1],
                     plot_label='Simulated final bed elevation')

             plot1d(ax, poly_coord[:,0],water_elevation[:,-1],
                     plot_label='Simulated final free surface elevation')

             #Compute experimental bed elevation with equilibrium slope
             z0[1] = bottom[-1,-1]
             z0[0] = z0[1] + slope[j]*39.

             plot1d(ax, x0, z0,
                     plot_label='Experimental bed elevation')

             ax.legend()
             print(" "*8+"~> Plotting img/bed_evol"+exp[j])
             plt.savefig('img/bed_evol'+exp[j]+'.png')
             plt.close('all')

             #Plotting sediment distribution
             exp3_dist = np.loadtxt('data/exp'+exp[j]+'_dist.csv', delimiter=";", comments='#')
             point=[[50.,0.5]]

             cas = TelemacCas(cas_gai[j], get_dico('gaia'))
             distrib = cas.get('CLASSES SEDIMENT DIAMETERS')
             del cas

             c=[]
             c_init=[]
             c.append(0)
             c_init.append(0)
             for i in range(len(distrib)-1):
                 c.append(c[i]+res_gai[j].get_data_on_points('LAY1 SAND RAT'+str(i+1),-1,point))
                 c_init.append(c_init[i]+res_gai[j].get_data_on_points('LAY1 SAND RAT'+str(i+1),0,point))
             c.append(1)
             c_init.append(1)

             distrib_OK=[]
             distrib_OK.append(0)
             for i in range(len(distrib)-1):
                 distrib_OK.append((distrib[i]+distrib[i+1])/2.)
             distrib_OK.append(6e-3)

             fig, ax = plt.subplots()
             plot1d(ax, distrib_OK,c,
                     plot_label='Simulated final distribution')

             plot1d(ax, distrib_OK,c_init,
                     plot_label='Initial distribution')

             plot1d(ax, exp3_dist[:,0],exp3_dist[:,1],
                     x_label='Sediment diameter (m)',
                     y_label='%',
                     plot_label='Measured final distribution')

             ax.legend()
             print(" "*8+"~> Plotting img/distrib"+exp[j])
             plt.savefig('img/distrib'+exp[j]+'.png')
             plt.close('all')


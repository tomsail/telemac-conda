
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
        self.rank = 1
        self.tags = ['fv','telemac2d']
        self.listing = True

    def _pre(self):
        """
        Defining the studies
        """

        # 2% slope
        self.add_study('vnv_1',
                       'telemac2d',
                       't2d_rain_runoff.cas')


        # 2% slope parallel
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_2',
                       'telemac2d',
                       't2d_rain_runoff_par.cas',
                       cas=cas)

        # 5% slope
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', 'geo_flume_5p.slf')

        self.add_study('vnv_3',
                       'telemac2d',
                       't2d_rain_runoff_5p.cas',
                       cas=cas)

        # 5% slope parallel
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', 'geo_flume_5p.slf')
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_4',
                       'telemac2d',
                       't2d_rain_runoff_5p_par.cas',
                       cas=cas)

        # 25% slope
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', 'geo_flume_25p.slf')

        self.add_study('vnv_5',
                       'telemac2d',
                       't2d_rain_runoff_25p.cas',
                       cas=cas)

        # 25% slope parallel
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', 'geo_flume_25p.slf')
        cas.set('PARALLEL PROCESSORS', 4)

        self.add_study('vnv_6',
                       'telemac2d',
                       't2d_rain_runoff_25p_par.cas',
                       cas=cas)

        # 2% slope classical reconstruction
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('OPTION OF THE HYDROSTATIC RECONSTRUCTION', 1)

        self.add_study('vnv_7',
                       'telemac2d',
                       't2d_rain_runoff_2p_cr.cas',
                       cas=cas)

        # 5% slope classical reconstruction
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', 'geo_flume_5p.slf')
        cas.set('OPTION OF THE HYDROSTATIC RECONSTRUCTION', 1)

        self.add_study('vnv_8',
                       'telemac2d',
                       't2d_rain_runoff_5p_cr.cas',
                       cas=cas)

        # 25% slope parallel classical
        cas = TelemacCas('t2d_rain_runoff.cas', get_dico('telemac2d'))
        cas.set('GEOMETRY FILE', 'geo_flume_25p.slf')
        cas.set('OPTION OF THE HYDROSTATIC RECONSTRUCTION', 1)

        self.add_study('vnv_9',
                       'telemac2d',
                       't2d_rain_runoff_25p_cr.cas',
                       cas=cas)
        del cas


    def _check_results(self):
        """
        Post-treatment processes
        """

        # Comparison with reference file
        self.check_epsilons('vnv_1:T2DRES',
                            'f2d_rain_runoff_2p.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_1:T2DRES',
                            'vnv_2:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_3:T2DRES',
                            'f2d_rain_runoff_5p.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_3:T2DRES',
                            'vnv_4:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_5:T2DRES',
                            'f2d_rain_runoff_25p.slf',
                            eps=[1.E-15])

        # Comparison seq/par.
        self.check_epsilons('vnv_5:T2DRES',
                            'vnv_6:T2DRES',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_7:T2DRES',
                            'f2d_rain_runoff_2p_cr.slf',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_8:T2DRES',
                            'f2d_rain_runoff_5p_cr.slf',
                            eps=[1.E-15])

        # Comparison with reference file
        self.check_epsilons('vnv_9:T2DRES',
                            'f2d_rain_runoff_25p_cr.slf',
                            eps=[1.E-15])

    def _post(self):
        """
        Post-treatment processes
        """
        import numpy as np
        from data_manip.computation.polyline_integrals import flux_2d
        from postel.plot1d import plot1d
        from postel.plot_vnv import vnv_plot2d
        import matplotlib.pyplot as plt
        from postel.parser_output import get_latest_output_files
        from postel.parser_output import OutputFileData
        import re


        liste = ['1','3','5','7','8','9']
        flux = []
        mass = []
        times = []

        for i in liste:

            #Extract mass fluxes from the listing
            t_form = re.compile(r'\s*FLUX BOUNDARY', re.I) # Search the time

            cas_file = self.studies['vnv_'+i].steering_file
            file_name = get_latest_output_files(cas_file)
            file_name = file_name[0]
            out_file = OutputFileData(file_name)

            my_grep_t = out_file.get_user_defined_output(t_form)
            my_grep = out_file.get_time_profile()
            mass_flux = []
            mass_flux.append(0.)
            if i=='1':
                mass_flux.append(0.)
            for j in my_grep_t:
                mass_flux.append(-float(j.split()[3]))
            mass.append(mass_flux)
            times.append(my_grep[1][1])

            #Get T2D results file
            res, _ = self.get_study_res('vnv_'+i+':T2DRES')

            #Extract discharge at the outlet for each slope
            line = [np.array([4.9,0.0]),np.array([4.9,1.])]
            line_num = [20]

            l, _, h = res.get_timeseries_on_polyline('WATER DEPTH',line,line_num)
            l, _, u = res.get_timeseries_on_polyline('VELOCITY U',line,line_num)
            l, _, v = res.get_timeseries_on_polyline('VELOCITY V',line,line_num)

            #Compute the output discharges
            flux_x = np.multiply(h,u)
            flux_y = np.multiply(h,v)
            flux.append(flux_2d(l,flux_x,flux_y))

            #Plot the bottom
            if i=='1':
                vnv_plot2d(\
                        'BOTTOM',
                        res,
                        fig_name='img/bottom_2p',
                        fig_size=(10,4),
                        cbar_label='Bottom elevation (m)',
                        filled_contours=True,
                        x_label='x (m)',y_label='y (m)')
            if i=='3':
                vnv_plot2d(\
                        'BOTTOM',
                        res,
                        fig_name='img/bottom_5p',
                        fig_size=(10,4),
                        cbar_label='Bottom elevation (m)',
                        filled_contours=True,
                        x_label='x (m)',y_label='y (m)')
            if i=='5':
                vnv_plot2d(\
                        'BOTTOM',
                        res,
                        fig_name='img/bottom_25p',
                        fig_size=(10,4),
                        cbar_label='Bottom elevation (m)',
                        filled_contours=True,
                        x_label='x (m)',y_label='y (m)')



        #Plot the mesh
        vnv_plot2d(\
                '',res,
                plot_mesh=True,
                fig_name='img/mesh',
                fig_size=(10,4),
                x_label='x (m)',y_label='y (m)')

        #Plot the boundary conditions
        geom, _ = self.get_study_res('vnv_1:T2DGEO', load_bnd=True)
        vnv_plot2d(\
                '',geom,
                plot_mesh=True,
                fig_name='img/bcd',
                fig_size=(10,4),
                x_label='x (m)',y_label='y (m)',
                annotate_bnd=True)

        #Extract data from csv
        csv_file = './data/qouta2.d'
        qouta = np.loadtxt(csv_file, delimiter=";", comments='#')
        csv_exp = './data/q2.out'
        qoutexp = np.loadtxt(csv_exp, delimiter=";", comments='#')

        #Initialising figure
        fig, ax = plt.subplots(figsize=(10,5))

        #Plot for slope = 2%
        plot1d(ax,res.times,flux[0],plot_label='Chen reco hu')
        plot1d(ax,res.times,flux[3],plot_label='Classical reco hu')
        plot1d(ax,times[0],mass[0],plot_label='Chen mass fluxes')
        plot1d(ax,times[3],mass[3],plot_label='Classical mass fluxes')
        plot1d(ax,qouta[:,0],qouta[:,1],plot_label='Analytical sol')
        plot1d(ax,qoutexp[:,0],qoutexp[:,1]*1e-6/0.115,plot_label='Experimental data',\
                x_label='Time (s)',y_label='Outlet discharges m\u00B3/s')

        ax.legend()
        fig_name = 'img/qout_2p'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Extract data from csv
        csv_file = './data/qouta5.d'
        qouta = np.loadtxt(csv_file, delimiter=";", comments='#')
        csv_exp = './data/q5.out'
        qoutexp = np.loadtxt(csv_exp, delimiter=";", comments='#')

        #Initialising figure
        fig, ax = plt.subplots(figsize=(10,5))

        #Plot for slope = 5%
        plot1d(ax,res.times,flux[1],plot_label='Chen reco')
        plot1d(ax,res.times,flux[4],plot_label='Classical reco')
        plot1d(ax,times[1],mass[1],plot_label='Chen mass fluxes')
        plot1d(ax,times[4],mass[4],plot_label='Classical mass fluxes')
        plot1d(ax,qouta[:,0],qouta[:,1],plot_label='Analytical sol')
        plot1d(ax,qoutexp[:,0],qoutexp[:,1]*1e-6/0.115,plot_label='Experimental data',\
                x_label='Time (s)',y_label='Outlet discharges m\u00B3/s')

        ax.legend()
        fig_name = 'img/qout_5p'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')

        #Extract data from csv
        csv_file = './data/qouta25.d'
        qouta = np.loadtxt(csv_file, delimiter=";", comments='#')
        csv_exp = './data/q25.out'
        qoutexp = np.loadtxt(csv_exp, delimiter=";", comments='#')

        #Initialising figure
        fig, ax = plt.subplots(figsize=(10,5))

        #Plot for slope = 25%
        plot1d(ax,res.times,flux[2],plot_label='Chen reco')
        plot1d(ax,res.times,flux[5],plot_label='Classical reco')
        plot1d(ax,times[2],mass[2],plot_label='Chen mass fluxes')
        plot1d(ax,times[5],mass[5],plot_label='Classical mass fluxes')
        plot1d(ax,qouta[:,0],qouta[:,1],plot_label='Analytical sol')
        plot1d(ax,qoutexp[:,0],qoutexp[:,1]*1e-6/0.115,plot_label='Experimental data',\
                x_label='Time (s)',y_label='Outlet discharges m\u00B3/s')

        ax.legend()
        fig_name = 'img/qout_25p'
        print(" "*8+'~> Plotting '+fig_name)
        plt.savefig(fig_name)
        plt.close('all')


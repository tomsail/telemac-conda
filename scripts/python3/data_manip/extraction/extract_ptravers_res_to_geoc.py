#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author TELEMAC-MASCARET Consortium
@brief Extract from ptravers Courlis res file to Courlis geometry file at a
given record
"""

import argparse
from data_manip.formats.mascaret_file import MascaretFile, Reach
from data_manip.formats.mascaretgeo_file import MascaretGeoFile
import os
import shutil
import datetime
import copy


def extract_ptravers_res_to_geoc(name,
                                 output_name,
                                 records,
                                 all_records,
                                 destination_folder):
    """
    Extract from ptravers Courlis res file to Croulise geometry file at a given
    record

    @param name (str) Name of the file from which to extract
    @param output_name (str) Name of the output file
    @param records (int) Records to extract
    @param all_records (bool) Option to extract all records
    @param destination_folder (string) Path of the folder containing all extracted
           eoC if all_records is True
    """

    courlis_ptravers = MascaretFile(name, fformat='ptravers')

    # minus 4 because there are 4 variables in addition to layers elevation
    # minus 1 because it is elevation of layer, river bottom and hard bottom,
    # not thicknesses of layers
    nlayers = courlis_ptravers.nsectionvar - 4 - 1

    section_vars_indexes = [i for i in range(0, nlayers + 2)]

    if all_records:
        records = [i for i in range(courlis_ptravers.ntimestep)]
        if os.path.exists(destination_folder):
            date = datetime.datetime.now()
            shutil.move(destination_folder, destination_folder + '_old_' +  str(date.year)
                                                               + '-' + str(date.month)
                                                               + '-' + str(date.day)
                                                               + '_' + str(date.hour)
                                                               + 'h' + str(date.minute)
                                                               + 'm' + str(date.second)
                                                               + 's')
        os.mkdir(destination_folder)

    else:
        destination_folder = "."

    if type(records) == int:
        records = [records]

    for record in records:
        if output_name.split(".")[-1] is not "geoC":
            output_name = output_name + ".geoC"

        if output_name is '_prev.geoC':
            output_name = '{}_{}_time_{}s.geoC'.format(short_name,
                                                       record,
                                                       courlis_ptravers.times[record])
        elif all_records:
            short_name = ".".join(name.split(".")[:-1])

            output_name = os.path.join(destination_folder,
                                       '{}_{}_time_{}s.geoC'.format(short_name,
                                           record,
                                           courlis_ptravers.times[record]))

        courlis_geo = []
        courlis_geo = MascaretGeoFile(output_name, mode='write')

        _, layers_elevation = courlis_ptravers.get_values(
                                 record,
                                 get_section_values=True,
                                 section_vars_indexes=section_vars_indexes)



        print('Extracting time {}s from {} to {}'
              .format(courlis_ptravers.times[record], name, output_name))

        # Only one reach in Courlis
        reach = Reach(1, "Reach")
        courlis_geo.has_layers = True
        courlis_geo.nlayers = nlayers
        for i in range(nlayers+1):
            layer = courlis_ptravers.section_varnames_dict['names'][i+1]
            courlis_geo.layer_names.append(layer)

        for i in range(courlis_ptravers.nsections):

            section = copy.deepcopy(courlis_ptravers.reaches[1].sections[i+1])
            dist = layers_elevation[1][i][0]
            z_list = layers_elevation[1][i][1]
            section.set_points_from_trans(dist, z_list)

            for j in range(nlayers):
                thickness_table = layers_elevation[1][i][j+1] - \
                                  layers_elevation[1][i][j+2]
                print(j, len(thickness_table))
                section.add_layer(thickness_table)

            reach.add_section(section)

        courlis_geo.add_reach(reach)
        print(reach)

        courlis_geo.save(output_name)
        del(reach)
        del(courlis_geo)

def extrac_ptraver_parser(subparser):
    """
    Build subparser

    @param subparser (ArgumentParser) The parser

    @returns (ArgumentParser) The updated parser
    """
    parser = subparser.add_parser('extract_ptravers_res_to_geoc',
                                  help='Extract from ptravers Courlis\
                                   res file to Courlis geometry'
                                  'file at a given record')
    parser.add_argument("args", metavar='ptravers Courlis file')
    parser.add_argument("-o", "--output",
                        dest="outputFileName",
                        default='_prev.geoC',
                        help="Option to give a name to the output Courlis "
                             "geometry file")
    parser.add_argument("-r", "--time-record",
                        dest="records", type=int, default='-1',
                        help="Record numbers of the state you want\
                                to extract from "
                             "ptravers to a geometry courlis file")
    parser.add_argument("-a", "--all-records",
                        dest="all_records", action='store_true',
                        help="Option to extract all records from ptravers Courlis\
                              res file to Courlis geometry in a folder (default\
                              folder name: 'geoC_extracted')")
    parser.add_argument("-f", "--destination-folder",
                        dest="destination_folder",
                        default='geoC_extracted',
                        help="Option to define the name of the folder containing \
                              extracted geometries if the option --all-racords\
                              (or -a) has been activated")

    return subparser


def main():
    """
    Main function
    """

    parser = argparse.ArgumentParser(
        description='Extract from ptravers Courlis res file to\
                        Courlis geometry'
                    'file at a given record')

    parser.add_argument("args", metavar='ptravers Courlis file')
    parser.add_argument("-o", "--output",
                        dest="outputFileName",
                        default='_prev.geoC',
                        help="Option to give a name to the output Courlis "
                             "geometry file")
    parser.add_argument("-r", "--time-record",
                        dest="record", type=int, default='-1',
                        help="Record numbers of the state you want\
                                to extract from "
                             "ptravers to a geometry courlis file")
    parser.add_argument("-a", "--all-records",
                        dest="all_records", action='store_true',
                        help="Option to extract all records from ptravers Courlis\
                              res file to Courlis geometry in a folder (default\
                              folder name: 'geoC_extracted')")
    parser.add_argument("-f", "--destination-folder",
                        dest="destination_folder",
                        default='geoC_extracted',
                        help="Option to define the name of the folder containing \
                              extracted geometries if the option --all-racords\
                              (or -a) has been activated")

    options = parser.parse_args()

    extract_ptravers_res_to_geoc(options.args,
                                 options.outputFileName,
                                 options.record,
                                 options.all_records,
                                 options.destination_folder)


if __name__ == '__main__':
    main()

r"""@author Christophe Coulet, Matthieu Secher

    @history 27/06/2017 -- Christophe Coulet:
            Creation of parser of XCAS file
            Return the list of Mascaret Input File

    @history 20/07/2017 -- Christophe Coulet:
            Rename the parser which become parserMascaret
            Adding capabilities to read Opthyca and Rubens results

    @brief
"""
# _____             ___________________________________________________
# ____/ Imports /__________________________________________________/
#
# ~~> dependencies towards standard python
import xml.etree.ElementTree as ET
from os import path, sep
import re
import shutil
from collections import OrderedDict
# ~~> dependencies towards other pytel/modules
from config import update_config, CFGS
from execution.telemac_cas import TelemacCas, get_dico
from execution.telemac_dico import TelemacDico
from utils.exceptions import TelemacException
from utils.files import is_newer
from xml.dom import minidom

PATHNODE_EXCEPTION = \
        ['parametresCas/parametresGeometrieReseau/extrLibres/num']

def convert_text(text):
    """
    Convert text from xml (key values) in python type according to regex

    @param text (str) text to convert
    """

    int_ex = re.compile("[-]?[0-9]+")
    real_ex = re.compile("[-]?[0-9]*.[0-9]*")

    if text.lower() == 'true':
        return True
    if text.lower() == 'false':
        return False
    if int_ex.fullmatch(text):
        return int(text)
    if real_ex.fullmatch(text):
        return float(text)

    return text

class MascaretCas():
    """
    Class to hanlde a Mascaret steering file
    """
    def __init__(self,
            file_name,
            dico_file=None,
            access='r',
            check_files=True,
            convert_xcas=False):
        """
        Init of the class

        @param file_name (string) Name of the steering file
        @param dico_file (string) Name of the dictionary to use
        @param access (string) r (read) or w (write)
        @param check_files (bool) If true checking that input files exist
        @param convert_xcas (bool) If true allow to write cas file (Damocles
                                   format) from xcas file (xml)
        """
        self.file_name = file_name
        if not path.exists(file_name):
            raise TelemacException(
                "File does not exists:\n{}".format(file_name))

        if dico_file is None:
            try:
                self.dico_filename = get_dico("mascaret")
                self.dico = TelemacDico(self.dico_filename)
            except:
                self.dico_filename = None
                self.dico = None
        else:
            self.dico_filename = dico_file
            self.dico = TelemacDico(self.dico_filename)

        self.case = None
        self.cas_filename = None
        self.xcas_filename = None
        self.courlis_case = None
        self.in_files = OrderedDict()
        self.out_files = OrderedDict()
        self.check_files = check_files

        self.file_type = self.file_name.split('.')[-1].strip()

        if self.file_type == 'xcas':
            self.xcas_filename = self.file_name
            # Reading data from file
            self._parse_xcas()
            if convert_xcas and self.dico_filename is not None:
                # Convert xcas into TelemacCase
                self.cas_filename = \
                        self.file_name.split('.')[0].strip() + ".cas"

                self.case = TelemacCas(
                        self.cas_filename,
                        self.dico_filename,
                        access='w',
                        check_files=self.check_files)

                self.case.lang = 'en'
                self.convert_xcas_telcase()
            elif convert_xcas:
                raise TelemacException(
                    "Mascaret dictionnary file does not exists")

        elif self.file_type == 'cas':
            self.cas_filename = self.file_name
            self.case = TelemacCas(
                    self.cas_filename,
                    self.dico_filename,
                    access='r',
                    check_files=False)

        # Identify input and ouput files
        self._set_io_files_xcas_cas()

    def _parse_xcas(self):
        """
        parsing xml file
        """
        tree = ET.parse(self.file_name)
        self.root = tree.getroot()
        self.parent_map = dict(
                (c, p) for p in self.root.iter() for c in p)

    def _get_pathnode(self, node):
        """
        get pathnode of an xml tag
        """
        return self._get_pathnode(self.parent_map[node]) \
                + '/' + node.tag if node in self.parent_map else ''

    def convert_xcas_telcase(self, lang='en'):
        """
        Convert xcas file data (MascaretCas class data) into a case file
        data (completion of the MascaretCas class data) to allow for
        example to write a case file (Damocles format) of the xcas file

        @param lang (string) option to choose the language of the cas file
                             to write ('en' for english, 'fr' for french)
        """
        allocation = []
        dynamic_keywords_dict = {}
        i = 0
        for elem in self.root.iter():
            if list(elem) == []:
                pathnode_elem = self._get_pathnode(elem)[1:].replace(' ', '')
                if pathnode_elem in PATHNODE_EXCEPTION or \
                   pathnode_elem not in self.dico.dict_corresp_pathnode_key:
                    continue

                keyword = self.dico.dict_corresp_pathnode_key[pathnode_elem]
                if 'ALLOCATE' in self.dico.data[keyword]:
                    allocation.append(self.dico.data[keyword]['ALLOCATE'])
                if 'x' in keyword:
                    if pathnode_elem in dynamic_keywords_dict:
                        dynamic_keywords_dict[pathnode_elem] += 1
                    else:
                        dynamic_keywords_dict[pathnode_elem] = 1

                    gen_dico = self.dico.data[keyword]
                    keyword_fr = self.dico.gb2fr[keyword]

                    rubrique = []
                    for item in gen_dico['RUBRIQUE']:
                        rubrique.append(item.replace(
                            'x', str(dynamic_keywords_dict[pathnode_elem])))
                    rubrique1 = []
                    for item in gen_dico['RUBRIQUE']:
                        rubrique1.append(item.replace(
                            'x', str(dynamic_keywords_dict[pathnode_elem])))
                    keyword = keyword.replace('x', str(
                        dynamic_keywords_dict[pathnode_elem]))
                    keyword_fr = keyword_fr.replace('x', str(
                        dynamic_keywords_dict[pathnode_elem]))
                    self.dico.data[keyword] =\
                        {
                         'TYPE' : gen_dico['TYPE'],
                         'RUBRIQUE' : rubrique,
                         'RUBRIQUE1' : rubrique1,
                         }
                    self.dico.gb2fr[keyword] = keyword_fr
                    self.case.dico = self.dico

                if lang == 'fr':
                    keyword = dico.gb2fr[keyword]
                if 'APPARENCE' in self.dico.data[keyword] and \
                        'UNIQUE' in self.dico.data[keyword]['APPARENCE']:
                    if elem.text == None:
                        value = [' ']
                        i += 1
                    else:
                        value = [elem.text.strip()]
                else:
                    if elem.text == None:
                        continue
                    else:
                        value = [el for el in elem.text.strip().split(' ')]

                for alloc in allocation:
                    if alloc in pathnode_elem:
                        if keyword in self.case.values.keys():
                            values = self.case.values[keyword]
                            [values.append(val) for val in value]
                            value = values
                self.case.values[keyword] = value

        self.case._convert_values()

    def write_case_file(self, cas_filename=None, lang='en'):
        """
        Write cas file (Damocles format)

        @param cas_filename (string) name of the cas file to write
        @param lang (string) option to choose the language of the cas file
                             to write ('en' for english, 'fr' for french)
        """
        if cas_filename == None:
            cas_filename = self.cas_filename

        if self.case is not None:
            self.case.lang = lang
            self.case.write(cas_filename)
        else:
            raise TelemacException(
                "convert_xcas option is not activated")

    def prettify(self, elem):
        """
        Return a pretty-printed XML string for the Element.
        """
        rough_string = ET.tostring(elem, 'utf-8')
        reparsed = minidom.parseString(rough_string)
        return reparsed.toprettyxml(indent="  ")

    def write_xcas_file(self, xcas_filename=None):
        """
        Write xcas file (xml)

        @param xcas_filename (string) name of the xcas file to write
        """
        if xcas_filename == None:
            xcas_filename = self.xcas_filename

        if self.case != None:
            #first section of xml file ==> "fichierCas"
            root = ET.Element('fichierCas')
            #Second section of xml file ==>> "parametreCas"
            case_param = ET.SubElement(root, 'parametresCas')
            #building tree
            tree = ET.ElementTree(root)

            allocated_dict = OrderedDict()
            for key in self.case.values.keys():
                value = self.case.values[key]
                if 'ALLOCATED' in self.dico.data[key] and \
                        'ALLOCATED2' in self.dico.data[key]:
                    key_alloc = self.dico.data[key]['ALLOCATED']
                    key_alloc2 = self.dico.data[key]['ALLOCATED2']
                    values = []
                    if isinstance(self.case.values[key_alloc2], list):
                        for dim in self.case.values[key_alloc2]:
                            list_temp = []
                            for i in range(dim):
                                list_temp.append(value[0])
                                value.pop(0)
                            values.append(list_temp)
                    else:
                        values = [value]
                    end_path = self.dico.data[key]['PATHNODE'].replace(
                            ' ', '').replace(
                                    self.dico.data[key_alloc]['ALLOCATE'], '')
                    if end_path == '':
                        end_path = \
                            self.dico.data[key_alloc]['ALLOCATE'].split('/')[-1]
                    if end_path[0] == '/':
                        end_path = end_path[1:]

                    for i, val in enumerate(values[1:]):
                        num_key_alloc = key_alloc + ' ' + str(i)
                        if num_key_alloc not in allocated_dict:
                            allocated_dict[num_key_alloc] = OrderedDict()
                            allocated_dict[num_key_alloc]['item'] = \
                                    self.dico.data[key_alloc]['ALLOCATE']

                        allocated_dict[num_key_alloc][end_path] = \
                                " ".join(str(v) for v in val)

                    value = " ".join(str(val) for val in values[0])

                elif 'ALLOCATED' in self.dico.data[key]:
                    key_alloc = self.dico.data[key]['ALLOCATED']
                    if isinstance(value, list):
                        value = [str(val) for val in value]
                    else:
                        value = str(value)
                    end_path = self.dico.data[key]['PATHNODE'].replace(
                            ' ', '').replace(
                                    self.dico.data[key_alloc]['ALLOCATE'], '')
                    if end_path == '':
                        end_path = \
                            self.dico.data[key_alloc]['ALLOCATE'].split('/')[-1]
                    if end_path[0] == '/':
                        end_path = end_path[1:]

                    if isinstance(value, str):
                        value = [value]
                    else:
                        for i, val in enumerate(value[1:]):
                            num_key_alloc = key_alloc + ' ' + str(i)
                            if num_key_alloc not in allocated_dict:
                                allocated_dict[num_key_alloc] = OrderedDict()
                                allocated_dict[num_key_alloc]['item'] = \
                                        self.dico.data[key_alloc]['ALLOCATE']

                            allocated_dict[num_key_alloc][end_path] = \
                                    value[i + 1]

                    value = value[0]

                if isinstance(value, list):
                    val_temp = ''
                    for val in value:
                        val_temp += str(val) + ' '
                        value = val_temp.strip()
                else:
                    value = str(value)

                pathnode = self.dico.data[key]['PATHNODE'].replace(" ", "")
                list_pathnode = pathnode.split('/')
                path = ""
                for sect in list_pathnode[:-1]:
                    sect = sect.strip()
                    path += "".join(sect + "/")
                    if tree.findall(".//" + path[:-1]) == []:
                        ET.SubElement(
                                tree.findall(".//" + previous_path)[0], sect)

                    previous_path = path[:-1]

                ET.SubElement(tree.findall(
                    ".//" + previous_path)[0], list_pathnode[-1]).text = value

            if allocated_dict != {}:
                already_done = []
                for key, dic in allocated_dict.items():
                    item = dic['item'].replace(' ', '')
                    dic.pop('item')
                    temp_tree = root
                    for level in item.split('/')[:-1]:
                        temp_tree = temp_tree.find(level)
                    store = temp_tree
                    new_item = ET.Element(item.split('/')[-1])
                    test_i = 0
                    other_item = None
                    for i, sect in enumerate(dic):
                        split_list = sect.strip().split('/')
                        if '' in split_list:
                            split_list.remove('')

                        if len(split_list) == 1:
                            if sect != item.split('/')[-1]:
                                new_temp = ET.Element(sect)
                                new_temp.text = dic[sect]
                                new_item.append(new_temp)
                            else:
                                new_item.text = dic[sect]
                            test_i = i + 1
                        else:
                            if i == test_i:
                                other_item = ET.Element(split_list[0])
                            else:
                                test = tree.findall(".//" + split_list[0])[-1]

                            new_temp = ET.Element(split_list[-1])
                            new_temp.text = dic[sect]
                            other_item.append(new_temp)

                    if other_item != None:
                        new_item.append(other_item)
                    store.append(new_item)
            file_string = ET.tostring(root)
            dom = minidom.parseString(file_string)
            pretty_xml = dom.toprettyxml(indent="  ")
            with open(xcas_filename, 'w') as xmldata:
                xmldata.write(pretty_xml)

    def _set_io_files_xcas_cas(self):
        """
        defining input and output files
        """
        if self.case is None:
            in_files = [
                ('xcas', 'parametresGeneraux/fichMotsCles'),
                ('geo', 'parametresGeometrieReseau/geometrie/fichier'),
                ('loi', 'parametresLoisHydrauliques/lois/structureParametresLoi/donnees/fichier'),
                ('lig', 'parametresConditionsInitiales/ligneEau/fichLigEau'),
                ('casier', 'parametresCasier/fichierGeomCasiers'),
                ('tracer_conc', 'parametresTraceur/parametresConcentrationsInitialesTraceur/fichConcInit'),
                ('tracer_parphy', 'parametresTraceur/parametresNumeriquesQualiteEau/fichParamPhysiqueTracer'),
                ('tracer_meteo', 'parametresTraceur/parametresNumeriquesQualiteEau/fichMeteoTracer'),
                ('tracer_loi', 'parametresTraceur/parametresLoisTraceur/loisTracer/structureSParametresLoiTraceur/fichier'),
                ('', 'parametresGeneraux/fichierMotCleCourlis'),
                ]

            for ftype, key in in_files:
                val = self.get(key)
                if val is not None:
                    if isinstance(val, list):
                        for ffile in val:
                            self.in_files[ffile] = ftype
                    else:
                        self.in_files[val] = ftype

            out_files = [
                ('listing', 'parametresImpressionResultats/listing/fichListing'),
                ('listing_casier', 'parametresImpressionResultats/casier/listingCasier'),
                ('listing_liaison', 'parametresImpressionResultats/casier/listingLiaison'),
                ('res', 'parametresImpressionResultats/resultats/fichResultat'),
                ('res_casier', 'parametresImpressionResultats/casier/resultatCasier'),
                ('res_liaison', 'parametresImpressionResultats/casier/resultatLiaison'),
               # ('', 'parametresImpressionResultats/fichReprise/fichRepriseEcr'),
                ('tracer_listing', 'parametresTraceur/parametresImpressionTraceur/fichListTracer'),
                ('tracer_res', 'parametresTraceur/parametresImpressionTraceur/fichResultTracer'),
                ]
            for ftype, key in out_files:
                val = self.get(key)
                if val is not None:
                    if isinstance(val, list):
                        for ffile in val:
                            self.out_files[ffile] = ftype
                    else:
                        self.out_files[val] = ftype

            # Handle courlis
            #TODO: adapt this part when Courlis parameters will be in xcas file
            val = self.get('parametresGeneraux/optionCourlis')
            if val is not None:
                courlis_cas_file = path.join(path.dirname(self.file_name),
                        self.get('parametresGeneraux/fichierMotCleCourlis'))

                cas = TelemacCas(courlis_cas_file,
                                 self.dico_filename,
                                 access='r',
                                 check_files=False)

                geo_courlis = cas.get('FICHIER DE GEOMETRIE COURLIS')
                self.in_files[geo_courlis] = 'geoC'

                #TODO: Add outputfiles as well
                listing_courlis = cas.get('FICHIER LISTING COURLIS')
		#TODO: inversed key comparing above ==> change
                self.out_files['listing_courlis'] = listing_courlis

            if self.check_files:
                for ffile in self.in_files:
                    if not path.exists(ffile):
                        raise TelemacException(
                            "The file {} does not exists".format(ffile))

        else:
            self.case._set_io_files()

            if self.case.get('COURLIS OPTION'):
                 courlis_case_filename = path.join(path.dirname(self.file_name),
                         self.case.get('KEYWORD FILE FOR COURLIS'))
                 #check files is set to False because there is still three
                 #Mascaret keywords in Courlis case file
                 self.courlis_case = TelemacCas(courlis_case_filename,
                                                self.dico_filename,
                                                access='r',
                                                check_files=False)

                 keys_to_del = ['FICHIER DES MOT-CLES',
                                'PROGRAMME PRINCIPAL',
                                'FICHIER DE GEOMETRIE']
                 for key in keys_to_del:
                     self.courlis_case.values.pop(key, None)

                 self.courlis_case._set_io_files()

            self.in_files = self.case.in_files
            self.out_files = self.case.out_files

    def get(self, key, convert=True):
        """
        Get value of path in xml

        @param key (str) path in the xml (use separator /)
        @param convert (bool) If True value will be converted according to a
        regex (see convert_text for more info)

        @return (list/int/bool/string/float) If the value as more than one item
        a list is returned
        """
        vals = self.root[0]
        cumul_key = ''
        for item in key.split('/'):
            cumul_key += '/' + item
            if isinstance(vals, list):
                new_vals = []
                for val in vals:
                    new_val = val.findall(item)
                    if new_val == []:
                        return None
                    new_vals.extend(new_val)
                vals = new_vals
            else:
                vals = vals.findall(item)
                if vals == []:
                    return None

        if convert:
            if len(vals) == 1:
                return convert_text(vals[0].text)

            return [convert_text(val.text) for val in vals]

        if len(vals) == 1:
            return vals[0].text

        return [val.text for val in vals]

    def copy_cas_files(self, dir_path, verbose=False, copy_cas_file=True):
        """
        Will copy all input files and the steering file into a directory

        @param dir_path (str) Directory in which to copy the files
        @param verbose (bool) If true makes a print for each copy
        @param copy_cas_file (boole) If True also copies the steering file
        """
        if not path.exists(dir_path):
            raise TelemacException(
                self, "Copy dir does not exists:\n"+dir_path)

        if self.case == None:
            # Copying input_files
            for ffile, ftype in self.in_files.items():
                if ftype == 'xcas':
                    continue
                src = path.join(path.dirname(self.file_name), ffile)
                dest = path.join(dir_path, ffile)
                if is_newer(dest, src):
                    shutil.copy2(src, dest)
                    if verbose:
                        print("Copying {} -> {}".format(ffile, dir_path))

            if copy_cas_file:
                # Copying steering file
                src = self.file_name
                dest = path.join(dir_path, path.basename(self.file_name))
                if is_newer(dest, src):
                    shutil.copy2(src, dest)
                    if verbose:
                        print("Copying {} -> {}".format(src, dir_path))
                if self.cas_filename is not None:
                    src = self.cas_filename
                    dest = path.join(dir_path, path.basename(self.cas_filename))
                    if is_newer(dest, src):
                        shutil.copy2(src, dest)
                        if verbose:
                            print("Copying {} -> {}".format(src, dir_path))

        else:

            self.case.copy_cas_files(
                    dir_path,
                    verbose=verbose,
                    copy_cas_file=copy_cas_file)

            if self.courlis_case is not None:
                self.courlis_case.copy_cas_files(
                        dir_path,
                        verbose=verbose,
                        copy_cas_file=copy_cas_file)


"localizationname=Nom de la localisation, de longueur maximum #MED_NAME_SIZE ." \
"weight=Poids des points d'intégration." \
"spacedimension=Dimension de l'espace des coordonnées des points d'intégration." \
"elementcoordinate=Coordonnées des noeuds de l'éléments de référence." \
"nipoint=Nombre de points d'intégration dans l'élément de référence." \
"ipointcoordinate=Coordonnées des points d'intégration." \
"localizationit=Itérateur sur les localisations. La valeur initiale d'un itérateur est 1." \
"geointerpname=Nom de la fonction de transformation géométrique (#MED_NO_INTERPOLATION si pas de transformation)." \
"sectionmeshname=Nom du maillage support sectionnant l'élément de structure à chaque point d'intégration (#MED_NO_MESH_SUPPORT si pas de section)." \
"nsectionmeshcell=Nombre de maille dans le maillage support section (0 si pas de section)." \
"sectiongeotype=Type géométrique des mailles du maillage support section (#MED_UNDEF_GEOTYPE si pas de section). " \
"nlocalization=Nombre de localisations de points d'intégration dans le fichier." \
\
\
\
\
"MEDlocalizationWrBrief=Cette routine permet l'écriture d'une localisation \a localizationname de\
 points d'intégration dans/autour d'un élément de référence de dimension \a spacedimension." \
\
"MEDlocalizationDetails=\
 L'élément de référence de type \a geotype est décrit par les coordonnées \a elementcoordinate de ses noeuds\
 dans un repère absolu de dimension \a spacedimension.\n\
 Les \a nipoints points d'intégrations sont positionnés dans l'élément de référence par leurs coordonnées\
 \a ipointcoordinate. Les poids associés à chacun des points d'intégration est spécifié par le tableau \a weight.\n" \
\
"MEDlocalizationWrDetails= \MEDlocalizationWrBrief \n \MEDlocalizationDetails" \
\
"MEDlocalizationRdBrief=Cette routine permet la lecture d'une localisation \a localizationname de\
 points d'intégration dans/autour d'un élément de référence de dimension \a spacedimension." \
\
"MEDlocalizationRdDetails=\MEDlocalizationRdBrief \n\
 L'élément de référence est décrit par les coordonnées \a elementcoordinate de ses noeuds\
 dans un repère absolu de dimension \a spacedimension.\n\
 Les points d'intégrations sont positionnés dans l'élément de référence par leurs coordonnées\
 \a ipointcoordinate. Les poids associés à chacun des points d'intégration est spécifié par le tableau \a weight.\n" \
\
"MEDlocalizationInfoByNameBrief=Cette routine permet d'obtenir la description d'une localisation de points d'intégration nommée \a localizationname." \
"MEDlocalizationInfoByNameDetails=\MEDlocalizationInfoByNameBrief \n\
 Les données lues sont :\n\
 \li le type géométrique de l'élément \li la dimension de l'espace du repère des coordonnées \li le nombre de points d'intégration\
 \li le nom de la transformation géométrique \li le nom de maillage support à la section d'élément de structure\
 \li le nombre de maille de ce maillage support et le type géométrique de ces mailles." \
\
"MEDlocalizationInfoBrief=Cette routine permet d'obtenir la description de la localisation de points d'intégration n° \a localizationit." \
"MEDlocalizationInfoDetails=\MEDlocalizationInfoBrief \n\
 Les données lues sont :\n\
 \li le nom de la localisation \li le type géométrique de l'élément \li la dimension de l'espace du repère des coordonnées \li le nombre de points d'intégration\
 \li le nom de la transformation géométrique \li le nom de maillage support à la section d'élément de structure\
 \li le nombre de maille de ce maillage support et le type géométrique de ces mailles." \
\
"MEDnLocalizationBrief=Cette routine permet de lire le nombre de localisations de points d'intégration contenues dans un fichier." \
"MEDnLocalizationDetails=\MEDnLocalizationBrief." \
\
\
\
\
"MEDlocalizationDef=\
Dans le cadre des échanges de champs de résultats exprimés sur des points d'intégration,\
 MED permet la localisation de ces points dans des éléments de référence en des lieux définis par la modélisation numérique choisie.\
 Pour chaque type de modélisation, il est possible de spécifier nominativement cette localisation sur des éléments de référence.\
 Chaque point d'intégration est localisé au sein d'un élément de référence par ses coordonnées et se voit associer un poids.\
" \
\
"MEDlocalizationRem=\
\li Si les points d'intégration se confondent avec les noeuds de l'élément, il est inutile de créer une localisation factice avec des poids qui ne signifient rien\
 et des coordonnées des points d'intégration identiques à celles des noeuds.\
 Dans ce cas de figure, il faut utiliser mot clé réservé #MED_GAUSS_ELNO comme nom de localisation à l'écriture des valeurs d'un champ.\
 \li Il est possible d'associer à une localisation de points d'intégration une fonction de transformation/ d'interpolation géométrique qui projette\
 les points de la maille de référence vers ceux de la maille réelle.\
 \li Si le type géométrique \a geotype utilisé est celui d'un #MED_STRUCT_ELEMENT, il est possible d'indiquer l'utilisation\
 d'un maillage support définissant une section du modèle d'élément de structure. Ce maillage support est alors utilisé comme\
 section de l'élément de structure à chaque point d'intégration.\
 Auquel cas un champ utilisant cette localisation définira autant de valeur par élément qu'il y a de mailles dans le maillage section\
 de chaque point d'intégration.\
" \
\
\
\
\
"MEDlocalizationRdWrRem=\
\li Si le type géométrique \a geotype utilisé est celui d'un #MED_STRUCT_ELEMENT, les coordonnées \a elementcoordinate des noeuds de l'élément de référence\
 sont celles des noeuds du maillage support du modèle de cet élément.\
 En effet l'élément de référence est déjà décrit à la définition de l'élément de structure.\
" \
"MEDlocalizationRdRem=\MEDlocalizationRdWrRem \
 Le tableau \a elementcoordinate renvoie les coordonnées des noeuds du mailage support utilisé par le modèle d'élément de structure.\
 Les coordonnées des points d'intégration y sont relatives." \
"MEDlocalizationWrRem=\MEDlocalizationRdWrRem \
 Il n'est pas necessaire de renseigner le tableau \a elementcoordinate.\
 Les coordonnées des points d'intégration sont relatives aux coordonnées des noeuds du mailage support utilisé par le modèle d'élément de structure.\
" \
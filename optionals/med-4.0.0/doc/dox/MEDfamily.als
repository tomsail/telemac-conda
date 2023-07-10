"familyname=familyname Nom de la famille de longueur maximum #MED_NAME_SIZE ." \
"familynumber=familynumber Numéro de la famille." \
"ngroup=ngroup Nombre de groupe." \
"groupname=groupname Nom(s) de(s) groupe(s). Chaque nom de groupe est une chaîne de de longueur maximum #MED_LNAME_SIZE caractères." \
"famit=famit Itérateur sur les familles du maillage." \
"attributenumber=attributenumber Liste des identificateurs des attributs." \
"attributevalue=attributevalue Liste des valeurs des attributs." \
"attributedes=attributedes Liste des descripteurs des attributs." \
"nfamily=Nombre de famille dans un maillage." \
"natt=Nombre d'attribut de la famille." \
"ngroup=Nombre de groupe de la famille." \
"MEDfamilyCrBrief=Cette routine permet la création d'une famille portant sur les entités d'un maillage." \
"MEDfamilyCrDetails=Cette routine permet la création d'une famille portant sur les entités d'un maillage. Une famille est composée d'une liste de noms de groupes (éventuellement vide). Chaque nom de groupes comprend #MED_LNAME_SIZE caractères. Les familles sont identifiées par un nom et un numéro avec les caractéristiques suivantes : \
\li Le nom d'une famille est une chaîne d'au plus #MED_NAME_SIZE caractères. Une famille de noeuds peut porter le même nom qu'un famille d'éléments. Par contre les familles d'éléments (respectivement de noeuds) doivent toutes avoir des noms différents. \
\li Le numéro d'une famille de noeuds doit être positif ou nul, le numéro d'une famille d'éléments doit être négatif ou nul. Par convention, la famille de numéro 0 ne comporte aucun groupe. La création de numéro 0 est obligatoire car elle constitue la famille de référence pour tous les noeuds et les éléments qui n'appartiennent à aucun groupe." \
"MEDfamilyInfoBrief=Cette routine permet de lire les informations relatives à une famille d'un maillage." \
"MEDfamilyInfoDetails=Cette routine permet de lire les informations relatives à une famille d'un maillage.  L'accès à la famille se fait via un itérateur. Les informations lues sont : \
\li Le nom de la famille qui est une chaîne d'au plus #MED_NAME_SIZE caractères. Une famille de noeuds peut porter le même nom qu'un famille d'éléments. Par contre les familles d'éléments (respectivement de noeuds) doivent toutes avoir des noms différents. \
\li Le numéro de la famille qui oit être positif ou nul (le numéro d'une famille d'éléments doit être négatif ou nul, par convention la famille de numéro 0 ne comporte aucun groupe).\
\li La liste de groupe de la famille (éventuellement vide). Chaque nom de groupes comprend #MED_LNAME_SIZE caractères. " \
"MEDfamily23InfoBrief=Cette routine permet de lire les informations relatives à une famille d'un maillage créé avec MED 2.3 ou MED 2.2, -i.e. pouvant comporter une liste d'attributs en plus de la liste de groupes." \
"MEDfamily23InfoDetails=Cette routine permet de lire les informations relatives à une famille d'un maillage créé avec MED 2.3 ou MED 2.2, -i.e. pouvant comporter une liste d'attributs en plus de la liste de groupes (la notion d'attribut a disparu avec MED 3.0). L'accès à la famille se fait via un itérateur et les informations lues sont : \
\li Le nom de la famille qui est une chaîne d'au plus #MED_NAME_SIZE caractères. Une famille de noeuds peut porter le même nom qu'un famille d'éléments. Par contre les familles d'éléments (respectivement de noeuds) doivent toutes avoir des noms différents. \
\li Les attributs de la famille fournis sous la forme de 3 listes distinctes : liste des descripteurs entiers (un descripteur entier correspond à un numéro d'ordre dans la liste), liste des valeurs des attributs (un attribut porte une valeur entière), liste des descripteurs (un descripteur est une chaîne de #MED_COMMENT_SIZE caractères). \
\li Le numéro de la famille qui oit être positif ou nul (le numéro d'une famille d'éléments doit être négatif ou nul, par convention la famille de numéro 0 ne comporte aucun groupe). \
\li La liste de groupe de la famille (éventuellement vide). Chaque nom de groupes comprend #MED_LNAME_SIZE caractères." \
"MEDnFamilyBrief=Cette routine permet de lire le nombre de famille dans un maillage." \
"MEDnFamilyDetails=Cette routine permet de lire le nombre de famille dans un maillage." \
"MEDnFamily23AttributeBrief=Cette routine permet de lire le nombre d'attribut dans une famille dans un maillage créé avec MED 2.2 ou 2.3." \
"MEDnFamily23AttributeDetails=Cette routine permet de lire le nombre d'attribut dans une famille dans un maillage créé avec MED 2.2 ou 2.3, -i.e. pouvant comporter une liste d'attributs en plus de la liste de groupes (la notion d'attribut a disparu avec MED 3.0). L'accès à la famille se fait via un itérateur." \
"MEDnFamilyGroupBrief=Cette routine permet de lire le nombre de groupe dans une famille." \
"MEDnFamilyGroupDetails=Cette routine permet de lire le nombre d'attribut dans une famille. L'accès à la famille se fait via un itérateur." \
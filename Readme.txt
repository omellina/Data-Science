CARTE SCOLAIRE DE PARIS

A. Les fichiers sources en ligne :
- Fichier des rues de Paris avec leur code Paris : http://parisdata.opendatasoft.com/explore/dataset/liste_des_bureaux_de_vote_2013_voies_de_paris/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true
- Fichier des adresses des rues de Paris : http://parisdata.opendatasoft.com/explore/dataset/adresse_paris/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true
- l'API de serveur de la DGSCO perimsco.paris.fr/

B. Les fichiers de traitement de données sont :
- 1.codage_adresses_rues_Paris_vf.R : qui code la base des adresses avec les codes rue de Paris ;
- 2.Carte_scolaire_scrap_Paris.R : qui extrait du serveur 3 la carte scolaires pour toute la base adresse de Paris
- 3.geocodage_etablissements.R : qui code l'adresse des établissements

C. Les fichiers de résultat sont :
1.- adresses_paris_codees.csv : qui rajoute un champ à la base adresse de Paris pour rajouter le code rue de Paris
	contient notamment les champs :
	- N_SQ_AD : identifiant unique d'adresse,
	- VOIE_ID : identifiant unique des rues de Paris ;
2.- correspondance_N_SQ_VO-VOIE_ID.csv : la version de liaison simple permettant de reconstituer adresses_paris_codees.csv à partir de la source (correspondance entre les codes rues de la base adresse et ceux de Paris) ;
3.- carte_scolaire_maternelles.csv : la carte scolaire de maternelle reliant les champs :
	- N_SQ_AD : index adresse de la base 1.
	- maternelle : numéro de l'établissement dans la liste 6.
4. - carte_scolaire_primaires.csv : la carte scolaire de primaire reliant les champs :
	- N_SQ_AD : index adresse de la base 1.
	- primaire : numéro de l'établissement dans la liste 6.
5. - carte_scolaire_college.csv : la carte scolaire de college reliant les champs :
	- N_SQ_AD : index adresse de la base 1.
	- maternelle : numéro de l'établissement dans la liste 6.
6.- etablissements_Paris.csv : liste des établissements, en majuscule, avec les champs :
	- libelle.complet : la chaîne de caractère complète extrait du site ;
	- index : l'index utilisé dans les tables 3., 4. et 5 ;
	- libelle : le nom de l'école (école maternelle, école primaire ou collège avec le nom) ;
	- numero : numéro dans la rue ;
	- rue : rue ;
	- N_SQ_AD : le code adresse de l'établissement suivant le codage du fichier 1.
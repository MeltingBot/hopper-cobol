       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-ACHATS.
      * ================================================
      * SYSTEME COMPLET DE GESTION DES ACHATS
      * - Gestion des fournisseurs (CRUD)
      * - Catalogue produits par fournisseur
      * - Saisie de commandes avec lignes
      * - Suivi et reception des commandes
      * ================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FOURNISSEURS ASSIGN TO 'FOURNIS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FRN-CODE.
           SELECT PRODUITS ASSIGN TO 'PRODUITS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRD-CODE.
           SELECT COMMANDES ASSIGN TO 'COMMANDES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CMD-NUM.
       DATA DIVISION.
       FILE SECTION.
       FD FOURNISSEURS.
       01 FOURNIS-REC.
           05 FRN-CODE      PIC X(4).
           05 FRN-NOM       PIC X(20).
           05 FRN-TEL       PIC X(10).
           05 FRN-DELAI     PIC 99.
       FD PRODUITS.
       01 PRODUIT-REC.
           05 PRD-CODE      PIC X(6).
           05 PRD-FRN       PIC X(4).
           05 PRD-LIBELLE   PIC X(20).
           05 PRD-PRIX      PIC 9(5)V99.
           05 PRD-STOCK     PIC 9(4).
       FD COMMANDES.
       01 COMMANDE-REC.
           05 CMD-NUM       PIC 9(6).
           05 CMD-FRN       PIC X(4).
           05 CMD-DATE      PIC 9(8).
           05 CMD-PRD       PIC X(6).
           05 CMD-QTE       PIC 9(4).
           05 CMD-PRIX      PIC 9(5)V99.
           05 CMD-STATUT    PIC X.
      *       E=EN COURS, V=VALIDEE, R=RECUE, X=ANNULEE
       WORKING-STORAGE SECTION.
       01 WS-MENU          PIC 9 VALUE 0.
       01 WS-CHOIX         PIC 9 VALUE 0.
       01 WS-FIN           PIC 9 VALUE 0.
       01 WS-EOF           PIC 9 VALUE 0.
       01 WS-TROUVE        PIC 9 VALUE 0.
       01 WS-CONFIRM       PIC X VALUE SPACE.
       01 WS-NEXT-CMD      PIC 9(6) VALUE 1.
       01 WS-TOTAL         PIC 9(7)V99 VALUE 0.
       01 WS-MONTANT       PIC 9(7)V99 VALUE 0.
       01 WS-COUNT         PIC 999 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O FOURNISSEURS.
           OPEN I-O PRODUITS.
           OPEN I-O COMMANDES.
           PERFORM UNTIL WS-FIN = 1
               PERFORM AFFICHER-MENU
               ACCEPT WS-MENU
               EVALUATE WS-MENU
                   WHEN 1 PERFORM MENU-FOURNISSEURS
                   WHEN 2 PERFORM MENU-PRODUITS
                   WHEN 3 PERFORM MENU-COMMANDES
                   WHEN 4 PERFORM MENU-RECEPTION
                   WHEN 9 MOVE 1 TO WS-FIN
               END-EVALUATE
           END-PERFORM.
           CLOSE FOURNISSEURS.
           CLOSE PRODUITS.
           CLOSE COMMANDES.
           DISPLAY ' '.
           DISPLAY '*** AU REVOIR ***'.
           STOP RUN.
       AFFICHER-MENU.
           DISPLAY ' '.
           DISPLAY '================================'.
           DISPLAY '  SYSTEME GESTION DES ACHATS'.
           DISPLAY '================================'.
           DISPLAY '1. GESTION FOURNISSEURS'.
           DISPLAY '2. GESTION PRODUITS'.
           DISPLAY '3. SAISIE COMMANDES'.
           DISPLAY '4. RECEPTION COMMANDES'.
           DISPLAY '9. QUITTER'.
           DISPLAY '--------------------------------'.
           DISPLAY 'VOTRE CHOIX:'.
      * ================================================
      * GESTION DES FOURNISSEURS
      * ================================================
       MENU-FOURNISSEURS.
           DISPLAY ' '.
           DISPLAY '--- FOURNISSEURS ---'.
           DISPLAY '1. CREER FOURNISSEUR'.
           DISPLAY '2. CONSULTER'.
           DISPLAY '3. MODIFIER'.
           DISPLAY '4. LISTER TOUS'.
           DISPLAY '0. RETOUR'.
           DISPLAY 'CHOIX:'.
           ACCEPT WS-CHOIX.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM CREER-FOURNISSEUR
               WHEN 2 PERFORM CONSULTER-FOURNISSEUR
               WHEN 3 PERFORM MODIFIER-FOURNISSEUR
               WHEN 4 PERFORM LISTER-FOURNISSEURS
           END-EVALUATE.
       CREER-FOURNISSEUR.
           DISPLAY 'CODE (4 CAR):'.
           ACCEPT FRN-CODE.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'NOM:'
                   ACCEPT FRN-NOM
                   DISPLAY 'TELEPHONE:'
                   ACCEPT FRN-TEL
                   DISPLAY 'DELAI LIVRAISON (JOURS):'
                   ACCEPT FRN-DELAI
                   WRITE FOURNIS-REC
                   DISPLAY '*** FOURNISSEUR CREE ***'
               NOT INVALID KEY
                   DISPLAY 'ERREUR: CODE DEJA UTILISE'
           END-READ.
       CONSULTER-FOURNISSEUR.
           DISPLAY 'CODE FOURNISSEUR:'.
           ACCEPT FRN-CODE.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR NON TROUVE'
               NOT INVALID KEY
                   DISPLAY '------------------------'
                   DISPLAY 'CODE:  ' FRN-CODE
                   DISPLAY 'NOM:   ' FRN-NOM
                   DISPLAY 'TEL:   ' FRN-TEL
                   DISPLAY 'DELAI: ' FRN-DELAI ' JOURS'
           END-READ.
       MODIFIER-FOURNISSEUR.
           DISPLAY 'CODE FOURNISSEUR:'.
           ACCEPT FRN-CODE.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR NON TROUVE'
               NOT INVALID KEY
                   DISPLAY 'NOM ACTUEL: ' FRN-NOM
                   DISPLAY 'NOUVEAU NOM (OU ENTREE):'
                   ACCEPT FRN-NOM
                   DISPLAY 'NOUVEAU TEL:'
                   ACCEPT FRN-TEL
                   DISPLAY 'NOUVEAU DELAI:'
                   ACCEPT FRN-DELAI
                   REWRITE FOURNIS-REC
                   DISPLAY '*** FOURNISSEUR MODIFIE ***'
           END-READ.
       LISTER-FOURNISSEURS.
           DISPLAY ' '.
           DISPLAY 'CODE | NOM                  | DELAI'.
           DISPLAY '-----+----------------------+------'.
           MOVE LOW-VALUES TO FRN-CODE.
           START FOURNISSEURS KEY >= FRN-CODE
               INVALID KEY
                   DISPLAY 'AUCUN FOURNISSEUR'
               NOT INVALID KEY
                   MOVE 0 TO WS-EOF
                   PERFORM UNTIL WS-EOF = 1
                       READ FOURNISSEURS NEXT
                           AT END MOVE 1 TO WS-EOF
                           NOT AT END
                               DISPLAY FRN-CODE ' | '
                                   FRN-NOM ' | ' FRN-DELAI 'J'
                       END-READ
                   END-PERFORM
           END-START.
      * ================================================
      * GESTION DES PRODUITS
      * ================================================
       MENU-PRODUITS.
           DISPLAY ' '.
           DISPLAY '--- PRODUITS ---'.
           DISPLAY '1. AJOUTER PRODUIT'.
           DISPLAY '2. CONSULTER'.
           DISPLAY '3. MODIFIER PRIX/STOCK'.
           DISPLAY '4. CATALOGUE FOURNISSEUR'.
           DISPLAY '0. RETOUR'.
           DISPLAY 'CHOIX:'.
           ACCEPT WS-CHOIX.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM AJOUTER-PRODUIT
               WHEN 2 PERFORM CONSULTER-PRODUIT
               WHEN 3 PERFORM MODIFIER-PRODUIT
               WHEN 4 PERFORM CATALOGUE-FOURNISSEUR
           END-EVALUATE.
       AJOUTER-PRODUIT.
           DISPLAY 'CODE FOURNISSEUR:'.
           ACCEPT PRD-FRN.
           MOVE PRD-FRN TO FRN-CODE.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR INCONNU'
               NOT INVALID KEY
                   DISPLAY 'FOURNISSEUR: ' FRN-NOM
                   DISPLAY 'CODE PRODUIT (6 CAR):'.
                   ACCEPT PRD-CODE
                   READ PRODUITS
                       INVALID KEY
                           DISPLAY 'LIBELLE:'
                           ACCEPT PRD-LIBELLE
                           DISPLAY 'PRIX UNITAIRE:'
                           ACCEPT PRD-PRIX
                           DISPLAY 'STOCK INITIAL:'
                           ACCEPT PRD-STOCK
                           WRITE PRODUIT-REC
                           DISPLAY '*** PRODUIT AJOUTE ***'
                       NOT INVALID KEY
                           DISPLAY 'CODE PRODUIT DEJA EXISTANT'
                   END-READ
           END-READ.
       CONSULTER-PRODUIT.
           DISPLAY 'CODE PRODUIT:'.
           ACCEPT PRD-CODE.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT NON TROUVE'
               NOT INVALID KEY
                   DISPLAY '------------------------'
                   DISPLAY 'CODE:    ' PRD-CODE
                   DISPLAY 'LIBELLE: ' PRD-LIBELLE
                   DISPLAY 'FOURNIS: ' PRD-FRN
                   DISPLAY 'PRIX:    ' PRD-PRIX
                   DISPLAY 'STOCK:   ' PRD-STOCK
           END-READ.
       MODIFIER-PRODUIT.
           DISPLAY 'CODE PRODUIT:'.
           ACCEPT PRD-CODE.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT NON TROUVE'
               NOT INVALID KEY
                   DISPLAY 'PRODUIT: ' PRD-LIBELLE
                   DISPLAY 'PRIX ACTUEL: ' PRD-PRIX
                   DISPLAY 'NOUVEAU PRIX:'
                   ACCEPT PRD-PRIX
                   DISPLAY 'STOCK ACTUEL: ' PRD-STOCK
                   DISPLAY 'NOUVEAU STOCK:'
                   ACCEPT PRD-STOCK
                   REWRITE PRODUIT-REC
                   DISPLAY '*** PRODUIT MODIFIE ***'
           END-READ.
       CATALOGUE-FOURNISSEUR.
           DISPLAY 'CODE FOURNISSEUR:'.
           ACCEPT FRN-CODE.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR INCONNU'
               NOT INVALID KEY
                   DISPLAY ' '
                   DISPLAY 'CATALOGUE: ' FRN-NOM
                   DISPLAY 'CODE   | LIBELLE              | PRIX   |STK'
                   DISPLAY '-------+----------------------+--------+---'
                   MOVE LOW-VALUES TO PRD-CODE
                   START PRODUITS KEY >= PRD-CODE
                       INVALID KEY
                           DISPLAY 'AUCUN PRODUIT'
                       NOT INVALID KEY
                           MOVE 0 TO WS-EOF
                           MOVE 0 TO WS-COUNT
                           PERFORM UNTIL WS-EOF = 1
                               READ PRODUITS NEXT
                                   AT END MOVE 1 TO WS-EOF
                                   NOT AT END
                                       IF PRD-FRN = FRN-CODE
                                           DISPLAY PRD-CODE ' | '
                                               PRD-LIBELLE ' | '
                                               PRD-PRIX ' | ' PRD-STOCK
                                           ADD 1 TO WS-COUNT
                                       END-IF
                               END-READ
                           END-PERFORM
                           DISPLAY '-------+----------------------+--------+---'
                           DISPLAY 'TOTAL: ' WS-COUNT ' PRODUITS'
                   END-START
           END-READ.
      * ================================================
      * SAISIE DES COMMANDES
      * ================================================
       MENU-COMMANDES.
           DISPLAY ' '.
           DISPLAY '--- COMMANDES ---'.
           DISPLAY '1. NOUVELLE COMMANDE'.
           DISPLAY '2. CONSULTER COMMANDE'.
           DISPLAY '3. VALIDER COMMANDE'.
           DISPLAY '4. ANNULER COMMANDE'.
           DISPLAY '5. LISTE PAR STATUT'.
           DISPLAY '0. RETOUR'.
           DISPLAY 'CHOIX:'.
           ACCEPT WS-CHOIX.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM NOUVELLE-COMMANDE
               WHEN 2 PERFORM CONSULTER-COMMANDE
               WHEN 3 PERFORM VALIDER-COMMANDE
               WHEN 4 PERFORM ANNULER-COMMANDE
               WHEN 5 PERFORM LISTER-COMMANDES
           END-EVALUATE.
       NOUVELLE-COMMANDE.
           DISPLAY 'CODE PRODUIT:'.
           ACCEPT PRD-CODE.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT INCONNU'
               NOT INVALID KEY
                   DISPLAY 'PRODUIT: ' PRD-LIBELLE
                   DISPLAY 'STOCK:   ' PRD-STOCK
                   DISPLAY 'PRIX:    ' PRD-PRIX
                   DISPLAY 'QUANTITE A COMMANDER:'
                   ACCEPT CMD-QTE
                   DISPLAY 'DATE (AAAAMMJJ):'
                   ACCEPT CMD-DATE
                   MOVE WS-NEXT-CMD TO CMD-NUM
                   ADD 1 TO WS-NEXT-CMD
                   MOVE PRD-FRN TO CMD-FRN
                   MOVE PRD-CODE TO CMD-PRD
                   MOVE PRD-PRIX TO CMD-PRIX
                   MOVE 'E' TO CMD-STATUT
                   WRITE COMMANDE-REC
                   COMPUTE WS-MONTANT = CMD-QTE * CMD-PRIX
                   DISPLAY '*** COMMANDE CREEE ***'
                   DISPLAY 'NUMERO:  ' CMD-NUM
                   DISPLAY 'MONTANT: ' WS-MONTANT
           END-READ.
       CONSULTER-COMMANDE.
           DISPLAY 'NUMERO COMMANDE:'.
           ACCEPT CMD-NUM.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE'
               NOT INVALID KEY
                   COMPUTE WS-MONTANT = CMD-QTE * CMD-PRIX
                   DISPLAY '------------------------'
                   DISPLAY 'COMMANDE N: ' CMD-NUM
                   DISPLAY 'FOURNIS:    ' CMD-FRN
                   DISPLAY 'PRODUIT:    ' CMD-PRD
                   DISPLAY 'QUANTITE:   ' CMD-QTE
                   DISPLAY 'PRIX UNIT:  ' CMD-PRIX
                   DISPLAY 'MONTANT:    ' WS-MONTANT
                   DISPLAY 'DATE:       ' CMD-DATE
                   EVALUATE CMD-STATUT
                       WHEN 'E' DISPLAY 'STATUT: EN COURS'
                       WHEN 'V' DISPLAY 'STATUT: VALIDEE'
                       WHEN 'R' DISPLAY 'STATUT: RECUE'
                       WHEN 'X' DISPLAY 'STATUT: ANNULEE'
                   END-EVALUATE
           END-READ.
       VALIDER-COMMANDE.
           DISPLAY 'NUMERO COMMANDE:'.
           ACCEPT CMD-NUM.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE'
               NOT INVALID KEY
                   IF CMD-STATUT = 'E'
                       MOVE 'V' TO CMD-STATUT
                       REWRITE COMMANDE-REC
                       DISPLAY '*** COMMANDE VALIDEE ***'
                       DISPLAY 'EN ATTENTE DE LIVRAISON'
                   ELSE
                       DISPLAY 'COMMANDE NON MODIFIABLE'
                   END-IF
           END-READ.
       ANNULER-COMMANDE.
           DISPLAY 'NUMERO COMMANDE:'.
           ACCEPT CMD-NUM.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE'
               NOT INVALID KEY
                   IF CMD-STATUT = 'E'
                       DISPLAY 'CONFIRMER ANNULATION (O/N)?'
                       ACCEPT WS-CONFIRM
                       IF WS-CONFIRM = 'O'
                           MOVE 'X' TO CMD-STATUT
                           REWRITE COMMANDE-REC
                           DISPLAY '*** COMMANDE ANNULEE ***'
                       END-IF
                   ELSE
                       DISPLAY 'SEULES LES CMD EN COURS PEUVENT ETRE ANNULEES'
                   END-IF
           END-READ.
       LISTER-COMMANDES.
           DISPLAY 'STATUT (E/V/R/X/T=TOUS):'.
           ACCEPT WS-CONFIRM.
           DISPLAY ' '.
           DISPLAY 'NUM    |FOUR|PRODUIT|QTE |MONTANT   |STAT'.
           DISPLAY '-------+----+-------+----+----------+----'.
           MOVE 0 TO CMD-NUM.
           START COMMANDES KEY >= CMD-NUM
               INVALID KEY
                   DISPLAY 'AUCUNE COMMANDE'
               NOT INVALID KEY
                   MOVE 0 TO WS-EOF
                   MOVE 0 TO WS-TOTAL
                   MOVE 0 TO WS-COUNT
                   PERFORM UNTIL WS-EOF = 1
                       READ COMMANDES NEXT
                           AT END MOVE 1 TO WS-EOF
                           NOT AT END
                               IF WS-CONFIRM = 'T'
                                   OR CMD-STATUT = WS-CONFIRM
                                   COMPUTE WS-MONTANT = CMD-QTE * CMD-PRIX
                                   ADD WS-MONTANT TO WS-TOTAL
                                   ADD 1 TO WS-COUNT
                                   DISPLAY CMD-NUM ' |' CMD-FRN '|'
                                       CMD-PRD '|' CMD-QTE '|'
                                       WS-MONTANT '|' CMD-STATUT
                               END-IF
                       END-READ
                   END-PERFORM
                   DISPLAY '-------+----+-------+----+----------+----'
                   DISPLAY 'TOTAL: ' WS-COUNT ' COMMANDES'
                   DISPLAY 'MONTANT TOTAL: ' WS-TOTAL
           END-START.
      * ================================================
      * RECEPTION DES COMMANDES
      * ================================================
       MENU-RECEPTION.
           DISPLAY ' '.
           DISPLAY '--- RECEPTION ---'.
           DISPLAY '1. RECEVOIR UNE COMMANDE'.
           DISPLAY '2. COMMANDES EN ATTENTE'.
           DISPLAY '0. RETOUR'.
           DISPLAY 'CHOIX:'.
           ACCEPT WS-CHOIX.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM RECEVOIR-COMMANDE
               WHEN 2 PERFORM COMMANDES-EN-ATTENTE
           END-EVALUATE.
       RECEVOIR-COMMANDE.
           DISPLAY 'NUMERO COMMANDE:'.
           ACCEPT CMD-NUM.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE'
               NOT INVALID KEY
                   IF CMD-STATUT = 'V'
                       DISPLAY 'PRODUIT: ' CMD-PRD
                       DISPLAY 'QTE COMMANDEE: ' CMD-QTE
                       DISPLAY 'CONFIRMER RECEPTION (O/N)?'
                       ACCEPT WS-CONFIRM
                       IF WS-CONFIRM = 'O'
                           MOVE 'R' TO CMD-STATUT
                           REWRITE COMMANDE-REC
                           MOVE CMD-PRD TO PRD-CODE
                           READ PRODUITS
                               NOT INVALID KEY
                                   ADD CMD-QTE TO PRD-STOCK
                                   REWRITE PRODUIT-REC
                                   DISPLAY '*** RECEPTION ENREGISTREE ***'
                                   DISPLAY 'NOUVEAU STOCK: ' PRD-STOCK
                           END-READ
                       END-IF
                   ELSE
                       IF CMD-STATUT = 'E'
                           DISPLAY 'COMMANDE NON VALIDEE'
                       ELSE
                           IF CMD-STATUT = 'R'
                               DISPLAY 'COMMANDE DEJA RECUE'
                           ELSE
                               DISPLAY 'COMMANDE ANNULEE'
                           END-IF
                       END-IF
                   END-IF
           END-READ.
       COMMANDES-EN-ATTENTE.
           DISPLAY ' '.
           DISPLAY '=== COMMANDES EN ATTENTE DE RECEPTION ==='.
           DISPLAY 'NUM    |FOUR|PRODUIT|QTE |DATE'.
           DISPLAY '-------+----+-------+----+--------'.
           MOVE 0 TO CMD-NUM.
           START COMMANDES KEY >= CMD-NUM
               INVALID KEY
                   DISPLAY 'AUCUNE COMMANDE'
               NOT INVALID KEY
                   MOVE 0 TO WS-EOF
                   MOVE 0 TO WS-COUNT
                   PERFORM UNTIL WS-EOF = 1
                       READ COMMANDES NEXT
                           AT END MOVE 1 TO WS-EOF
                           NOT AT END
                               IF CMD-STATUT = 'V'
                                   ADD 1 TO WS-COUNT
                                   DISPLAY CMD-NUM ' |' CMD-FRN '|'
                                       CMD-PRD '|' CMD-QTE '|' CMD-DATE
                               END-IF
                       END-READ
                   END-PERFORM
                   DISPLAY '-------+----+-------+----+--------'
                   DISPLAY WS-COUNT ' COMMANDES EN ATTENTE'
           END-START.

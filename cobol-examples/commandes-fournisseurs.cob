       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-ACHATS.
      *===============================================
      * SYSTEME COMPLET DE GESTION DES ACHATS
      * Interface ecran IBM 3270
      * - Gestion des fournisseurs (CRUD)
      * - Catalogue produits par fournisseur
      * - Saisie de commandes avec lignes
      * - Suivi et reception des commandes
      *===============================================
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
       01 WS-LIGNE         PIC 99 VALUE 0.
      * Variables pour la pagination
       01 WS-PAGE          PIC 999 VALUE 1.
       01 WS-PAGE-COUNT    PIC 99 VALUE 0.
       01 WS-NAV           PIC X VALUE SPACE.
       01 WS-LIGNES-MAX    PIC 99 VALUE 8.
       01 WS-TOTAL-GLOBAL  PIC 9999 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O FOURNISSEURS.
           OPEN I-O PRODUITS.
           OPEN I-O COMMANDES.
           PERFORM UNTIL WS-FIN = 1
               PERFORM AFFICHER-MENU-PRINCIPAL
               ACCEPT WS-MENU LINE 18 POSITION 25
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
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'AU REVOIR' LINE 12 POSITION 35 HIGHLIGHT.
           STOP RUN.

       AFFICHER-MENU-PRINCIPAL.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 2 POSITION 18.
           DISPLAY '------------------------------------------'
               LINE 2 POSITION 19.
           DISPLAY '+' LINE 2 POSITION 61.
           DISPLAY '|' LINE 3 POSITION 18.
           DISPLAY '      SYSTEME GESTION DES ACHATS         '
               LINE 3 POSITION 20 HIGHLIGHT.
           DISPLAY '|' LINE 3 POSITION 61.
           DISPLAY '+' LINE 4 POSITION 18.
           DISPLAY '------------------------------------------'
               LINE 4 POSITION 19.
           DISPLAY '+' LINE 4 POSITION 61.
           DISPLAY '1.' LINE 7 POSITION 26.
           DISPLAY 'Gestion Fournisseurs' LINE 7 POSITION 29.
           DISPLAY '2.' LINE 9 POSITION 26.
           DISPLAY 'Gestion Produits' LINE 9 POSITION 29.
           DISPLAY '3.' LINE 11 POSITION 26.
           DISPLAY 'Saisie Commandes' LINE 11 POSITION 29.
           DISPLAY '4.' LINE 13 POSITION 26.
           DISPLAY 'Reception Commandes' LINE 13 POSITION 29.
           DISPLAY '9.' LINE 15 POSITION 26.
           DISPLAY 'Quitter' LINE 15 POSITION 29.
           DISPLAY 'Votre choix:' LINE 18 POSITION 12.
           DISPLAY '[_]' LINE 18 POSITION 25 REVERSE-VIDEO.

      *===============================================
      * GESTION DES FOURNISSEURS
      *===============================================
       MENU-FOURNISSEURS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== FOURNISSEURS ===' LINE 2 POSITION 30 HIGHLIGHT.
           DISPLAY '1.' LINE 5 POSITION 28.
           DISPLAY 'Creer fournisseur' LINE 5 POSITION 31.
           DISPLAY '2.' LINE 7 POSITION 28.
           DISPLAY 'Consulter' LINE 7 POSITION 31.
           DISPLAY '3.' LINE 9 POSITION 28.
           DISPLAY 'Modifier' LINE 9 POSITION 31.
           DISPLAY '4.' LINE 11 POSITION 28.
           DISPLAY 'Lister tous' LINE 11 POSITION 31.
           DISPLAY '0.' LINE 13 POSITION 28.
           DISPLAY 'Retour' LINE 13 POSITION 31.
           DISPLAY 'Choix:' LINE 16 POSITION 28.
           DISPLAY '[_]' LINE 16 POSITION 35 REVERSE-VIDEO.
           ACCEPT WS-CHOIX LINE 16 POSITION 36.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM CREER-FOURNISSEUR
               WHEN 2 PERFORM CONSULTER-FOURNISSEUR
               WHEN 3 PERFORM MODIFIER-FOURNISSEUR
               WHEN 4 PERFORM LISTER-FOURNISSEURS
           END-EVALUATE.

       CREER-FOURNISSEUR.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== NOUVEAU FOURNISSEUR ===' LINE 2 POSITION 26
               HIGHLIGHT.
           DISPLAY 'Code (4 car):' LINE 5 POSITION 15.
           DISPLAY '[____]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT FRN-CODE LINE 5 POSITION 31.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'Nom:' LINE 7 POSITION 15
                   DISPLAY '[____________________]' LINE 7 POSITION 30
                       REVERSE-VIDEO
                   ACCEPT FRN-NOM LINE 7 POSITION 31
                   DISPLAY 'Telephone:' LINE 9 POSITION 15
                   DISPLAY '[__________]' LINE 9 POSITION 30 REVERSE-VIDEO
                   ACCEPT FRN-TEL LINE 9 POSITION 31
                   DISPLAY 'Delai livraison (jours):' LINE 11 POSITION 15
                   DISPLAY '[__]' LINE 11 POSITION 40 REVERSE-VIDEO
                   ACCEPT FRN-DELAI LINE 11 POSITION 41
                   WRITE FOURNIS-REC
                   DISPLAY '*** FOURNISSEUR CREE ***' LINE 14 POSITION 28
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY 'ERREUR: CODE DEJA UTILISE' LINE 14 POSITION 27
                       HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       CONSULTER-FOURNISSEUR.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CONSULTER FOURNISSEUR ===' LINE 2 POSITION 25
               HIGHLIGHT.
           DISPLAY 'Code fournisseur:' LINE 5 POSITION 15.
           DISPLAY '[____]' LINE 5 POSITION 33 REVERSE-VIDEO.
           ACCEPT FRN-CODE LINE 5 POSITION 34.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR NON TROUVE' LINE 12 POSITION 29
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+------------------------+'
                       LINE 7 POSITION 22
                   DISPLAY '|' LINE 8 POSITION 22
                   DISPLAY 'Code:' LINE 8 POSITION 24
                   DISPLAY FRN-CODE LINE 8 POSITION 32 HIGHLIGHT
                   DISPLAY '|' LINE 8 POSITION 46
                   DISPLAY '|' LINE 9 POSITION 22
                   DISPLAY 'Nom:' LINE 9 POSITION 24
                   DISPLAY FRN-NOM LINE 9 POSITION 32
                   DISPLAY '|' LINE 9 POSITION 46
                   DISPLAY '|' LINE 10 POSITION 22
                   DISPLAY 'Tel:' LINE 10 POSITION 24
                   DISPLAY FRN-TEL LINE 10 POSITION 32
                   DISPLAY '|' LINE 10 POSITION 46
                   DISPLAY '|' LINE 11 POSITION 22
                   DISPLAY 'Delai:' LINE 11 POSITION 24
                   DISPLAY FRN-DELAI LINE 11 POSITION 32 HIGHLIGHT
                   DISPLAY 'jours' LINE 11 POSITION 36
                   DISPLAY '|' LINE 11 POSITION 46
                   DISPLAY '+------------------------+'
                       LINE 12 POSITION 22
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       MODIFIER-FOURNISSEUR.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== MODIFIER FOURNISSEUR ===' LINE 2 POSITION 26
               HIGHLIGHT.
           DISPLAY 'Code fournisseur:' LINE 5 POSITION 15.
           DISPLAY '[____]' LINE 5 POSITION 33 REVERSE-VIDEO.
           ACCEPT FRN-CODE LINE 5 POSITION 34.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR NON TROUVE' LINE 12 POSITION 29
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY 'Nom actuel:' LINE 7 POSITION 15
                   DISPLAY FRN-NOM LINE 7 POSITION 30 HIGHLIGHT
                   DISPLAY 'Nouveau nom:' LINE 8 POSITION 15
                   DISPLAY '[____________________]' LINE 8 POSITION 30
                       REVERSE-VIDEO
                   ACCEPT FRN-NOM LINE 8 POSITION 31
                   DISPLAY 'Nouveau tel:' LINE 10 POSITION 15
                   DISPLAY '[__________]' LINE 10 POSITION 30 REVERSE-VIDEO
                   ACCEPT FRN-TEL LINE 10 POSITION 31
                   DISPLAY 'Nouveau delai:' LINE 12 POSITION 15
                   DISPLAY '[__]' LINE 12 POSITION 30 REVERSE-VIDEO
                   ACCEPT FRN-DELAI LINE 12 POSITION 31
                   REWRITE FOURNIS-REC
                   DISPLAY '*** FOURNISSEUR MODIFIE ***' LINE 15 POSITION 26
                       HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       LISTER-FOURNISSEURS.
           MOVE 1 TO WS-PAGE.
           MOVE 0 TO WS-TOTAL-GLOBAL.
           MOVE LOW-VALUES TO FRN-CODE.
           START FOURNISSEURS KEY >= FRN-CODE
               INVALID KEY
                   DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS
                   DISPLAY '=== LISTE FOURNISSEURS ===' LINE 2 POSITION 27
                       HIGHLIGHT
                   DISPLAY 'AUCUN FOURNISSEUR' LINE 10 POSITION 31 HIGHLIGHT
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK
                   ACCEPT WS-CHOIX LINE 20 POSITION 49
               NOT INVALID KEY
                   MOVE 0 TO WS-EOF
                   MOVE SPACE TO WS-NAV
                   PERFORM UNTIL WS-NAV = 'Q'
                       MOVE LOW-VALUES TO FRN-CODE
                       START FOURNISSEURS KEY >= FRN-CODE
                       MOVE 0 TO WS-EOF
                       PERFORM AFFICHER-PAGE-FOURNISSEURS
                       IF WS-EOF = 0
                           DISPLAY '[P]rec [S]uite [Q]uitter' LINE 20
                               POSITION 25
                           ACCEPT WS-NAV LINE 20 POSITION 51
                           EVALUATE WS-NAV
                               WHEN 'S'
                                   ADD 1 TO WS-PAGE
                               WHEN 's'
                                   ADD 1 TO WS-PAGE
                               WHEN 'P'
                                   IF WS-PAGE > 1
                                       SUBTRACT 1 FROM WS-PAGE
                                   END-IF
                               WHEN 'p'
                                   IF WS-PAGE > 1
                                       SUBTRACT 1 FROM WS-PAGE
                                   END-IF
                               WHEN OTHER
                                   MOVE 'Q' TO WS-NAV
                           END-EVALUATE
                       ELSE
                           DISPLAY 'Fin - Appuyez ENTREE...' LINE 20
                               POSITION 28 BLINK
                           ACCEPT WS-CHOIX LINE 20 POSITION 52
                           MOVE 'Q' TO WS-NAV
                       END-IF
                   END-PERFORM
           END-START.

       AFFICHER-PAGE-FOURNISSEURS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== LISTE FOURNISSEURS ===' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'Page' LINE 2 POSITION 55.
           DISPLAY WS-PAGE LINE 2 POSITION 60.
           DISPLAY 'CODE' LINE 5 POSITION 5 UNDERLINE.
           DISPLAY 'NOM' LINE 5 POSITION 12 UNDERLINE.
           DISPLAY 'TELEPHONE' LINE 5 POSITION 35 UNDERLINE.
           DISPLAY 'DELAI' LINE 5 POSITION 48 UNDERLINE.
           DISPLAY '---------------------------------------------------'
               LINE 6 POSITION 5.
      *    Sauter les pages precedentes
           MOVE 0 TO WS-COUNT.
           COMPUTE WS-COUNT = (WS-PAGE - 1) * WS-LIGNES-MAX.
           IF WS-COUNT > 0
               PERFORM WS-COUNT TIMES
                   READ FOURNISSEURS NEXT
                       AT END MOVE 1 TO WS-EOF
                   END-READ
               END-PERFORM
           END-IF.
      *    Afficher la page courante
           MOVE 0 TO WS-PAGE-COUNT.
           MOVE 7 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-PAGE-COUNT >= WS-LIGNES-MAX
               READ FOURNISSEURS NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       DISPLAY FRN-CODE LINE WS-LIGNE POSITION 5
                       DISPLAY FRN-NOM LINE WS-LIGNE POSITION 12
                       DISPLAY FRN-TEL LINE WS-LIGNE POSITION 35
                       DISPLAY FRN-DELAI LINE WS-LIGNE POSITION 48
                       DISPLAY 'J' LINE WS-LIGNE POSITION 51
                       ADD 1 TO WS-LIGNE
                       ADD 1 TO WS-PAGE-COUNT
                       ADD 1 TO WS-TOTAL-GLOBAL
               END-READ
           END-PERFORM.
           DISPLAY '---------------------------------------------------'
               LINE 17 POSITION 5.
           DISPLAY 'Affichage:' LINE 18 POSITION 5.
           DISPLAY WS-TOTAL-GLOBAL LINE 18 POSITION 16 HIGHLIGHT.
           DISPLAY 'fournisseur(s)' LINE 18 POSITION 21.

      *===============================================
      * GESTION DES PRODUITS
      *===============================================
       MENU-PRODUITS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== PRODUITS ===' LINE 2 POSITION 32 HIGHLIGHT.
           DISPLAY '1.' LINE 5 POSITION 28.
           DISPLAY 'Ajouter produit' LINE 5 POSITION 31.
           DISPLAY '2.' LINE 7 POSITION 28.
           DISPLAY 'Consulter' LINE 7 POSITION 31.
           DISPLAY '3.' LINE 9 POSITION 28.
           DISPLAY 'Modifier prix/stock' LINE 9 POSITION 31.
           DISPLAY '4.' LINE 11 POSITION 28.
           DISPLAY 'Catalogue fournisseur' LINE 11 POSITION 31.
           DISPLAY '0.' LINE 13 POSITION 28.
           DISPLAY 'Retour' LINE 13 POSITION 31.
           DISPLAY 'Choix:' LINE 16 POSITION 28.
           DISPLAY '[_]' LINE 16 POSITION 35 REVERSE-VIDEO.
           ACCEPT WS-CHOIX LINE 16 POSITION 36.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM AJOUTER-PRODUIT
               WHEN 2 PERFORM CONSULTER-PRODUIT
               WHEN 3 PERFORM MODIFIER-PRODUIT
               WHEN 4 PERFORM CATALOGUE-FOURNISSEUR
           END-EVALUATE.

       AJOUTER-PRODUIT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== NOUVEAU PRODUIT ===' LINE 2 POSITION 28 HIGHLIGHT.
           DISPLAY 'Code fournisseur:' LINE 5 POSITION 15.
           DISPLAY '[____]' LINE 5 POSITION 33 REVERSE-VIDEO.
           ACCEPT PRD-FRN LINE 5 POSITION 34.
           MOVE PRD-FRN TO FRN-CODE.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR INCONNU' LINE 12 POSITION 30
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY 'Fournisseur:' LINE 6 POSITION 15
                   DISPLAY FRN-NOM LINE 6 POSITION 30 HIGHLIGHT
                   DISPLAY 'Code produit (6 car):' LINE 8 POSITION 15
                   DISPLAY '[______]' LINE 8 POSITION 37 REVERSE-VIDEO
                   ACCEPT PRD-CODE LINE 8 POSITION 38
                   READ PRODUITS
                       INVALID KEY
                           DISPLAY 'Libelle:' LINE 10 POSITION 15
                           DISPLAY '[____________________]' LINE 10
                               POSITION 30 REVERSE-VIDEO
                           ACCEPT PRD-LIBELLE LINE 10 POSITION 31
                           DISPLAY 'Prix unitaire:' LINE 12 POSITION 15
                           DISPLAY '[_______]' LINE 12 POSITION 30
                               REVERSE-VIDEO
                           ACCEPT PRD-PRIX LINE 12 POSITION 31
                           DISPLAY 'Stock initial:' LINE 14 POSITION 15
                           DISPLAY '[____]' LINE 14 POSITION 30
                               REVERSE-VIDEO
                           ACCEPT PRD-STOCK LINE 14 POSITION 31
                           WRITE PRODUIT-REC
                           DISPLAY '*** PRODUIT AJOUTE ***' LINE 17
                               POSITION 29 HIGHLIGHT
                       NOT INVALID KEY
                           DISPLAY 'CODE PRODUIT DEJA EXISTANT' LINE 17
                               POSITION 27 HIGHLIGHT
                   END-READ
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 49.

       CONSULTER-PRODUIT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CONSULTER PRODUIT ===' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'Code produit:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT PRD-CODE LINE 5 POSITION 31.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT NON TROUVE' LINE 12 POSITION 31
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+---------------------------+'
                       LINE 7 POSITION 20
                   DISPLAY '|' LINE 8 POSITION 20
                   DISPLAY 'Code:' LINE 8 POSITION 22
                   DISPLAY PRD-CODE LINE 8 POSITION 32
                   DISPLAY '|' LINE 8 POSITION 48
                   DISPLAY '|' LINE 9 POSITION 20
                   DISPLAY 'Libelle:' LINE 9 POSITION 22
                   DISPLAY PRD-LIBELLE LINE 9 POSITION 32 HIGHLIGHT
                   DISPLAY '|' LINE 9 POSITION 48
                   DISPLAY '|' LINE 10 POSITION 20
                   DISPLAY 'Fournis:' LINE 10 POSITION 22
                   DISPLAY PRD-FRN LINE 10 POSITION 32
                   DISPLAY '|' LINE 10 POSITION 48
                   DISPLAY '|' LINE 11 POSITION 20
                   DISPLAY 'Prix:' LINE 11 POSITION 22
                   DISPLAY PRD-PRIX LINE 11 POSITION 32
                   DISPLAY 'EUR' LINE 11 POSITION 42
                   DISPLAY '|' LINE 11 POSITION 48
                   DISPLAY '|' LINE 12 POSITION 20
                   DISPLAY 'Stock:' LINE 12 POSITION 22
                   DISPLAY PRD-STOCK LINE 12 POSITION 32 HIGHLIGHT
                   DISPLAY '|' LINE 12 POSITION 48
                   DISPLAY '+---------------------------+'
                       LINE 13 POSITION 20
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       MODIFIER-PRODUIT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== MODIFIER PRODUIT ===' LINE 2 POSITION 28
               HIGHLIGHT.
           DISPLAY 'Code produit:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT PRD-CODE LINE 5 POSITION 31.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT NON TROUVE' LINE 12 POSITION 31
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY 'Produit:' LINE 7 POSITION 15
                   DISPLAY PRD-LIBELLE LINE 7 POSITION 25 HIGHLIGHT
                   DISPLAY 'Prix actuel:' LINE 9 POSITION 15
                   DISPLAY PRD-PRIX LINE 9 POSITION 30
                   DISPLAY 'Nouveau prix:' LINE 10 POSITION 15
                   DISPLAY '[_______]' LINE 10 POSITION 30 REVERSE-VIDEO
                   ACCEPT PRD-PRIX LINE 10 POSITION 31
                   DISPLAY 'Stock actuel:' LINE 12 POSITION 15
                   DISPLAY PRD-STOCK LINE 12 POSITION 30
                   DISPLAY 'Nouveau stock:' LINE 13 POSITION 15
                   DISPLAY '[____]' LINE 13 POSITION 30 REVERSE-VIDEO
                   ACCEPT PRD-STOCK LINE 13 POSITION 31
                   REWRITE PRODUIT-REC
                   DISPLAY '*** PRODUIT MODIFIE ***' LINE 16 POSITION 28
                       HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 49.

       CATALOGUE-FOURNISSEUR.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CATALOGUE FOURNISSEUR ===' LINE 2 POSITION 25
               HIGHLIGHT.
           DISPLAY 'Code fournisseur:' LINE 4 POSITION 15.
           DISPLAY '[____]' LINE 4 POSITION 33 REVERSE-VIDEO.
           ACCEPT FRN-CODE LINE 4 POSITION 34.
           READ FOURNISSEURS
               INVALID KEY
                   DISPLAY 'FOURNISSEUR INCONNU' LINE 12 POSITION 30
                       HIGHLIGHT
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK
                   ACCEPT WS-CHOIX LINE 20 POSITION 49
               NOT INVALID KEY
                   MOVE 1 TO WS-PAGE
                   MOVE 0 TO WS-TOTAL-GLOBAL
                   MOVE LOW-VALUES TO PRD-CODE
                   START PRODUITS KEY >= PRD-CODE
                       INVALID KEY
                           DISPLAY 'Catalogue:' LINE 5 POSITION 15
                           DISPLAY FRN-NOM LINE 5 POSITION 26 HIGHLIGHT
                           DISPLAY 'AUCUN PRODUIT' LINE 12 POSITION 33
                           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31
                               BLINK
                           ACCEPT WS-CHOIX LINE 20 POSITION 49
                       NOT INVALID KEY
                           MOVE 0 TO WS-EOF
                           MOVE SPACE TO WS-NAV
                           PERFORM UNTIL WS-NAV = 'Q'
                               MOVE LOW-VALUES TO PRD-CODE
                               START PRODUITS KEY >= PRD-CODE
                               MOVE 0 TO WS-EOF
                               PERFORM AFFICHER-PAGE-CATALOGUE
                               IF WS-EOF = 0
                                   DISPLAY '[P]rec [S]uite [Q]uitter' LINE 20
                                       POSITION 25
                                   ACCEPT WS-NAV LINE 20 POSITION 51
                                   EVALUATE WS-NAV
                                       WHEN 'S'
                                           ADD 1 TO WS-PAGE
                                       WHEN 's'
                                           ADD 1 TO WS-PAGE
                                       WHEN 'P'
                                           IF WS-PAGE > 1
                                               SUBTRACT 1 FROM WS-PAGE
                                           END-IF
                                       WHEN 'p'
                                           IF WS-PAGE > 1
                                               SUBTRACT 1 FROM WS-PAGE
                                           END-IF
                                       WHEN OTHER
                                           MOVE 'Q' TO WS-NAV
                                   END-EVALUATE
                               ELSE
                                   DISPLAY 'Fin - Appuyez ENTREE...' LINE 20
                                       POSITION 28 BLINK
                                   ACCEPT WS-CHOIX LINE 20 POSITION 52
                                   MOVE 'Q' TO WS-NAV
                               END-IF
                           END-PERFORM
                   END-START
           END-READ.

       AFFICHER-PAGE-CATALOGUE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CATALOGUE FOURNISSEUR ===' LINE 2 POSITION 25
               HIGHLIGHT.
           DISPLAY 'Page' LINE 2 POSITION 55.
           DISPLAY WS-PAGE LINE 2 POSITION 60.
           DISPLAY 'Catalogue:' LINE 4 POSITION 15.
           DISPLAY FRN-NOM LINE 4 POSITION 26 HIGHLIGHT.
           DISPLAY 'CODE' LINE 6 POSITION 5 UNDERLINE.
           DISPLAY 'LIBELLE' LINE 6 POSITION 14 UNDERLINE.
           DISPLAY 'PRIX' LINE 6 POSITION 37 UNDERLINE.
           DISPLAY 'STOCK' LINE 6 POSITION 48 UNDERLINE.
           DISPLAY '---------------------------------------------------'
               LINE 7 POSITION 5.
      *    Sauter les pages precedentes
           MOVE 0 TO WS-COUNT.
           COMPUTE WS-COUNT = (WS-PAGE - 1) * WS-LIGNES-MAX.
           IF WS-COUNT > 0
               PERFORM UNTIL WS-COUNT = 0 OR WS-EOF = 1
                   READ PRODUITS NEXT
                       AT END MOVE 1 TO WS-EOF
                       NOT AT END
                           IF PRD-FRN = FRN-CODE
                               SUBTRACT 1 FROM WS-COUNT
                           END-IF
                   END-READ
               END-PERFORM
           END-IF.
      *    Afficher la page courante
           MOVE 0 TO WS-PAGE-COUNT.
           MOVE 8 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-PAGE-COUNT >= WS-LIGNES-MAX
               READ PRODUITS NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF PRD-FRN = FRN-CODE
                           DISPLAY PRD-CODE LINE WS-LIGNE POSITION 5
                           DISPLAY PRD-LIBELLE LINE WS-LIGNE POSITION 14
                           DISPLAY PRD-PRIX LINE WS-LIGNE POSITION 35
                           DISPLAY PRD-STOCK LINE WS-LIGNE POSITION 48
                           ADD 1 TO WS-LIGNE
                           ADD 1 TO WS-PAGE-COUNT
                           ADD 1 TO WS-TOTAL-GLOBAL
                       END-IF
               END-READ
           END-PERFORM.
           DISPLAY '---------------------------------------------------'
               LINE 17 POSITION 5.
           DISPLAY 'Affichage:' LINE 18 POSITION 5.
           DISPLAY WS-TOTAL-GLOBAL LINE 18 POSITION 16 HIGHLIGHT.
           DISPLAY 'produit(s) pour' LINE 18 POSITION 21.
           DISPLAY FRN-CODE LINE 18 POSITION 37.

      *===============================================
      * SAISIE DES COMMANDES
      *===============================================
       MENU-COMMANDES.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== COMMANDES ===' LINE 2 POSITION 31 HIGHLIGHT.
           DISPLAY '1.' LINE 5 POSITION 28.
           DISPLAY 'Nouvelle commande' LINE 5 POSITION 31.
           DISPLAY '2.' LINE 7 POSITION 28.
           DISPLAY 'Consulter commande' LINE 7 POSITION 31.
           DISPLAY '3.' LINE 9 POSITION 28.
           DISPLAY 'Valider commande' LINE 9 POSITION 31.
           DISPLAY '4.' LINE 11 POSITION 28.
           DISPLAY 'Annuler commande' LINE 11 POSITION 31.
           DISPLAY '5.' LINE 13 POSITION 28.
           DISPLAY 'Liste par statut' LINE 13 POSITION 31.
           DISPLAY '0.' LINE 15 POSITION 28.
           DISPLAY 'Retour' LINE 15 POSITION 31.
           DISPLAY 'Choix:' LINE 17 POSITION 28.
           DISPLAY '[_]' LINE 17 POSITION 35 REVERSE-VIDEO.
           ACCEPT WS-CHOIX LINE 17 POSITION 36.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM NOUVELLE-COMMANDE
               WHEN 2 PERFORM CONSULTER-COMMANDE
               WHEN 3 PERFORM VALIDER-COMMANDE
               WHEN 4 PERFORM ANNULER-COMMANDE
               WHEN 5 PERFORM LISTER-COMMANDES
           END-EVALUATE.

       NOUVELLE-COMMANDE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== NOUVELLE COMMANDE ===' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'Code produit:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 30 REVERSE-VIDEO.
           ACCEPT PRD-CODE LINE 5 POSITION 31.
           READ PRODUITS
               INVALID KEY
                   DISPLAY 'PRODUIT INCONNU' LINE 12 POSITION 32
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+---------------------------+'
                       LINE 7 POSITION 20
                   DISPLAY '|' LINE 8 POSITION 20
                   DISPLAY 'Produit:' LINE 8 POSITION 22
                   DISPLAY PRD-LIBELLE LINE 8 POSITION 32 HIGHLIGHT
                   DISPLAY '|' LINE 8 POSITION 48
                   DISPLAY '|' LINE 9 POSITION 20
                   DISPLAY 'Stock:' LINE 9 POSITION 22
                   DISPLAY PRD-STOCK LINE 9 POSITION 32
                   DISPLAY '|' LINE 9 POSITION 48
                   DISPLAY '|' LINE 10 POSITION 20
                   DISPLAY 'Prix:' LINE 10 POSITION 22
                   DISPLAY PRD-PRIX LINE 10 POSITION 32
                   DISPLAY 'EUR' LINE 10 POSITION 42
                   DISPLAY '|' LINE 10 POSITION 48
                   DISPLAY '+---------------------------+'
                       LINE 11 POSITION 20
                   DISPLAY 'Quantite a commander:' LINE 13 POSITION 15
                   DISPLAY '[____]' LINE 13 POSITION 38 REVERSE-VIDEO
                   ACCEPT CMD-QTE LINE 13 POSITION 39
                   DISPLAY 'Date (AAAAMMJJ):' LINE 14 POSITION 15
                   DISPLAY '[________]' LINE 14 POSITION 38 REVERSE-VIDEO
                   ACCEPT CMD-DATE LINE 14 POSITION 39
                   MOVE WS-NEXT-CMD TO CMD-NUM
                   ADD 1 TO WS-NEXT-CMD
                   MOVE PRD-FRN TO CMD-FRN
                   MOVE PRD-CODE TO CMD-PRD
                   MOVE PRD-PRIX TO CMD-PRIX
                   MOVE 'E' TO CMD-STATUT
                   WRITE COMMANDE-REC
                   COMPUTE WS-MONTANT = CMD-QTE * CMD-PRIX
                   DISPLAY '*** COMMANDE CREEE ***' LINE 16 POSITION 29
                       HIGHLIGHT
                   DISPLAY 'Numero:' LINE 17 POSITION 20
                   DISPLAY CMD-NUM LINE 17 POSITION 28 HIGHLIGHT
                   DISPLAY 'Montant:' LINE 18 POSITION 20
                   DISPLAY WS-MONTANT LINE 18 POSITION 29 HIGHLIGHT
                   DISPLAY 'EUR' LINE 18 POSITION 40
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 21 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 21 POSITION 49.

       CONSULTER-COMMANDE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CONSULTER COMMANDE ===' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'Numero commande:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 32 REVERSE-VIDEO.
           ACCEPT CMD-NUM LINE 5 POSITION 33.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE' LINE 12 POSITION 30
                       HIGHLIGHT
               NOT INVALID KEY
                   COMPUTE WS-MONTANT = CMD-QTE * CMD-PRIX
                   DISPLAY '+-----------------------------+'
                       LINE 7 POSITION 19
                   DISPLAY '|' LINE 8 POSITION 19
                   DISPLAY 'Commande N:' LINE 8 POSITION 21
                   DISPLAY CMD-NUM LINE 8 POSITION 33 HIGHLIGHT
                   DISPLAY '|' LINE 8 POSITION 49
                   DISPLAY '|' LINE 9 POSITION 19
                   DISPLAY 'Fournisseur:' LINE 9 POSITION 21
                   DISPLAY CMD-FRN LINE 9 POSITION 35
                   DISPLAY '|' LINE 9 POSITION 49
                   DISPLAY '|' LINE 10 POSITION 19
                   DISPLAY 'Produit:' LINE 10 POSITION 21
                   DISPLAY CMD-PRD LINE 10 POSITION 33
                   DISPLAY '|' LINE 10 POSITION 49
                   DISPLAY '|' LINE 11 POSITION 19
                   DISPLAY 'Quantite:' LINE 11 POSITION 21
                   DISPLAY CMD-QTE LINE 11 POSITION 33
                   DISPLAY '|' LINE 11 POSITION 49
                   DISPLAY '|' LINE 12 POSITION 19
                   DISPLAY 'Prix unit:' LINE 12 POSITION 21
                   DISPLAY CMD-PRIX LINE 12 POSITION 33
                   DISPLAY 'EUR' LINE 12 POSITION 43
                   DISPLAY '|' LINE 12 POSITION 49
                   DISPLAY '|' LINE 13 POSITION 19
                   DISPLAY 'Montant:' LINE 13 POSITION 21
                   DISPLAY WS-MONTANT LINE 13 POSITION 33 HIGHLIGHT
                   DISPLAY 'EUR' LINE 13 POSITION 43
                   DISPLAY '|' LINE 13 POSITION 49
                   DISPLAY '|' LINE 14 POSITION 19
                   DISPLAY 'Date:' LINE 14 POSITION 21
                   DISPLAY CMD-DATE LINE 14 POSITION 33
                   DISPLAY '|' LINE 14 POSITION 49
                   DISPLAY '|' LINE 15 POSITION 19
                   DISPLAY 'Statut:' LINE 15 POSITION 21
                   EVALUATE CMD-STATUT
                       WHEN 'E' DISPLAY 'EN COURS' LINE 15 POSITION 33
                       WHEN 'V' DISPLAY 'VALIDEE' LINE 15 POSITION 33
                           HIGHLIGHT
                       WHEN 'R' DISPLAY 'RECUE' LINE 15 POSITION 33
                           HIGHLIGHT
                       WHEN 'X' DISPLAY 'ANNULEE' LINE 15 POSITION 33
                   END-EVALUATE
                   DISPLAY '|' LINE 15 POSITION 49
                   DISPLAY '+-----------------------------+'
                       LINE 16 POSITION 19
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 20 POSITION 49.

       VALIDER-COMMANDE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== VALIDER COMMANDE ===' LINE 2 POSITION 28
               HIGHLIGHT.
           DISPLAY 'Numero commande:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 32 REVERSE-VIDEO.
           ACCEPT CMD-NUM LINE 5 POSITION 33.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE' LINE 12 POSITION 30
                       HIGHLIGHT
               NOT INVALID KEY
                   IF CMD-STATUT = 'E'
                       MOVE 'V' TO CMD-STATUT
                       REWRITE COMMANDE-REC
                       DISPLAY '*** COMMANDE VALIDEE ***' LINE 10
                           POSITION 28 HIGHLIGHT
                       DISPLAY 'EN ATTENTE DE LIVRAISON' LINE 12
                           POSITION 28
                   ELSE
                       DISPLAY 'COMMANDE NON MODIFIABLE' LINE 12
                           POSITION 28 HIGHLIGHT
                   END-IF
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       ANNULER-COMMANDE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== ANNULER COMMANDE ===' LINE 2 POSITION 28
               HIGHLIGHT.
           DISPLAY 'Numero commande:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 32 REVERSE-VIDEO.
           ACCEPT CMD-NUM LINE 5 POSITION 33.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE' LINE 12 POSITION 30
                       HIGHLIGHT
               NOT INVALID KEY
                   IF CMD-STATUT = 'E'
                       DISPLAY 'Confirmer annulation (O/N)?'
                           LINE 8 POSITION 24
                       DISPLAY '[_]' LINE 8 POSITION 52 REVERSE-VIDEO
                       ACCEPT WS-CONFIRM LINE 8 POSITION 53
                       IF WS-CONFIRM = 'O'
                           MOVE 'X' TO CMD-STATUT
                           REWRITE COMMANDE-REC
                           DISPLAY '*** COMMANDE ANNULEE ***' LINE 12
                               POSITION 28 HIGHLIGHT
                       END-IF
                   ELSE
                       DISPLAY 'SEULES LES CMD EN COURS' LINE 10
                           POSITION 28
                       DISPLAY 'PEUVENT ETRE ANNULEES' LINE 11
                           POSITION 29 HIGHLIGHT
                   END-IF
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       LISTER-COMMANDES.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== LISTE DES COMMANDES ===' LINE 2 POSITION 26
               HIGHLIGHT.
           DISPLAY 'Statut (E/V/R/X/T=Tous):' LINE 4 POSITION 20.
           DISPLAY '[_]' LINE 4 POSITION 45 REVERSE-VIDEO.
           ACCEPT WS-CONFIRM LINE 4 POSITION 46.
           MOVE 1 TO WS-PAGE.
           MOVE 0 TO WS-TOTAL-GLOBAL.
           MOVE 0 TO WS-TOTAL.
           MOVE 0 TO CMD-NUM.
           START COMMANDES KEY >= CMD-NUM
               INVALID KEY
                   DISPLAY 'AUCUNE COMMANDE' LINE 10 POSITION 32
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK
                   ACCEPT WS-CHOIX LINE 20 POSITION 49
               NOT INVALID KEY
                   MOVE 0 TO WS-EOF
                   MOVE SPACE TO WS-NAV
                   PERFORM UNTIL WS-NAV = 'Q'
                       MOVE 0 TO CMD-NUM
                       START COMMANDES KEY >= CMD-NUM
                       MOVE 0 TO WS-EOF
                       PERFORM AFFICHER-PAGE-COMMANDES
                       IF WS-EOF = 0
                           DISPLAY '[P]rec [S]uite [Q]uitter' LINE 21
                               POSITION 25
                           ACCEPT WS-NAV LINE 21 POSITION 51
                           EVALUATE WS-NAV
                               WHEN 'S'
                                   ADD 1 TO WS-PAGE
                               WHEN 's'
                                   ADD 1 TO WS-PAGE
                               WHEN 'P'
                                   IF WS-PAGE > 1
                                       SUBTRACT 1 FROM WS-PAGE
                                   END-IF
                               WHEN 'p'
                                   IF WS-PAGE > 1
                                       SUBTRACT 1 FROM WS-PAGE
                                   END-IF
                               WHEN OTHER
                                   MOVE 'Q' TO WS-NAV
                           END-EVALUATE
                       ELSE
                           DISPLAY 'Fin - Appuyez ENTREE...' LINE 21
                               POSITION 28 BLINK
                           ACCEPT WS-CHOIX LINE 21 POSITION 52
                           MOVE 'Q' TO WS-NAV
                       END-IF
                   END-PERFORM
           END-START.

       AFFICHER-PAGE-COMMANDES.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== LISTE DES COMMANDES ===' LINE 2 POSITION 26
               HIGHLIGHT.
           DISPLAY 'Page' LINE 2 POSITION 55.
           DISPLAY WS-PAGE LINE 2 POSITION 60.
           DISPLAY 'Filtre:' LINE 4 POSITION 20.
           IF WS-CONFIRM = 'T'
               DISPLAY 'TOUS' LINE 4 POSITION 28 HIGHLIGHT
           ELSE
               DISPLAY WS-CONFIRM LINE 4 POSITION 28 HIGHLIGHT
           END-IF.
           DISPLAY 'NUM' LINE 6 POSITION 3 UNDERLINE.
           DISPLAY 'FRN' LINE 6 POSITION 11 UNDERLINE.
           DISPLAY 'PRODUIT' LINE 6 POSITION 17 UNDERLINE.
           DISPLAY 'QTE' LINE 6 POSITION 26 UNDERLINE.
           DISPLAY 'MONTANT' LINE 6 POSITION 32 UNDERLINE.
           DISPLAY 'STAT' LINE 6 POSITION 44 UNDERLINE.
           DISPLAY '------------------------------------------------'
               LINE 7 POSITION 3.
      *    Sauter les pages precedentes
           MOVE 0 TO WS-COUNT.
           COMPUTE WS-COUNT = (WS-PAGE - 1) * WS-LIGNES-MAX.
           IF WS-COUNT > 0
               PERFORM UNTIL WS-COUNT = 0 OR WS-EOF = 1
                   READ COMMANDES NEXT
                       AT END MOVE 1 TO WS-EOF
                       NOT AT END
                           IF WS-CONFIRM = 'T' OR CMD-STATUT = WS-CONFIRM
                               SUBTRACT 1 FROM WS-COUNT
                           END-IF
                   END-READ
               END-PERFORM
           END-IF.
      *    Afficher la page courante
           MOVE 0 TO WS-PAGE-COUNT.
           MOVE 8 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-PAGE-COUNT >= WS-LIGNES-MAX
               READ COMMANDES NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF WS-CONFIRM = 'T' OR CMD-STATUT = WS-CONFIRM
                           COMPUTE WS-MONTANT = CMD-QTE * CMD-PRIX
                           ADD WS-MONTANT TO WS-TOTAL
                           ADD 1 TO WS-TOTAL-GLOBAL
                           DISPLAY CMD-NUM LINE WS-LIGNE POSITION 3
                           DISPLAY CMD-FRN LINE WS-LIGNE POSITION 11
                           DISPLAY CMD-PRD LINE WS-LIGNE POSITION 17
                           DISPLAY CMD-QTE LINE WS-LIGNE POSITION 26
                           DISPLAY WS-MONTANT LINE WS-LIGNE POSITION 31
                           DISPLAY CMD-STATUT LINE WS-LIGNE POSITION 45
                           ADD 1 TO WS-LIGNE
                           ADD 1 TO WS-PAGE-COUNT
                       END-IF
               END-READ
           END-PERFORM.
           DISPLAY '------------------------------------------------'
               LINE 17 POSITION 3.
           DISPLAY 'Affichage:' LINE 18 POSITION 5.
           DISPLAY WS-TOTAL-GLOBAL LINE 18 POSITION 16 HIGHLIGHT.
           DISPLAY 'commande(s)' LINE 18 POSITION 21.
           DISPLAY 'Montant cumule:' LINE 19 POSITION 5.
           DISPLAY WS-TOTAL LINE 19 POSITION 21 HIGHLIGHT.
           DISPLAY 'EUR' LINE 19 POSITION 32.

      *===============================================
      * RECEPTION DES COMMANDES
      *===============================================
       MENU-RECEPTION.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== RECEPTION ===' LINE 2 POSITION 31 HIGHLIGHT.
           DISPLAY '1.' LINE 6 POSITION 28.
           DISPLAY 'Recevoir une commande' LINE 6 POSITION 31.
           DISPLAY '2.' LINE 8 POSITION 28.
           DISPLAY 'Commandes en attente' LINE 8 POSITION 31.
           DISPLAY '0.' LINE 10 POSITION 28.
           DISPLAY 'Retour' LINE 10 POSITION 31.
           DISPLAY 'Choix:' LINE 14 POSITION 28.
           DISPLAY '[_]' LINE 14 POSITION 35 REVERSE-VIDEO.
           ACCEPT WS-CHOIX LINE 14 POSITION 36.
           EVALUATE WS-CHOIX
               WHEN 1 PERFORM RECEVOIR-COMMANDE
               WHEN 2 PERFORM COMMANDES-EN-ATTENTE
           END-EVALUATE.

       RECEVOIR-COMMANDE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== RECEPTION COMMANDE ===' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'Numero commande:' LINE 5 POSITION 15.
           DISPLAY '[______]' LINE 5 POSITION 32 REVERSE-VIDEO.
           ACCEPT CMD-NUM LINE 5 POSITION 33.
           READ COMMANDES
               INVALID KEY
                   DISPLAY 'COMMANDE NON TROUVEE' LINE 12 POSITION 30
                       HIGHLIGHT
               NOT INVALID KEY
                   IF CMD-STATUT = 'V'
                       DISPLAY 'Produit:' LINE 7 POSITION 20
                       DISPLAY CMD-PRD LINE 7 POSITION 30 HIGHLIGHT
                       DISPLAY 'Qte commandee:' LINE 8 POSITION 20
                       DISPLAY CMD-QTE LINE 8 POSITION 35 HIGHLIGHT
                       DISPLAY 'Confirmer reception (O/N)?'
                           LINE 10 POSITION 22
                       DISPLAY '[_]' LINE 10 POSITION 49 REVERSE-VIDEO
                       ACCEPT WS-CONFIRM LINE 10 POSITION 50
                       IF WS-CONFIRM = 'O'
                           MOVE 'R' TO CMD-STATUT
                           REWRITE COMMANDE-REC
                           MOVE CMD-PRD TO PRD-CODE
                           READ PRODUITS
                               NOT INVALID KEY
                                   ADD CMD-QTE TO PRD-STOCK
                                   REWRITE PRODUIT-REC
                                   DISPLAY '*** RECEPTION ENREGISTREE ***'
                                       LINE 13 POSITION 25 HIGHLIGHT
                                   DISPLAY 'Nouveau stock:' LINE 14
                                       POSITION 22
                                   DISPLAY PRD-STOCK LINE 14 POSITION 37
                                       HIGHLIGHT
                           END-READ
                       END-IF
                   ELSE
                       IF CMD-STATUT = 'E'
                           DISPLAY 'COMMANDE NON VALIDEE' LINE 12
                               POSITION 30 HIGHLIGHT
                       ELSE
                           IF CMD-STATUT = 'R'
                               DISPLAY 'COMMANDE DEJA RECUE' LINE 12
                                   POSITION 30 HIGHLIGHT
                           ELSE
                               DISPLAY 'COMMANDE ANNULEE' LINE 12
                                   POSITION 32 HIGHLIGHT
                           END-IF
                       END-IF
                   END-IF
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 18 POSITION 31 BLINK.
           ACCEPT WS-CHOIX LINE 18 POSITION 49.

       COMMANDES-EN-ATTENTE.
           MOVE 1 TO WS-PAGE.
           MOVE 0 TO WS-TOTAL-GLOBAL.
           MOVE 0 TO CMD-NUM.
           START COMMANDES KEY >= CMD-NUM
               INVALID KEY
                   DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS
                   DISPLAY '=== COMMANDES EN ATTENTE DE RECEPTION ==='
                       LINE 2 POSITION 19 HIGHLIGHT
                   DISPLAY 'AUCUNE COMMANDE' LINE 10 POSITION 32
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 31 BLINK
                   ACCEPT WS-CHOIX LINE 20 POSITION 49
               NOT INVALID KEY
                   MOVE 0 TO WS-EOF
                   MOVE SPACE TO WS-NAV
                   PERFORM UNTIL WS-NAV = 'Q'
                       MOVE 0 TO CMD-NUM
                       START COMMANDES KEY >= CMD-NUM
                       MOVE 0 TO WS-EOF
                       PERFORM AFFICHER-PAGE-ATTENTE
                       IF WS-EOF = 0
                           DISPLAY '[P]rec [S]uite [Q]uitter' LINE 20
                               POSITION 25
                           ACCEPT WS-NAV LINE 20 POSITION 51
                           EVALUATE WS-NAV
                               WHEN 'S'
                                   ADD 1 TO WS-PAGE
                               WHEN 's'
                                   ADD 1 TO WS-PAGE
                               WHEN 'P'
                                   IF WS-PAGE > 1
                                       SUBTRACT 1 FROM WS-PAGE
                                   END-IF
                               WHEN 'p'
                                   IF WS-PAGE > 1
                                       SUBTRACT 1 FROM WS-PAGE
                                   END-IF
                               WHEN OTHER
                                   MOVE 'Q' TO WS-NAV
                           END-EVALUATE
                       ELSE
                           DISPLAY 'Fin - Appuyez ENTREE...' LINE 20
                               POSITION 28 BLINK
                           ACCEPT WS-CHOIX LINE 20 POSITION 52
                           MOVE 'Q' TO WS-NAV
                       END-IF
                   END-PERFORM
           END-START.

       AFFICHER-PAGE-ATTENTE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== COMMANDES EN ATTENTE DE RECEPTION ==='
               LINE 2 POSITION 19 HIGHLIGHT.
           DISPLAY 'Page' LINE 2 POSITION 62.
           DISPLAY WS-PAGE LINE 2 POSITION 67.
           DISPLAY 'NUM' LINE 5 POSITION 5 UNDERLINE.
           DISPLAY 'FRN' LINE 5 POSITION 14 UNDERLINE.
           DISPLAY 'PRODUIT' LINE 5 POSITION 21 UNDERLINE.
           DISPLAY 'QTE' LINE 5 POSITION 30 UNDERLINE.
           DISPLAY 'DATE' LINE 5 POSITION 38 UNDERLINE.
           DISPLAY '---------------------------------------------'
               LINE 6 POSITION 5.
      *    Sauter les pages precedentes
           MOVE 0 TO WS-COUNT.
           COMPUTE WS-COUNT = (WS-PAGE - 1) * WS-LIGNES-MAX.
           IF WS-COUNT > 0
               PERFORM UNTIL WS-COUNT = 0 OR WS-EOF = 1
                   READ COMMANDES NEXT
                       AT END MOVE 1 TO WS-EOF
                       NOT AT END
                           IF CMD-STATUT = 'V'
                               SUBTRACT 1 FROM WS-COUNT
                           END-IF
                   END-READ
               END-PERFORM
           END-IF.
      *    Afficher la page courante
           MOVE 0 TO WS-PAGE-COUNT.
           MOVE 7 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-PAGE-COUNT >= WS-LIGNES-MAX
               READ COMMANDES NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF CMD-STATUT = 'V'
                           ADD 1 TO WS-TOTAL-GLOBAL
                           DISPLAY CMD-NUM LINE WS-LIGNE POSITION 5
                           DISPLAY CMD-FRN LINE WS-LIGNE POSITION 14
                           DISPLAY CMD-PRD LINE WS-LIGNE POSITION 21
                           DISPLAY CMD-QTE LINE WS-LIGNE POSITION 30
                           DISPLAY CMD-DATE LINE WS-LIGNE POSITION 36
                           ADD 1 TO WS-LIGNE
                           ADD 1 TO WS-PAGE-COUNT
                       END-IF
               END-READ
           END-PERFORM.
           DISPLAY '---------------------------------------------'
               LINE 17 POSITION 5.
           DISPLAY WS-TOTAL-GLOBAL LINE 18 POSITION 25 HIGHLIGHT.
           DISPLAY 'commande(s) en attente' LINE 18 POSITION 30.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTS.
      *===============================================
      * GESTION CLIENTS - Interface IBM 3270
      * Demonstre les extensions screen control
      *===============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTS ASSIGN TO 'CLIENTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-ID.
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTS.
       01 CLIENT-REC.
           05 CLI-ID PIC 9(5).
           05 CLI-NOM PIC X(20).
           05 CLI-VILLE PIC X(15).
           05 CLI-SOLDE PIC S9(7)V99.
       WORKING-STORAGE SECTION.
       01 WS-CHOIX PIC 9 VALUE 0.
       01 WS-FIN PIC 9 VALUE 0.
       01 WS-FOUND PIC 9 VALUE 0.
       01 WS-LIGNE PIC 99 VALUE 0.
       01 WS-CONFIRM PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O CLIENTS.
           PERFORM UNTIL WS-FIN = 1
               PERFORM AFFICHER-MENU
               EVALUATE WS-CHOIX
                   WHEN 1 PERFORM AJOUTER-CLIENT
                   WHEN 2 PERFORM RECHERCHER-CLIENT
                   WHEN 3 PERFORM MODIFIER-CLIENT
                   WHEN 4 PERFORM SUPPRIMER-CLIENT
                   WHEN 5 PERFORM LISTER-CLIENTS
                   WHEN 9 MOVE 1 TO WS-FIN
               END-EVALUATE
           END-PERFORM.
           CLOSE CLIENTS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'Au revoir!' LINE 12 POSITION 35 HIGHLIGHT.
           STOP RUN.

       AFFICHER-MENU.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '================================================'
               LINE 2 POSITION 16 HIGHLIGHT.
           DISPLAY '       SYSTEME DE GESTION CLIENTS       '
               LINE 3 POSITION 16 REVERSE-VIDEO.
           DISPLAY '================================================'
               LINE 4 POSITION 16 HIGHLIGHT.
           DISPLAY '1. Ajouter un client' LINE 7 POSITION 25.
           DISPLAY '2. Rechercher un client' LINE 8 POSITION 25.
           DISPLAY '3. Modifier un client' LINE 9 POSITION 25.
           DISPLAY '4. Supprimer un client' LINE 10 POSITION 25.
           DISPLAY '5. Lister tous les clients' LINE 11 POSITION 25.
           DISPLAY '9. Quitter' LINE 13 POSITION 25 BLINK.
           DISPLAY 'Votre choix:' LINE 16 POSITION 25.
           ACCEPT WS-CHOIX LINE 16 POSITION 38.

       AJOUTER-CLIENT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- AJOUT CLIENT ---' LINE 2 POSITION 30
               HIGHLIGHT.
           DISPLAY 'ID Client (5 chiffres):' LINE 5 POSITION 10.
           ACCEPT CLI-ID LINE 5 POSITION 35.
           DISPLAY 'Nom:' LINE 7 POSITION 10.
           ACCEPT CLI-NOM LINE 7 POSITION 35.
           DISPLAY 'Ville:' LINE 9 POSITION 10.
           ACCEPT CLI-VILLE LINE 9 POSITION 35.
           DISPLAY 'Solde:' LINE 11 POSITION 10.
           ACCEPT CLI-SOLDE LINE 11 POSITION 35.
           WRITE CLIENT-REC
               INVALID KEY
                   DISPLAY 'ERREUR: ID deja existant!' LINE 15
                       POSITION 20 REVERSE-VIDEO
               NOT INVALID KEY
                   DISPLAY 'Client ajoute avec succes' LINE 15
                       POSITION 20 HIGHLIGHT
           END-WRITE.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 30.
           ACCEPT WS-CONFIRM.

       RECHERCHER-CLIENT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- RECHERCHE CLIENT ---' LINE 2 POSITION 28
               HIGHLIGHT.
           DISPLAY 'ID a rechercher:' LINE 5 POSITION 10.
           ACCEPT CLI-ID LINE 5 POSITION 30.
           READ CLIENTS
               INVALID KEY
                   DISPLAY 'Client non trouve!' LINE 10 POSITION 30
                       REVERSE-VIDEO
               NOT INVALID KEY
                   DISPLAY 'Fiche Client' LINE 8 POSITION 33
                       UNDERLINE
                   DISPLAY 'ID:' LINE 10 POSITION 15
                   DISPLAY CLI-ID LINE 10 POSITION 25 HIGHLIGHT
                   DISPLAY 'Nom:' LINE 11 POSITION 15
                   DISPLAY CLI-NOM LINE 11 POSITION 25 HIGHLIGHT
                   DISPLAY 'Ville:' LINE 12 POSITION 15
                   DISPLAY CLI-VILLE LINE 12 POSITION 25 HIGHLIGHT
                   DISPLAY 'Solde:' LINE 13 POSITION 15
                   DISPLAY CLI-SOLDE LINE 13 POSITION 25 HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 30.
           ACCEPT WS-CONFIRM.

       MODIFIER-CLIENT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- MODIFICATION CLIENT ---' LINE 2 POSITION 26
               HIGHLIGHT.
           DISPLAY 'ID a modifier:' LINE 5 POSITION 10.
           ACCEPT CLI-ID LINE 5 POSITION 30.
           READ CLIENTS
               INVALID KEY
                   DISPLAY 'Client non trouve!' LINE 10 POSITION 30
                       REVERSE-VIDEO
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 30
                   ACCEPT WS-CONFIRM
               NOT INVALID KEY
                   DISPLAY 'Nouveau nom:' LINE 8 POSITION 10
                   DISPLAY CLI-NOM LINE 8 POSITION 30 REVERSE-VIDEO
                   ACCEPT CLI-NOM LINE 8 POSITION 30
                   DISPLAY 'Nouvelle ville:' LINE 10 POSITION 10
                   DISPLAY CLI-VILLE LINE 10 POSITION 30 REVERSE-VIDEO
                   ACCEPT CLI-VILLE LINE 10 POSITION 30
                   REWRITE CLIENT-REC
                   DISPLAY 'Client modifie!' LINE 15 POSITION 32
                       HIGHLIGHT
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 30
                   ACCEPT WS-CONFIRM
           END-READ.

       SUPPRIMER-CLIENT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- SUPPRESSION CLIENT ---' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'ID a supprimer:' LINE 5 POSITION 10.
           ACCEPT CLI-ID LINE 5 POSITION 30.
           DISPLAY 'Confirmer suppression? (O/N)' LINE 8 POSITION 20
               BLINK.
           ACCEPT WS-CONFIRM LINE 8 POSITION 50.
           IF WS-CONFIRM = 'O' OR WS-CONFIRM = 'o'
               DELETE CLIENTS
                   INVALID KEY
                       DISPLAY 'Client non trouve!' LINE 12
                           POSITION 30 REVERSE-VIDEO
                   NOT INVALID KEY
                       DISPLAY 'Client supprime!' LINE 12
                           POSITION 32 HIGHLIGHT
               END-DELETE
           ELSE
               DISPLAY 'Suppression annulee' LINE 12 POSITION 30
           END-IF.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 30.
           ACCEPT WS-CONFIRM.

       LISTER-CLIENTS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- LISTE DES CLIENTS ---' LINE 2 POSITION 28
               HIGHLIGHT.
           DISPLAY 'ID    | NOM                  | VILLE'
               LINE 4 POSITION 10 UNDERLINE.
           DISPLAY '------+----------------------+---------------'
               LINE 5 POSITION 10.
           MOVE 0 TO CLI-ID.
           MOVE 6 TO WS-LIGNE.
           START CLIENTS KEY >= CLI-ID
               INVALID KEY
                   DISPLAY 'Aucun client enregistre' LINE 8
                       POSITION 28 REVERSE-VIDEO
           END-START.
           MOVE 0 TO WS-FOUND.
           PERFORM UNTIL WS-FOUND = 1 OR WS-LIGNE > 18
               READ CLIENTS NEXT
                   AT END MOVE 1 TO WS-FOUND
                   NOT AT END
                       DISPLAY CLI-ID LINE WS-LIGNE POSITION 10
                       DISPLAY '|' LINE WS-LIGNE POSITION 16
                       DISPLAY CLI-NOM LINE WS-LIGNE POSITION 18
                       DISPLAY '|' LINE WS-LIGNE POSITION 39
                       DISPLAY CLI-VILLE LINE WS-LIGNE POSITION 41
                       ADD 1 TO WS-LIGNE
               END-READ
           END-PERFORM.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 30.
           ACCEPT WS-CONFIRM.

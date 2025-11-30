       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTS.
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
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O CLIENTS.
           PERFORM UNTIL WS-FIN = 1
               DISPLAY '=== GESTION CLIENTS ==='
               DISPLAY '1. AJOUTER CLIENT'
               DISPLAY '2. RECHERCHER CLIENT'
               DISPLAY '3. MODIFIER CLIENT'
               DISPLAY '4. SUPPRIMER CLIENT'
               DISPLAY '5. LISTER TOUS'
               DISPLAY '9. QUITTER'
               DISPLAY 'CHOIX:'
               ACCEPT WS-CHOIX
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
           STOP RUN.
       AJOUTER-CLIENT.
           DISPLAY 'ID CLIENT:'.
           ACCEPT CLI-ID.
           DISPLAY 'NOM:'.
           ACCEPT CLI-NOM.
           DISPLAY 'VILLE:'.
           ACCEPT CLI-VILLE.
           DISPLAY 'SOLDE:'.
           ACCEPT CLI-SOLDE.
           WRITE CLIENT-REC
               INVALID KEY DISPLAY 'ERREUR: ID EXISTE'
               NOT INVALID KEY DISPLAY 'CLIENT AJOUTE'
           END-WRITE.
       RECHERCHER-CLIENT.
           DISPLAY 'ID A RECHERCHER:'.
           ACCEPT CLI-ID.
           READ CLIENTS
               INVALID KEY DISPLAY 'CLIENT NON TROUVE'
               NOT INVALID KEY
                   DISPLAY 'NOM: ' CLI-NOM
                   DISPLAY 'VILLE: ' CLI-VILLE
                   DISPLAY 'SOLDE: ' CLI-SOLDE
           END-READ.
       MODIFIER-CLIENT.
           DISPLAY 'ID A MODIFIER:'.
           ACCEPT CLI-ID.
           READ CLIENTS
               INVALID KEY DISPLAY 'CLIENT NON TROUVE'
               NOT INVALID KEY
                   DISPLAY 'NOUVEAU NOM:'
                   ACCEPT CLI-NOM
                   DISPLAY 'NOUVELLE VILLE:'
                   ACCEPT CLI-VILLE
                   REWRITE CLIENT-REC
                   DISPLAY 'CLIENT MODIFIE'
           END-READ.
       SUPPRIMER-CLIENT.
           DISPLAY 'ID A SUPPRIMER:'.
           ACCEPT CLI-ID.
           DELETE CLIENTS
               INVALID KEY DISPLAY 'CLIENT NON TROUVE'
               NOT INVALID KEY DISPLAY 'CLIENT SUPPRIME'
           END-DELETE.
       LISTER-CLIENTS.
           MOVE 0 TO CLI-ID.
           START CLIENTS KEY >= CLI-ID
               INVALID KEY DISPLAY 'FICHIER VIDE'
           END-START.
           MOVE 0 TO WS-FOUND.
           PERFORM UNTIL WS-FOUND = 1
               READ CLIENTS NEXT
                   AT END MOVE 1 TO WS-FOUND
                   NOT AT END
                       DISPLAY CLI-ID ' | ' CLI-NOM ' | ' CLI-VILLE
               END-READ
           END-PERFORM.
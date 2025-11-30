       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOTEL.
      *===============================================
      * SYSTEME DE RESERVATION HOTELIERE
      * Interface ecran IBM 3270
      *===============================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHAMBRES ASSIGN TO 'CHAMBRES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHB-NUM.
           SELECT RESERVATIONS ASSIGN TO 'RESERV.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RES-ID.
       DATA DIVISION.
       FILE SECTION.
       FD CHAMBRES.
       01 CHAMBRE-REC.
           05 CHB-NUM PIC 9(3).
           05 CHB-TYPE PIC X(10).
           05 CHB-PRIX PIC 9(4)V99.
           05 CHB-DISPO PIC 9 VALUE 1.
       FD RESERVATIONS.
       01 RESERV-REC.
           05 RES-ID PIC 9(8).
           05 RES-CLIENT PIC X(25).
           05 RES-CHAMBRE PIC 9(3).
           05 RES-DEBUT PIC 9(8).
           05 RES-NUITS PIC 99.
           05 RES-TOTAL PIC 9(6)V99.
       WORKING-STORAGE SECTION.
       01 WS-CHOIX PIC 9 VALUE 0.
       01 WS-FIN PIC 9 VALUE 0.
       01 WS-EOF PIC 9 VALUE 0.
       01 WS-LIGNE PIC 99 VALUE 0.
       01 WS-MSG PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O CHAMBRES.
           OPEN I-O RESERVATIONS.
           PERFORM UNTIL WS-FIN = 1
               PERFORM AFFICHER-MENU
               ACCEPT WS-CHOIX LINE 18 POSITION 25
               EVALUATE WS-CHOIX
                   WHEN 1 PERFORM VOIR-DISPOS
                   WHEN 2 PERFORM RESERVER
                   WHEN 3 PERFORM CONSULTER
                   WHEN 4 PERFORM ANNULER
                   WHEN 9 MOVE 1 TO WS-FIN
               END-EVALUATE
           END-PERFORM.
           CLOSE CHAMBRES.
           CLOSE RESERVATIONS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'AU REVOIR' LINE 12 POSITION 35 HIGHLIGHT.
           STOP RUN.

       AFFICHER-MENU.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '+' LINE 2 POSITION 20.
           DISPLAY '--------------------------------------'
               LINE 2 POSITION 21.
           DISPLAY '+' LINE 2 POSITION 59.
           DISPLAY '|' LINE 3 POSITION 20.
           DISPLAY '        HOTEL COBOL - RESERVATIONS        '
               LINE 3 POSITION 22 HIGHLIGHT.
           DISPLAY '|' LINE 3 POSITION 59.
           DISPLAY '+' LINE 4 POSITION 20.
           DISPLAY '--------------------------------------'
               LINE 4 POSITION 21.
           DISPLAY '+' LINE 4 POSITION 59.
           DISPLAY '1.' LINE 7 POSITION 28.
           DISPLAY 'Chambres disponibles' LINE 7 POSITION 31.
           DISPLAY '2.' LINE 9 POSITION 28.
           DISPLAY 'Nouvelle reservation' LINE 9 POSITION 31.
           DISPLAY '3.' LINE 11 POSITION 28.
           DISPLAY 'Consulter reservation' LINE 11 POSITION 31.
           DISPLAY '4.' LINE 13 POSITION 28.
           DISPLAY 'Annuler reservation' LINE 13 POSITION 31.
           DISPLAY '9.' LINE 15 POSITION 28.
           DISPLAY 'Quitter' LINE 15 POSITION 31.
           DISPLAY 'Votre choix:' LINE 18 POSITION 12.
           DISPLAY '[_]' LINE 18 POSITION 25 REVERSE-VIDEO.

       VOIR-DISPOS.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CHAMBRES DISPONIBLES ==='
               LINE 2 POSITION 26 HIGHLIGHT.
           DISPLAY 'NUM' LINE 5 POSITION 10 UNDERLINE.
           DISPLAY 'TYPE' LINE 5 POSITION 20 UNDERLINE.
           DISPLAY 'PRIX/NUIT' LINE 5 POSITION 35 UNDERLINE.
           DISPLAY '-------------------------------------------'
               LINE 6 POSITION 10.
           MOVE 0 TO CHB-NUM.
           START CHAMBRES KEY >= CHB-NUM.
           MOVE 0 TO WS-EOF.
           MOVE 7 TO WS-LIGNE.
           PERFORM UNTIL WS-EOF = 1 OR WS-LIGNE > 18
               READ CHAMBRES NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF CHB-DISPO = 1
                           DISPLAY CHB-NUM LINE WS-LIGNE POSITION 10
                           DISPLAY CHB-TYPE LINE WS-LIGNE POSITION 20
                           DISPLAY CHB-PRIX LINE WS-LIGNE POSITION 35
                           DISPLAY 'EUR' LINE WS-LIGNE POSITION 45
                           ADD 1 TO WS-LIGNE
                       END-IF
               END-READ
           END-PERFORM.
           DISPLAY 'Appuyez sur ENTREE...' LINE 22 POSITION 30 BLINK.
           ACCEPT WS-CHOIX LINE 22 POSITION 52.

       RESERVER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== NOUVELLE RESERVATION ==='
               LINE 2 POSITION 26 HIGHLIGHT.
           DISPLAY 'Numero chambre:' LINE 6 POSITION 15.
           DISPLAY '[___]' LINE 6 POSITION 32 REVERSE-VIDEO.
           ACCEPT CHB-NUM LINE 6 POSITION 33.
           READ CHAMBRES
               INVALID KEY
                   DISPLAY 'CHAMBRE INEXISTANTE' LINE 20 POSITION 30
                       HIGHLIGHT
                   DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 30
                   ACCEPT WS-CHOIX LINE 22 POSITION 48
                   GO TO RESERVER-FIN
           END-READ.
           IF CHB-DISPO = 0
               DISPLAY 'CHAMBRE NON DISPONIBLE' LINE 20 POSITION 28
                   HIGHLIGHT
               DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 30
               ACCEPT WS-CHOIX LINE 22 POSITION 48
               GO TO RESERVER-FIN
           END-IF.
           DISPLAY 'Type:' LINE 8 POSITION 15.
           DISPLAY CHB-TYPE LINE 8 POSITION 32 HIGHLIGHT.
           DISPLAY 'Prix:' LINE 9 POSITION 15.
           DISPLAY CHB-PRIX LINE 9 POSITION 32.
           DISPLAY 'EUR/nuit' LINE 9 POSITION 42.
           DISPLAY 'Nom client:' LINE 11 POSITION 15.
           DISPLAY '[                         ]' LINE 11 POSITION 32
               REVERSE-VIDEO.
           ACCEPT RES-CLIENT LINE 11 POSITION 33.
           DISPLAY 'Date arrivee (AAAAMMJJ):' LINE 13 POSITION 15.
           DISPLAY '[________]' LINE 13 POSITION 40 REVERSE-VIDEO.
           ACCEPT RES-DEBUT LINE 13 POSITION 41.
           DISPLAY 'Nombre de nuits:' LINE 15 POSITION 15.
           DISPLAY '[__]' LINE 15 POSITION 32 REVERSE-VIDEO.
           ACCEPT RES-NUITS LINE 15 POSITION 33.
           COMPUTE RES-ID = FUNCTION RANDOM * 99999999.
           MOVE CHB-NUM TO RES-CHAMBRE.
           COMPUTE RES-TOTAL = CHB-PRIX * RES-NUITS.
           WRITE RESERV-REC.
           MOVE 0 TO CHB-DISPO.
           REWRITE CHAMBRE-REC.
           DISPLAY '*** RESERVATION CONFIRMEE ***'
               LINE 18 POSITION 25 HIGHLIGHT.
           DISPLAY 'Numero:' LINE 19 POSITION 20.
           DISPLAY RES-ID LINE 19 POSITION 28 HIGHLIGHT.
           DISPLAY 'Total:' LINE 20 POSITION 20.
           DISPLAY RES-TOTAL LINE 20 POSITION 28.
           DISPLAY 'EUR' LINE 20 POSITION 40.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 30.
           ACCEPT WS-CHOIX LINE 22 POSITION 48.
       RESERVER-FIN.
           EXIT.

       CONSULTER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== CONSULTER RESERVATION ==='
               LINE 2 POSITION 25 HIGHLIGHT.
           DISPLAY 'Numero reservation:' LINE 6 POSITION 15.
           DISPLAY '[________]' LINE 6 POSITION 35 REVERSE-VIDEO.
           ACCEPT RES-ID LINE 6 POSITION 36.
           READ RESERVATIONS
               INVALID KEY
                   DISPLAY 'RESERVATION INEXISTANTE' LINE 12 POSITION 28
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY '+--------------------------+'
                       LINE 8 POSITION 22
                   DISPLAY '|' LINE 9 POSITION 22.
                   DISPLAY 'Client:' LINE 9 POSITION 24.
                   DISPLAY RES-CLIENT LINE 9 POSITION 32.
                   DISPLAY '|' LINE 9 POSITION 49.
                   DISPLAY '|' LINE 10 POSITION 22.
                   DISPLAY 'Chambre:' LINE 10 POSITION 24.
                   DISPLAY RES-CHAMBRE LINE 10 POSITION 33.
                   DISPLAY '|' LINE 10 POSITION 49.
                   DISPLAY '|' LINE 11 POSITION 22.
                   DISPLAY 'Arrivee:' LINE 11 POSITION 24.
                   DISPLAY RES-DEBUT LINE 11 POSITION 33.
                   DISPLAY '|' LINE 11 POSITION 49.
                   DISPLAY '|' LINE 12 POSITION 22.
                   DISPLAY 'Nuits:' LINE 12 POSITION 24.
                   DISPLAY RES-NUITS LINE 12 POSITION 33.
                   DISPLAY '|' LINE 12 POSITION 49.
                   DISPLAY '|' LINE 13 POSITION 22.
                   DISPLAY 'Total:' LINE 13 POSITION 24.
                   DISPLAY RES-TOTAL LINE 13 POSITION 33.
                   DISPLAY 'EUR' LINE 13 POSITION 44.
                   DISPLAY '|' LINE 13 POSITION 49.
                   DISPLAY '+--------------------------+'
                       LINE 14 POSITION 22
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 30.
           ACCEPT WS-CHOIX LINE 22 POSITION 48.

       ANNULER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '=== ANNULER RESERVATION ==='
               LINE 2 POSITION 26 HIGHLIGHT.
           DISPLAY 'Numero reservation:' LINE 6 POSITION 15.
           DISPLAY '[________]' LINE 6 POSITION 35 REVERSE-VIDEO.
           ACCEPT RES-ID LINE 6 POSITION 36.
           READ RESERVATIONS
               INVALID KEY
                   DISPLAY 'RESERVATION INEXISTANTE' LINE 12 POSITION 28
                       HIGHLIGHT
               NOT INVALID KEY
                   DISPLAY 'Client:' LINE 8 POSITION 20.
                   DISPLAY RES-CLIENT LINE 8 POSITION 28 HIGHLIGHT.
                   DISPLAY 'Chambre:' LINE 9 POSITION 20.
                   DISPLAY RES-CHAMBRE LINE 9 POSITION 29.
                   MOVE RES-CHAMBRE TO CHB-NUM
                   READ CHAMBRES
                   MOVE 1 TO CHB-DISPO
                   REWRITE CHAMBRE-REC
                   DELETE RESERVATIONS
                   DISPLAY '*** RESERVATION ANNULEE ***'
                       LINE 14 POSITION 26 HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 22 POSITION 30.
           ACCEPT WS-CHOIX LINE 22 POSITION 48.

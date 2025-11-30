       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOTEL.
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
       01 WS-NUITS PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O CHAMBRES.
           OPEN I-O RESERVATIONS.
           PERFORM UNTIL WS-FIN = 1
               DISPLAY ' '
               DISPLAY '===== HOTEL COBOL ====='
               DISPLAY '1. CHAMBRES DISPONIBLES'
               DISPLAY '2. NOUVELLE RESERVATION'
               DISPLAY '3. CONSULTER RESERVATION'
               DISPLAY '4. ANNULER RESERVATION'
               DISPLAY '9. QUITTER'
               DISPLAY 'CHOIX:'
               ACCEPT WS-CHOIX
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
           STOP RUN.
       VOIR-DISPOS.
           DISPLAY '=== CHAMBRES DISPONIBLES ==='.
           MOVE 0 TO CHB-NUM.
           START CHAMBRES KEY >= CHB-NUM.
           MOVE 0 TO WS-EOF.
           PERFORM UNTIL WS-EOF = 1
               READ CHAMBRES NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       IF CHB-DISPO = 1
                           DISPLAY 'N.' CHB-NUM ' | ' CHB-TYPE
                               ' | ' CHB-PRIX ' EUR/NUIT'
                       END-IF
               END-READ
           END-PERFORM.
       RESERVER.
           DISPLAY 'NUMERO CHAMBRE:'.
           ACCEPT CHB-NUM.
           READ CHAMBRES
               INVALID KEY
                   DISPLAY 'CHAMBRE INEXISTANTE'
                   GO TO RESERVER-FIN
           END-READ.
           IF CHB-DISPO = 0
               DISPLAY 'CHAMBRE NON DISPONIBLE'
               GO TO RESERVER-FIN
           END-IF.
           DISPLAY 'TYPE: ' CHB-TYPE ' - ' CHB-PRIX ' EUR'.
           DISPLAY 'NOM CLIENT:'.
           ACCEPT RES-CLIENT.
           DISPLAY 'DATE ARRIVEE (AAAAMMJJ):'.
           ACCEPT RES-DEBUT.
           DISPLAY 'NOMBRE DE NUITS:'.
           ACCEPT RES-NUITS.
           COMPUTE RES-ID = FUNCTION RANDOM * 99999999.
           MOVE CHB-NUM TO RES-CHAMBRE.
           COMPUTE RES-TOTAL = CHB-PRIX * RES-NUITS.
           WRITE RESERV-REC.
           MOVE 0 TO CHB-DISPO.
           REWRITE CHAMBRE-REC.
           DISPLAY '*** RESERVATION CONFIRMEE ***'.
           DISPLAY 'NUMERO: ' RES-ID.
           DISPLAY 'TOTAL: ' RES-TOTAL ' EUR'.
       RESERVER-FIN.
           EXIT.
       CONSULTER.
           DISPLAY 'NUMERO RESERVATION:'.
           ACCEPT RES-ID.
           READ RESERVATIONS
               INVALID KEY DISPLAY 'RESERVATION INEXISTANTE'
               NOT INVALID KEY
                   DISPLAY '========================='
                   DISPLAY 'CLIENT:  ' RES-CLIENT
                   DISPLAY 'CHAMBRE: ' RES-CHAMBRE
                   DISPLAY 'ARRIVEE: ' RES-DEBUT
                   DISPLAY 'NUITS:   ' RES-NUITS
                   DISPLAY 'TOTAL:   ' RES-TOTAL ' EUR'
           END-READ.
       ANNULER.
           DISPLAY 'NUMERO RESERVATION:'.
           ACCEPT RES-ID.
           READ RESERVATIONS
               INVALID KEY
                   DISPLAY 'RESERVATION INEXISTANTE'
               NOT INVALID KEY
                   MOVE RES-CHAMBRE TO CHB-NUM
                   READ CHAMBRES
                   MOVE 1 TO CHB-DISPO
                   REWRITE CHAMBRE-REC
                   DELETE RESERVATIONS
                   DISPLAY 'RESERVATION ANNULEE'
           END-READ.
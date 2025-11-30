       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQUE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COMPTES ASSIGN TO 'COMPTES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CPT-NUM.
       DATA DIVISION.
       FILE SECTION.
       FD COMPTES.
       01 COMPTE-REC.
           05 CPT-NUM PIC 9(10).
           05 CPT-NOM PIC X(25).
           05 CPT-SOLDE PIC S9(9)V99.
           05 CPT-TYPE PIC X(2).
       WORKING-STORAGE SECTION.
       01 WS-CHOIX PIC 9 VALUE 0.
       01 WS-MONTANT PIC 9(7)V99 VALUE 0.
       01 WS-FIN PIC 9 VALUE 0.
       01 WS-NUM-DEST PIC 9(10) VALUE 0.
       01 WS-SOLDE-TEMP PIC S9(9)V99 VALUE 0.
       01 COMPTE-DEST.
           05 CPT2-NUM PIC 9(10).
           05 CPT2-NOM PIC X(25).
           05 CPT2-SOLDE PIC S9(9)V99.
           05 CPT2-TYPE PIC X(2).
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O COMPTES.
           PERFORM UNTIL WS-FIN = 1
               DISPLAY ' '
               DISPLAY '====== BANQUE COBOL ======'
               DISPLAY '1. CONSULTER COMPTE'
               DISPLAY '2. DEPOSER'
               DISPLAY '3. RETIRER'
               DISPLAY '4. VIREMENT'
               DISPLAY '5. HISTORIQUE'
               DISPLAY '9. QUITTER'
               DISPLAY 'CHOIX:'
               ACCEPT WS-CHOIX
               EVALUATE WS-CHOIX
                   WHEN 1 PERFORM CONSULTER
                   WHEN 2 PERFORM DEPOSER
                   WHEN 3 PERFORM RETIRER
                   WHEN 4 PERFORM VIREMENT
                   WHEN 5 PERFORM HISTORIQUE
                   WHEN 9 MOVE 1 TO WS-FIN
               END-EVALUATE
           END-PERFORM.
           CLOSE COMPTES.
           DISPLAY 'MERCI DE VOTRE VISITE'.
           STOP RUN.
       CONSULTER.
           DISPLAY 'NUMERO DE COMPTE:'.
           ACCEPT CPT-NUM.
           READ COMPTES
               INVALID KEY DISPLAY 'COMPTE INEXISTANT'
               NOT INVALID KEY
                   DISPLAY '=========================='
                   DISPLAY 'TITULAIRE: ' CPT-NOM
                   DISPLAY 'TYPE:      ' CPT-TYPE
                   DISPLAY 'SOLDE:     ' CPT-SOLDE ' EUR'
                   DISPLAY '=========================='
           END-READ.
       DEPOSER.
           DISPLAY 'NUMERO DE COMPTE:'.
           ACCEPT CPT-NUM.
           READ COMPTES
               INVALID KEY DISPLAY 'COMPTE INEXISTANT'
               NOT INVALID KEY
                   DISPLAY 'SOLDE ACTUEL: ' CPT-SOLDE
                   DISPLAY 'MONTANT A DEPOSER:'
                   ACCEPT WS-MONTANT
                   ADD WS-MONTANT TO CPT-SOLDE
                   REWRITE COMPTE-REC
                   DISPLAY 'DEPOT EFFECTUE'
                   DISPLAY 'NOUVEAU SOLDE: ' CPT-SOLDE
           END-READ.
       RETIRER.
           DISPLAY 'NUMERO DE COMPTE:'.
           ACCEPT CPT-NUM.
           READ COMPTES
               INVALID KEY DISPLAY 'COMPTE INEXISTANT'
               NOT INVALID KEY
                   DISPLAY 'SOLDE ACTUEL: ' CPT-SOLDE
                   DISPLAY 'MONTANT A RETIRER:'
                   ACCEPT WS-MONTANT
                   IF WS-MONTANT > CPT-SOLDE
                       DISPLAY '*** SOLDE INSUFFISANT ***'
                   ELSE
                       SUBTRACT WS-MONTANT FROM CPT-SOLDE
                       REWRITE COMPTE-REC
                       DISPLAY 'RETRAIT EFFECTUE'
                       DISPLAY 'NOUVEAU SOLDE: ' CPT-SOLDE
                   END-IF
           END-READ.
       VIREMENT.
           DISPLAY 'COMPTE SOURCE:'.
           ACCEPT CPT-NUM.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'COMPTE SOURCE INEXISTANT'
                   GO TO VIREMENT-FIN
           END-READ.
           MOVE CPT-SOLDE TO WS-SOLDE-TEMP.
           DISPLAY 'SOLDE DISPONIBLE: ' CPT-SOLDE.
           DISPLAY 'COMPTE DESTINATAIRE:'.
           ACCEPT WS-NUM-DEST.
           DISPLAY 'MONTANT:'.
           ACCEPT WS-MONTANT.
           IF WS-MONTANT > WS-SOLDE-TEMP
               DISPLAY '*** SOLDE INSUFFISANT ***'
               GO TO VIREMENT-FIN
           END-IF.
           SUBTRACT WS-MONTANT FROM CPT-SOLDE.
           REWRITE COMPTE-REC.
           MOVE WS-NUM-DEST TO CPT-NUM.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'DESTINATAIRE INEXISTANT'
               NOT INVALID KEY
                   ADD WS-MONTANT TO CPT-SOLDE
                   REWRITE COMPTE-REC
                   DISPLAY 'VIREMENT EFFECTUE'
           END-READ.
       VIREMENT-FIN.
           EXIT.
       HISTORIQUE.
           DISPLAY '*** HISTORIQUE NON DISPONIBLE ***'.
           DISPLAY '(NECESSITE FICHIER MOUVEMENTS)'.
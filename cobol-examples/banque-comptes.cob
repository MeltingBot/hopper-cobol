       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANQUE.
      *===============================================
      * BANQUE COBOL - Gestion de comptes bancaires
      * Interface IBM 3270 avec screen control
      *===============================================
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
       01 WS-CONFIRM PIC X VALUE SPACE.
       01 WS-SOLDE-EDIT PIC Z(8)9.99-.
       01 COMPTE-DEST.
           05 CPT2-NUM PIC 9(10).
           05 CPT2-NOM PIC X(25).
           05 CPT2-SOLDE PIC S9(9)V99.
           05 CPT2-TYPE PIC X(2).
       PROCEDURE DIVISION.
       DEBUT.
           OPEN I-O COMPTES.
           PERFORM UNTIL WS-FIN = 1
               PERFORM AFFICHER-MENU
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
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY 'Merci de votre visite!' LINE 12 POSITION 29
               HIGHLIGHT.
           DISPLAY 'BANQUE COBOL' LINE 14 POSITION 34.
           STOP RUN.

       AFFICHER-MENU.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '    $$$   BANQUE COBOL   $$$    '
               LINE 2 POSITION 24 REVERSE-VIDEO.
           DISPLAY '================================'
               LINE 3 POSITION 24 HIGHLIGHT.
           DISPLAY '1. Consulter un compte' LINE 6 POSITION 28.
           DISPLAY '2. Deposer de l argent' LINE 7 POSITION 28.
           DISPLAY '3. Retirer de l argent' LINE 8 POSITION 28.
           DISPLAY '4. Effectuer un virement' LINE 9 POSITION 28.
           DISPLAY '5. Historique' LINE 10 POSITION 28.
           DISPLAY '9. Quitter' LINE 12 POSITION 28 BLINK.
           DISPLAY 'Votre choix:' LINE 15 POSITION 28.
           ACCEPT WS-CHOIX LINE 15 POSITION 42.

       CONSULTER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- CONSULTATION COMPTE ---' LINE 2 POSITION 27
               HIGHLIGHT.
           DISPLAY 'Numero de compte:' LINE 5 POSITION 15.
           ACCEPT CPT-NUM LINE 5 POSITION 35.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'COMPTE INEXISTANT!' LINE 10 POSITION 31
                       REVERSE-VIDEO
               NOT INVALID KEY
                   DISPLAY '+----------------------------+'
                       LINE 8 POSITION 20
                   DISPLAY '|      RELEVE DE COMPTE      |'
                       LINE 9 POSITION 20 HIGHLIGHT
                   DISPLAY '+----------------------------+'
                       LINE 10 POSITION 20
                   DISPLAY 'Compte N:' LINE 12 POSITION 22
                   DISPLAY CPT-NUM LINE 12 POSITION 35 UNDERLINE
                   DISPLAY 'Titulaire:' LINE 13 POSITION 22
                   DISPLAY CPT-NOM LINE 13 POSITION 35 HIGHLIGHT
                   DISPLAY 'Type:' LINE 14 POSITION 22
                   DISPLAY CPT-TYPE LINE 14 POSITION 35
                   MOVE CPT-SOLDE TO WS-SOLDE-EDIT
                   DISPLAY 'Solde:' LINE 15 POSITION 22
                   IF CPT-SOLDE < 0
                       DISPLAY WS-SOLDE-EDIT LINE 15 POSITION 35
                           REVERSE-VIDEO BLINK
                       DISPLAY ' EUR' LINE 15 POSITION 47
                   ELSE
                       DISPLAY WS-SOLDE-EDIT LINE 15 POSITION 35
                           HIGHLIGHT
                       DISPLAY ' EUR' LINE 15 POSITION 47
                   END-IF
                   DISPLAY '+----------------------------+'
                       LINE 16 POSITION 20
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32.
           ACCEPT WS-CONFIRM.

       DEPOSER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- DEPOT ---' LINE 2 POSITION 34 HIGHLIGHT.
           DISPLAY 'Numero de compte:' LINE 5 POSITION 15.
           ACCEPT CPT-NUM LINE 5 POSITION 35.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'COMPTE INEXISTANT!' LINE 10 POSITION 31
                       REVERSE-VIDEO
               NOT INVALID KEY
                   MOVE CPT-SOLDE TO WS-SOLDE-EDIT
                   DISPLAY 'Titulaire:' LINE 8 POSITION 15
                   DISPLAY CPT-NOM LINE 8 POSITION 30 HIGHLIGHT
                   DISPLAY 'Solde actuel:' LINE 9 POSITION 15
                   DISPLAY WS-SOLDE-EDIT LINE 9 POSITION 30
                   DISPLAY 'Montant a deposer:' LINE 12 POSITION 15
                   ACCEPT WS-MONTANT LINE 12 POSITION 35
                   ADD WS-MONTANT TO CPT-SOLDE
                   REWRITE COMPTE-REC
                   MOVE CPT-SOLDE TO WS-SOLDE-EDIT
                   DISPLAY 'DEPOT EFFECTUE' LINE 15 POSITION 33
                       HIGHLIGHT
                   DISPLAY 'Nouveau solde:' LINE 16 POSITION 23
                   DISPLAY WS-SOLDE-EDIT LINE 16 POSITION 38
                       REVERSE-VIDEO
                   DISPLAY ' EUR' LINE 16 POSITION 50
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32.
           ACCEPT WS-CONFIRM.

       RETIRER.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- RETRAIT ---' LINE 2 POSITION 33 HIGHLIGHT.
           DISPLAY 'Numero de compte:' LINE 5 POSITION 15.
           ACCEPT CPT-NUM LINE 5 POSITION 35.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'COMPTE INEXISTANT!' LINE 10 POSITION 31
                       REVERSE-VIDEO
               NOT INVALID KEY
                   MOVE CPT-SOLDE TO WS-SOLDE-EDIT
                   DISPLAY 'Titulaire:' LINE 8 POSITION 15
                   DISPLAY CPT-NOM LINE 8 POSITION 30 HIGHLIGHT
                   DISPLAY 'Solde actuel:' LINE 9 POSITION 15
                   DISPLAY WS-SOLDE-EDIT LINE 9 POSITION 30
                   DISPLAY 'Montant a retirer:' LINE 12 POSITION 15
                   ACCEPT WS-MONTANT LINE 12 POSITION 35
                   IF WS-MONTANT > CPT-SOLDE
                       DISPLAY '*** SOLDE INSUFFISANT ***' LINE 15
                           POSITION 28 REVERSE-VIDEO BLINK
                   ELSE
                       SUBTRACT WS-MONTANT FROM CPT-SOLDE
                       REWRITE COMPTE-REC
                       MOVE CPT-SOLDE TO WS-SOLDE-EDIT
                       DISPLAY 'RETRAIT EFFECTUE' LINE 15 POSITION 32
                           HIGHLIGHT
                       DISPLAY 'Nouveau solde:' LINE 16 POSITION 23
                       DISPLAY WS-SOLDE-EDIT LINE 16 POSITION 38
                           REVERSE-VIDEO
                       DISPLAY ' EUR' LINE 16 POSITION 50
                   END-IF
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32.
           ACCEPT WS-CONFIRM.

       VIREMENT.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- VIREMENT ---' LINE 2 POSITION 32 HIGHLIGHT.
           DISPLAY 'Compte source:' LINE 5 POSITION 15.
           ACCEPT CPT-NUM LINE 5 POSITION 35.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'COMPTE SOURCE INEXISTANT!' LINE 10
                       POSITION 28 REVERSE-VIDEO
                   DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32
                   ACCEPT WS-CONFIRM
                   GO TO VIREMENT-FIN
           END-READ.
           MOVE CPT-SOLDE TO WS-SOLDE-TEMP.
           MOVE CPT-SOLDE TO WS-SOLDE-EDIT.
           DISPLAY 'Titulaire:' LINE 7 POSITION 15.
           DISPLAY CPT-NOM LINE 7 POSITION 30 HIGHLIGHT.
           DISPLAY 'Solde disponible:' LINE 8 POSITION 15.
           DISPLAY WS-SOLDE-EDIT LINE 8 POSITION 35.
           DISPLAY 'Compte destinataire:' LINE 11 POSITION 15.
           ACCEPT WS-NUM-DEST LINE 11 POSITION 38.
           DISPLAY 'Montant:' LINE 13 POSITION 15.
           ACCEPT WS-MONTANT LINE 13 POSITION 35.
           IF WS-MONTANT > WS-SOLDE-TEMP
               DISPLAY '*** SOLDE INSUFFISANT ***' LINE 16
                   POSITION 28 REVERSE-VIDEO BLINK
               DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32
               ACCEPT WS-CONFIRM
               GO TO VIREMENT-FIN
           END-IF.
           SUBTRACT WS-MONTANT FROM CPT-SOLDE.
           REWRITE COMPTE-REC.
           MOVE WS-NUM-DEST TO CPT-NUM.
           READ COMPTES
               INVALID KEY
                   DISPLAY 'DESTINATAIRE INEXISTANT!' LINE 16
                       POSITION 28 REVERSE-VIDEO
               NOT INVALID KEY
                   ADD WS-MONTANT TO CPT-SOLDE
                   REWRITE COMPTE-REC
                   DISPLAY 'VIREMENT EFFECTUE' LINE 16 POSITION 32
                       HIGHLIGHT
           END-READ.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32.
           ACCEPT WS-CONFIRM.
       VIREMENT-FIN.
           EXIT.

       HISTORIQUE.
           DISPLAY ' ' LINE 1 POSITION 1 ERASE EOS.
           DISPLAY '--- HISTORIQUE ---' LINE 2 POSITION 31 HIGHLIGHT.
           DISPLAY '*** FONCTIONNALITE NON DISPONIBLE ***'
               LINE 10 POSITION 21 REVERSE-VIDEO.
           DISPLAY '(Necessite fichier mouvements)' LINE 12 POSITION 25.
           DISPLAY 'Appuyez ENTREE...' LINE 20 POSITION 32.
           ACCEPT WS-CONFIRM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAIE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYES ASSIGN TO 'EMPLOYES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EMP-MATRICULE.
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYES.
       01 EMPLOYE-REC.
           05 EMP-MATRICULE PIC 9(6).
           05 EMP-NOM PIC X(25).
           05 EMP-TAUX-H PIC 9(4)V99.
           05 EMP-SERVICE PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-HEURES PIC 9(3)V99 VALUE 0.
       01 WS-H-SUP PIC 9(2)V99 VALUE 0.
       01 WS-BRUT PIC 9(7)V99 VALUE 0.
       01 WS-BASE PIC 9(7)V99 VALUE 0.
       01 WS-SUPP PIC 9(6)V99 VALUE 0.
       01 WS-COTIS PIC 9(6)V99 VALUE 0.
       01 WS-NET PIC 9(7)V99 VALUE 0.
       01 WS-TAUX-COTIS PIC V99 VALUE 0.23.
       01 WS-TAUX-SUP PIC 9V99 VALUE 1.25.
       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT EMPLOYES.
           DISPLAY '================================'.
           DISPLAY '     CALCUL BULLETIN PAIE       '.
           DISPLAY '================================'.
           DISPLAY 'MATRICULE EMPLOYE:'.
           ACCEPT EMP-MATRICULE.
           READ EMPLOYES
               INVALID KEY
                   DISPLAY 'EMPLOYE NON TROUVE'
                   STOP RUN
           END-READ.
           DISPLAY ' '.
           DISPLAY 'EMPLOYE: ' EMP-NOM.
           DISPLAY 'SERVICE: ' EMP-SERVICE.
           DISPLAY 'TAUX HORAIRE: ' EMP-TAUX-H.
           DISPLAY ' '.
           DISPLAY 'HEURES TRAVAILLEES:'.
           ACCEPT WS-HEURES.
           IF WS-HEURES > 151.67
               COMPUTE WS-H-SUP = WS-HEURES - 151.67
               MOVE 151.67 TO WS-HEURES
           ELSE
               MOVE 0 TO WS-H-SUP
           END-IF.
           COMPUTE WS-BASE = WS-HEURES * EMP-TAUX-H.
           COMPUTE WS-SUPP = WS-H-SUP * EMP-TAUX-H
               * WS-TAUX-SUP.
           COMPUTE WS-BRUT = WS-BASE + WS-SUPP.
           COMPUTE WS-COTIS = WS-BRUT * WS-TAUX-COTIS.
           COMPUTE WS-NET = WS-BRUT - WS-COTIS.
           DISPLAY '================================'.
           DISPLAY '       BULLETIN DE PAIE         '.
           DISPLAY '================================'.
           DISPLAY 'SALAIRE BASE:    ' WS-BASE.
           DISPLAY 'HEURES SUP (' WS-H-SUP 'h): ' WS-SUPP.
           DISPLAY '--------------------------------'.
           DISPLAY 'SALAIRE BRUT:    ' WS-BRUT.
           DISPLAY 'COTISATIONS 23%: ' WS-COTIS.
           DISPLAY '--------------------------------'.
           DISPLAY 'NET A PAYER:     ' WS-NET.
           DISPLAY '================================'.
           CLOSE EMPLOYES.
           STOP RUN.
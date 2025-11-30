       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-IMPRIMANTE.
      *===============================================
      * DEMONSTRATION IMPRIMANTE DOT-MATRIX
      *===============================================
      * Ce programme genere un rapport de ventes
      * destine a etre imprime sur l'imprimante
      * dot-matrix retro de HOPPER COBOL.
      *
      * Apres execution, cliquez sur le bouton
      * imprimante (icone) pour voir le rendu
      * sur papier listing avec bandes vertes.
      *===============================================
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *--- En-tete du rapport ---
       01 WS-LIGNE-TIRET     PIC X(60) VALUE ALL "-".
       01 WS-LIGNE-EGAL      PIC X(60) VALUE ALL "=".
       01 WS-DATE-RAPPORT    PIC X(10) VALUE "25/12/2024".
       01 WS-PAGE            PIC 99 VALUE 1.

      *--- Table des produits ---
       01 WS-NB-PRODUITS     PIC 99 VALUE 8.
       01 WS-TABLE-PRODUITS.
          05 FILLER PIC X(33) VALUE "PRD001Clavier mecanique     00089".
          05 FILLER PIC X(33) VALUE "PRD002Souris sans fil       00045".
          05 FILLER PIC X(33) VALUE "PRD003Ecran 27 pouces       00299".
          05 FILLER PIC X(33) VALUE "PRD004Webcam HD             00079".
          05 FILLER PIC X(33) VALUE "PRD005Casque audio          00129".
          05 FILLER PIC X(33) VALUE "PRD006Tapis de souris       00025".
          05 FILLER PIC X(33) VALUE "PRD007Hub USB 3.0           00035".
          05 FILLER PIC X(33) VALUE "PRD008Support ecran         00049".
       01 WS-PRODUITS REDEFINES WS-TABLE-PRODUITS.
          05 WS-PRODUIT OCCURS 8.
             10 WS-PROD-CODE  PIC X(6).
             10 WS-PROD-NOM   PIC X(22).
             10 WS-PROD-PRIX  PIC 9(5).

      *--- Table des quantites vendues ---
       01 WS-TABLE-QTES.
          05 FILLER PIC 999 VALUE 150.
          05 FILLER PIC 999 VALUE 280.
          05 FILLER PIC 999 VALUE 45.
          05 FILLER PIC 999 VALUE 120.
          05 FILLER PIC 999 VALUE 85.
          05 FILLER PIC 999 VALUE 320.
          05 FILLER PIC 999 VALUE 95.
          05 FILLER PIC 999 VALUE 60.
       01 WS-QTES REDEFINES WS-TABLE-QTES.
          05 WS-QTE OCCURS 8 PIC 999.

      *--- Variables de calcul ---
       01 WS-INDEX           PIC 99 VALUE 0.
       01 WS-MONTANT-LIGNE   PIC 9(8)V99 VALUE 0.
       01 WS-TOTAL-GENERAL   PIC 9(9)V99 VALUE 0.
       01 WS-NB-ARTICLES     PIC 9(5) VALUE 0.

      *--- Lignes d'impression ---
       01 WS-LIGNE-DETAIL.
          05 FILLER          PIC X(3) VALUE "   ".
          05 WS-DET-CODE     PIC X(8).
          05 WS-DET-NOM      PIC X(24).
          05 WS-DET-PRIX     PIC ZZ,ZZ9.
          05 FILLER          PIC X(3) VALUE " x ".
          05 WS-DET-QTE      PIC ZZ9.
          05 FILLER          PIC X(3) VALUE " = ".
          05 WS-DET-TOTAL    PIC ZZZ,ZZ9.99.

       01 WS-LIGNE-TOTAUX.
          05 FILLER          PIC X(41) VALUE SPACES.
          05 FILLER          PIC X(10) VALUE "TOTAL:".
          05 WS-TOT-MONTANT  PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       DEBUT.
           PERFORM IMPRIMER-EN-TETE.
           PERFORM IMPRIMER-CORPS.
           PERFORM IMPRIMER-TOTAUX.
           PERFORM IMPRIMER-PIED.
           STOP RUN.

       IMPRIMER-EN-TETE.
           DISPLAY WS-LIGNE-EGAL.
           DISPLAY " ".
           DISPLAY "              RAPPORT DES VENTES MENSUELLES".
           DISPLAY "              ==============================".
           DISPLAY " ".
           DISPLAY "   Societe  : HOPPER INFORMATIQUE SARL".
           DISPLAY "   Date     : " WS-DATE-RAPPORT.
           DISPLAY "   Page     : " WS-PAGE.
           DISPLAY " ".
           DISPLAY WS-LIGNE-EGAL.
           DISPLAY " ".
           DISPLAY "   CODE     DESIGNATION              PRIX   QTE"
                   "      MONTANT".
           DISPLAY "   ------  ----------------------  ------  ---"
                   "  -----------".

       IMPRIMER-CORPS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-NB-PRODUITS
               MOVE WS-PROD-CODE(WS-INDEX) TO WS-DET-CODE
               MOVE WS-PROD-NOM(WS-INDEX) TO WS-DET-NOM
               MOVE WS-PROD-PRIX(WS-INDEX) TO WS-DET-PRIX
               MOVE WS-QTE(WS-INDEX) TO WS-DET-QTE
               COMPUTE WS-MONTANT-LIGNE =
                   WS-PROD-PRIX(WS-INDEX) * WS-QTE(WS-INDEX)
               MOVE WS-MONTANT-LIGNE TO WS-DET-TOTAL
               DISPLAY WS-LIGNE-DETAIL
               ADD WS-MONTANT-LIGNE TO WS-TOTAL-GENERAL
               ADD WS-QTE(WS-INDEX) TO WS-NB-ARTICLES
           END-PERFORM.

       IMPRIMER-TOTAUX.
           DISPLAY " ".
           DISPLAY WS-LIGNE-TIRET.
           MOVE WS-TOTAL-GENERAL TO WS-TOT-MONTANT.
           DISPLAY WS-LIGNE-TOTAUX.
           DISPLAY " ".
           DISPLAY "   Nombre total d'articles vendus : " WS-NB-ARTICLES.

       IMPRIMER-PIED.
           DISPLAY " ".
           DISPLAY WS-LIGNE-EGAL.
           DISPLAY " ".
           DISPLAY "   Ce rapport a ete genere par HOPPER COBOL".
           DISPLAY "   Imprimante: DOT-MATRIX SIMULATION".
           DISPLAY " ".
           DISPLAY "   *** CLIQUEZ SUR L'ICONE IMPRIMANTE ***".
           DISPLAY "   *** POUR VOIR LE RENDU PAPIER LISTING ***".
           DISPLAY " ".
           DISPLAY WS-LIGNE-EGAL.

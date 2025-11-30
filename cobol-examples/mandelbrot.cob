       IDENTIFICATION DIVISION.
       PROGRAM-ID. MANDELBROT.
      *===============================================
      * FRACTALE DE MANDELBROT EN ASCII
      * 40x20 caracteres, affichage ligne par ligne
      *===============================================
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *--- Parametres de la vue (initialises dans code) ---
       01 WS-X-MIN    PIC S9(2)V9(4) VALUE 0.
       01 WS-X-MAX    PIC S9(2)V9(4) VALUE 0.
       01 WS-Y-MIN    PIC S9(2)V9(4) VALUE 0.
       01 WS-Y-MAX    PIC S9(2)V9(4) VALUE 0.
      *--- Dimensions ---
       01 WS-WIDTH    PIC 99 VALUE 40.
       01 WS-HEIGHT   PIC 99 VALUE 20.
       01 WS-MAX-ITER PIC 99 VALUE 15.
      *--- Variables de calcul ---
       01 WS-PX       PIC 99 VALUE 0.
       01 WS-PY       PIC 99 VALUE 0.
       01 WS-X0       PIC S9(2)V9(4) VALUE 0.
       01 WS-Y0       PIC S9(2)V9(4) VALUE 0.
       01 WS-X        PIC S9(4)V9(4) VALUE 0.
       01 WS-Y        PIC S9(4)V9(4) VALUE 0.
       01 WS-X-NEW    PIC S9(4)V9(4) VALUE 0.
       01 WS-X-SQ     PIC S9(8)V9(4) VALUE 0.
       01 WS-Y-SQ     PIC S9(8)V9(4) VALUE 0.
       01 WS-XY       PIC S9(8)V9(4) VALUE 0.
       01 WS-MAG-SQ   PIC S9(8)V9(4) VALUE 0.
       01 WS-ITER     PIC 99 VALUE 0.
       01 WS-ESC-ITER PIC 99 VALUE 0.
       01 WS-ESCAPED  PIC 9 VALUE 0.
       01 WS-DX       PIC S9V9(6) VALUE 0.
       01 WS-DY       PIC S9V9(6) VALUE 0.
      *--- Caracteres de la ligne ---
       01 WS-CHAR     PIC X VALUE SPACE.
       01 WS-PAL-IDX  PIC 99 VALUE 0.
       01 WS-C01      PIC X VALUE SPACE.
       01 WS-C02      PIC X VALUE SPACE.
       01 WS-C03      PIC X VALUE SPACE.
       01 WS-C04      PIC X VALUE SPACE.
       01 WS-C05      PIC X VALUE SPACE.
       01 WS-C06      PIC X VALUE SPACE.
       01 WS-C07      PIC X VALUE SPACE.
       01 WS-C08      PIC X VALUE SPACE.
       01 WS-C09      PIC X VALUE SPACE.
       01 WS-C10      PIC X VALUE SPACE.
       01 WS-C11      PIC X VALUE SPACE.
       01 WS-C12      PIC X VALUE SPACE.
       01 WS-C13      PIC X VALUE SPACE.
       01 WS-C14      PIC X VALUE SPACE.
       01 WS-C15      PIC X VALUE SPACE.
       01 WS-C16      PIC X VALUE SPACE.
       01 WS-C17      PIC X VALUE SPACE.
       01 WS-C18      PIC X VALUE SPACE.
       01 WS-C19      PIC X VALUE SPACE.
       01 WS-C20      PIC X VALUE SPACE.
       01 WS-C21      PIC X VALUE SPACE.
       01 WS-C22      PIC X VALUE SPACE.
       01 WS-C23      PIC X VALUE SPACE.
       01 WS-C24      PIC X VALUE SPACE.
       01 WS-C25      PIC X VALUE SPACE.
       01 WS-C26      PIC X VALUE SPACE.
       01 WS-C27      PIC X VALUE SPACE.
       01 WS-C28      PIC X VALUE SPACE.
       01 WS-C29      PIC X VALUE SPACE.
       01 WS-C30      PIC X VALUE SPACE.
       01 WS-C31      PIC X VALUE SPACE.
       01 WS-C32      PIC X VALUE SPACE.
       01 WS-C33      PIC X VALUE SPACE.
       01 WS-C34      PIC X VALUE SPACE.
       01 WS-C35      PIC X VALUE SPACE.
       01 WS-C36      PIC X VALUE SPACE.
       01 WS-C37      PIC X VALUE SPACE.
       01 WS-C38      PIC X VALUE SPACE.
       01 WS-C39      PIC X VALUE SPACE.
       01 WS-C40      PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       DEBUT.
      *--- Initialiser les bornes (contournement bug negatifs) ---
           COMPUTE WS-X-MIN = 0 - 2.
           COMPUTE WS-X-MAX = 1.
           COMPUTE WS-Y-MIN = 0 - 1.2.
           COMPUTE WS-Y-MAX = 1.2.
      *--- Calculer les pas ---
           COMPUTE WS-DX = (WS-X-MAX - WS-X-MIN) / WS-WIDTH.
           COMPUTE WS-DY = (WS-Y-MAX - WS-Y-MIN) / WS-HEIGHT.
      *--- Affichage ---
           DISPLAY '======= MANDELBROT FRACTAL ======='.
           DISPLAY ' '.
           MOVE 0 TO WS-PY.
           PERFORM CALCUL-LIGNE UNTIL WS-PY >= WS-HEIGHT.
           DISPLAY ' '.
           DISPLAY '=================================='.
           STOP RUN.
       CALCUL-LIGNE.
           COMPUTE WS-Y0 = WS-Y-MAX - (WS-PY * WS-DY).
           PERFORM CALC-COL-01.
           PERFORM CALC-COL-02.
           PERFORM CALC-COL-03.
           PERFORM CALC-COL-04.
           PERFORM CALC-COL-05.
           PERFORM CALC-COL-06.
           PERFORM CALC-COL-07.
           PERFORM CALC-COL-08.
           PERFORM CALC-COL-09.
           PERFORM CALC-COL-10.
           PERFORM CALC-COL-11.
           PERFORM CALC-COL-12.
           PERFORM CALC-COL-13.
           PERFORM CALC-COL-14.
           PERFORM CALC-COL-15.
           PERFORM CALC-COL-16.
           PERFORM CALC-COL-17.
           PERFORM CALC-COL-18.
           PERFORM CALC-COL-19.
           PERFORM CALC-COL-20.
           PERFORM CALC-COL-21.
           PERFORM CALC-COL-22.
           PERFORM CALC-COL-23.
           PERFORM CALC-COL-24.
           PERFORM CALC-COL-25.
           PERFORM CALC-COL-26.
           PERFORM CALC-COL-27.
           PERFORM CALC-COL-28.
           PERFORM CALC-COL-29.
           PERFORM CALC-COL-30.
           PERFORM CALC-COL-31.
           PERFORM CALC-COL-32.
           PERFORM CALC-COL-33.
           PERFORM CALC-COL-34.
           PERFORM CALC-COL-35.
           PERFORM CALC-COL-36.
           PERFORM CALC-COL-37.
           PERFORM CALC-COL-38.
           PERFORM CALC-COL-39.
           PERFORM CALC-COL-40.
           DISPLAY WS-C01 WS-C02 WS-C03 WS-C04 WS-C05
                   WS-C06 WS-C07 WS-C08 WS-C09 WS-C10
                   WS-C11 WS-C12 WS-C13 WS-C14 WS-C15
                   WS-C16 WS-C17 WS-C18 WS-C19 WS-C20
                   WS-C21 WS-C22 WS-C23 WS-C24 WS-C25
                   WS-C26 WS-C27 WS-C28 WS-C29 WS-C30
                   WS-C31 WS-C32 WS-C33 WS-C34 WS-C35
                   WS-C36 WS-C37 WS-C38 WS-C39 WS-C40.
           ADD 1 TO WS-PY.
       CALC-COL-01.
           MOVE 0 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C01.
       CALC-COL-02.
           MOVE 1 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C02.
       CALC-COL-03.
           MOVE 2 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C03.
       CALC-COL-04.
           MOVE 3 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C04.
       CALC-COL-05.
           MOVE 4 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C05.
       CALC-COL-06.
           MOVE 5 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C06.
       CALC-COL-07.
           MOVE 6 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C07.
       CALC-COL-08.
           MOVE 7 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C08.
       CALC-COL-09.
           MOVE 8 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C09.
       CALC-COL-10.
           MOVE 9 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C10.
       CALC-COL-11.
           MOVE 10 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C11.
       CALC-COL-12.
           MOVE 11 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C12.
       CALC-COL-13.
           MOVE 12 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C13.
       CALC-COL-14.
           MOVE 13 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C14.
       CALC-COL-15.
           MOVE 14 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C15.
       CALC-COL-16.
           MOVE 15 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C16.
       CALC-COL-17.
           MOVE 16 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C17.
       CALC-COL-18.
           MOVE 17 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C18.
       CALC-COL-19.
           MOVE 18 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C19.
       CALC-COL-20.
           MOVE 19 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C20.
       CALC-COL-21.
           MOVE 20 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C21.
       CALC-COL-22.
           MOVE 21 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C22.
       CALC-COL-23.
           MOVE 22 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C23.
       CALC-COL-24.
           MOVE 23 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C24.
       CALC-COL-25.
           MOVE 24 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C25.
       CALC-COL-26.
           MOVE 25 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C26.
       CALC-COL-27.
           MOVE 26 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C27.
       CALC-COL-28.
           MOVE 27 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C28.
       CALC-COL-29.
           MOVE 28 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C29.
       CALC-COL-30.
           MOVE 29 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C30.
       CALC-COL-31.
           MOVE 30 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C31.
       CALC-COL-32.
           MOVE 31 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C32.
       CALC-COL-33.
           MOVE 32 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C33.
       CALC-COL-34.
           MOVE 33 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C34.
       CALC-COL-35.
           MOVE 34 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C35.
       CALC-COL-36.
           MOVE 35 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C36.
       CALC-COL-37.
           MOVE 36 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C37.
       CALC-COL-38.
           MOVE 37 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C38.
       CALC-COL-39.
           MOVE 38 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C39.
       CALC-COL-40.
           MOVE 39 TO WS-PX.
           PERFORM CALC-PIXEL.
           MOVE WS-CHAR TO WS-C40.
       CALC-PIXEL.
           COMPUTE WS-X0 = WS-X-MIN + (WS-PX * WS-DX).
           MOVE 0 TO WS-X.
           MOVE 0 TO WS-Y.
           MOVE 0 TO WS-ITER.
           MOVE 0 TO WS-ESCAPED.
           PERFORM MANDEL-ITER UNTIL WS-ITER >= WS-MAX-ITER.
           IF WS-ESCAPED = 1
               COMPUTE WS-PAL-IDX = WS-ESC-ITER * 6 / WS-MAX-ITER
               EVALUATE WS-PAL-IDX
                   WHEN 0 MOVE '.' TO WS-CHAR
                   WHEN 1 MOVE ':' TO WS-CHAR
                   WHEN 2 MOVE '+' TO WS-CHAR
                   WHEN 3 MOVE '*' TO WS-CHAR
                   WHEN 4 MOVE '#' TO WS-CHAR
                   WHEN 5 MOVE '@' TO WS-CHAR
                   WHEN OTHER MOVE 'M' TO WS-CHAR
               END-EVALUATE
           ELSE
               MOVE ' ' TO WS-CHAR
           END-IF.
       MANDEL-ITER.
           COMPUTE WS-X-SQ = WS-X * WS-X.
           COMPUTE WS-Y-SQ = WS-Y * WS-Y.
           COMPUTE WS-MAG-SQ = WS-X-SQ + WS-Y-SQ.
           IF WS-MAG-SQ > 4
               MOVE 1 TO WS-ESCAPED
               MOVE WS-ITER TO WS-ESC-ITER
               MOVE WS-MAX-ITER TO WS-ITER
           ELSE
               COMPUTE WS-XY = WS-X * WS-Y
               COMPUTE WS-X-NEW = WS-X-SQ - WS-Y-SQ + WS-X0
               COMPUTE WS-Y = 2 * WS-XY + WS-Y0
               MOVE WS-X-NEW TO WS-X
               ADD 1 TO WS-ITER
           END-IF.

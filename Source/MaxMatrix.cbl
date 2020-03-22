       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRIX.
       AUTHOR. Mattia Righetti (mattiarighe@me.com).
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MATRIX.
           02 ROW OCCURS 5 TIMES.
             03 ELEMENT PIC 99 OCCURS 4 TIMES.
       01 MAXROW PIC 999.
       01 IMAXROW PIC 9.
       01 I PIC 9.
       01 MAXCOLUMN PIC 999.
       01 IMAXCOLUMN PIC 9.
       01 J PIC 9.
       01 SUM PIC 999.          
       SCREEN SECTION.
       01 CLEANER.
           02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN.
      *GIVEN A MATRIX COMPOSED BY 4 ROWS AND 4 COLUMNS,
      *THIS PROGRAM FINDS THE ROW AND THE COLUMN WITH THE HIGHER VALUE.
           MOVE 1 TO J
           PERFORM UNTIL J > 5
             MOVE 1 TO I
             PERFORM UNTIL I > 4
               DISPLAY "INSERT ELEMENT " I " OF ROW " J ":"
               ACCEPT ELEMENT(J,I)
               ADD 1 TO I
             END-PERFORM
             ADD 1 TO J
           END-PERFORM
           MOVE 1 TO J
           MOVE 0 TO MAXROW
           PERFORM UNTIL J > 5
             MOVE 0 TO SUM
             MOVE 1 TO I
             PERFORM UNTIL I > 4
               COMPUTE SUM = SUM + ELEMENT(J,I)
               ADD 1 TO I
             END-PERFORM
             IF SUM > MAXROW
               MOVE SUM TO MAXROW
               MOVE J TO IMAXROW
             END-IF
             ADD 1 TO J
           END-PERFORM
           DISPLAY CLEANER
           DISPLAY "MAX ROW IS " IMAXROW "WITH VALUE " MAXROW
           MOVE 1 TO J
           MOVE 0 TO MAXCOLUMN
           PERFORM UNTIL J > 4
             MOVE 0 TO SUM
             MOVE 1 TO I
             PERFORM UNTIL I > 5
               COMPUTE SUM = SUM + ELEMENT(I,J)
               ADD 1 TO I
             END-PERFORM
             IF SUM > MAXCOLUMN
               MOVE SUM TO MAXCOLUMN
               MOVE I TO IMAXCOLUMN
             END-IF
             ADD 1 TO J
           END-PERFORM
           DISPLAY "MAX COLUMN IS " IMAXCOLUMN " WITH VALUE " MAXCOLUMN
           MOVE 1 TO J
           PERFORM UNTIL J > 5
             MOVE 1 TO I
             PERFORM UNTIL I > 4
               DISPLAY ELEMENT(J,I)
               AT LINE (J + 5) COL (I * 4)
               ADD 1 TO I
             END-PERFORM
             ADD 1 TO J
           END-PERFORM
           ACCEPT OMITTED
           STOP RUN.


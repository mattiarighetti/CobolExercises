       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELEMENTS-COUNTER.
       AUTHOR. Mattia Righetti (mattiarighe@me.com).
       DATE-WRITTEN. DECEMBER 2011.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           SELECT PRINT ASSIGN TO "ELEMENTS-TABLE.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PRINT
           LABEL RECORD IS OMITTED.
       01 PRINT-REC.
           02 ROW-PRINT PIC X(80).
       WORKING-STORAGE SECTION.
       01 ARRAY.
           02 ELEMENT PIC X OCCURS 100 TIMES.
       01 COUNTER.
           02 CHAR PIC X OCCURS 100 TIMES.
           02 CHAR-TIMES PIC 9 OCCURS 100 TIMES.
       01 I PIC 9(3).
       01 J PIC 9(3).
       01 FLAG PIC X(2).
       01 ROW.
           02 ROW-ELEMENT PIC X.
           02 FILLER PIC X(5) VALUE SPACES.
           02 TEMPO PIC 9(3).
       PROCEDURE DIVISION.
      *THIS PROGRAM COUNTS ELEMENTS.
       MAIN.
           DISPLAY "ELEMENT COUNTER (PRESS ANY KEY TO CONTINUE...)"
           DISPLAY OMITTED
           PERFORM DATA-ENTRY
           PERFORM COUNTING
           PERFORM PRINT-RESULT
           ACCEPT OMITTED
           STOP RUN.
 
       DATA-ENTRY.
           DISPLAY "TYPE THE ELEMENTS TO COUNT..."
           ACCEPT ARRAY.

       COUNTING.
           MOVE 1 TO I
           PERFORM UNTIL I > 100
              PERFORM CONTROLLER
              MOVE ELEMENT(I) TO CHAR(I)
              MOVE 1 TO J
              PERFORM UNTIL J > 100                   
                 IF CHAR(I) = CHAR(J)
                    ADD 1 TO CHAR-TIMES(I)
                 END-IF
              END-PERFORM
              ADD 1 TO I
           END-PERFORM.   

       CONTROLLER.                 
           MOVE 1 TO J
           MOVE "KO" TO FLAG
           PERFORM UNTIL J = I
              IF ELEMENT(J) = ELEMENT(I)
                 MOVE "OK" TO FLAG
              END-IF
              ADD 1 TO J
           END-PERFORM
           IF FLAG = "OK" 
              ADD 1 TO I
           END-IF.

       PRINT-RESULT.
           OPEN OUTPUT PRINT
           MOVE 1 TO I
           PERFORM UNTIL CHAR(I) = SPACE
              MOVE CHAR(I) TO ROW-ELEMENT
              MOVE CHAR-TIMES(I) TO TEMPO
              MOVE ROW TO ROW-PRINT
              WRITE PRINT-REC
              ADD 1 TO I
           END-PERFORM
           CLOSE PRINT.


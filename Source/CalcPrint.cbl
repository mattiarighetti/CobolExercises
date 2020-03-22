       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC-STAMP.
       AUTHOR. Mattia Righetti (mattiarighe@me.com).
       DATE-WRITTEN. February 2012.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CALCULATIONS ASSIGN TO "CALCULATIONS.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD CALCULATIONS
           LABEL RECORD IS OMITTED.
       01 PRINT-RECORD.      
           02 PRINT-ROW PIC X(80).
       WORKING-STORAGE SECTION.
       01 N PIC 99.
       01 FIRST-NUMBER PIC 9(5)V99.
       01 OPERATION PIC X.
       01 SECOND-NUMBER PIC 9(5)V99.
       01 RESULT PIC 9(7)V99.
       01 TITLE-TEXT-ROW.
           02 FILLER PIC X(33) VALUE SPACES.
           02 TITLE-TEXT PIC X(15).
           02 FILLER PIC X(32) VALUE SPACES.
       01 CALCULATIONS-ROW.
           02 NUM1 PIC Z(5),99.
           02 SYM PIC X.
           02 NUM2 PIC Z(5),99.
           02 FILLER PIC X VALUE "=".
           02 CALC-RESULT PIC Z(7),99.
       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT CALCULATIONS
           PERFORM BEGIN
           PERFORM PRINT-TITLE
           PERFORM CALCULATOR
           PERFORM PRINTER
           ACCEPT OMITTED
           STOP RUN.
       
       BEGIN.
           DISPLAY "CALCULATOR (30 OPERATIONS MAX)"
           DISPLAY "HOW MANY OPERATIONS YOU WISH TO EXECUTE?"
           ACCEPT N CONVERT.
           
       PRINT-TITLE.
           MOVE "CALCULATIONS" TO TITLE-TEXT
           MOVE TITLE-TEXT-ROW TO PRINT-ROW
           WRITE PRINT-RECORD.

       CALCULATOR.
           PERFORM N TIMES
              DISPLAY "TYPE THE FIRST NUMBER."
              ACCEPT FIRST-NUMBER CONVERT
              DISPLAY "TYPE THE OPERATION THAT HAS TO BE EXECUTED."
              ACCEPT OPERATION
              IF OPERATION <> "+" OR "-" OR "*" OR "/"
              PERFORM UNTIL OPERATION = "+" OR OPERATION = "-" 
              OR OPERATION = "*" OR OPERATION = "/"
                    DISPLAY "OPERATION NOT RECOGNISED."
                    DISPLAY "TRY AGAIN..."
                    ACCEPT OPERATION
                 END-PERFORM
              END-IF
              DISPLAY "TYPE THE SECOND NUMBER."
              ACCEPT SECOND-NUMBER CONVERT
              IF OPERATION = "+"
                 COMPUTE RESULT = FIRST-NUMBER + SECOND-NUMBER
                 DISPLAY "THE RESULT OF THE OPERATION IS:" RESULT 
                                                          CONVERT
              END-IF
              IF OPERATION = "-"
                 COMPUTE RESULT = FIRST-NUMBER - SECOND-NUMBER
                 DISPLAY "THE RESULT OF THE OPERATION IS:" RESULT 
              END-IF                                 
              IF OPERATION = "*"
                 COMPUTE RESULT = FIRST-NUMBER * SECOND-NUMBER
                 DISPLAY "THE RESULT OF THE OPERATION IS:" RESULT 
              END-IF                                      
              IF OPERATION = "/"
                 COMPUTE RESULT = FIRST-NUMBER / SECOND-NUMBER
                 DISPLAY "THE RESULT OF THE OPERATION IS:" RESULT 
              END-IF                                      
           END-PERFORM.
           
       PRINTER.
           MOVE FIRST-NUMBER TO NUM1
           MOVE SECOND-NUMBER TO NUM2
           MOVE OPERATION TO SYM
           MOVE RESULT TO CALC-RESULT
           MOVE CALCULATIONS-ROW TO PRINT-ROW
           WRITE PRINT-RECORD.


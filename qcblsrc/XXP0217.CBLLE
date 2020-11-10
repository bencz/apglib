       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0217.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  27.09.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0217".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  RESULT                         PIC S9(9)V9(3).
       01  VALUE-1                   LIKE RESULT.
       01  VALUE-2                   LIKE RESULT.
       01  VALUE-3                   LIKE RESULT.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0217"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

      *
           PERFORM  START-DEBUGGING-CHECK.
           IF       VALUE-2 NOT = 42
                    GO TO ENDE
           ELSE
     ‚*             ERGEBNIS MUSS XXXXX SEIN, NUR DANN WURDE DIESER
     ‚*             PUNKT ORDENTLICH ABGEARBEITET
                    COMPUTE RESULT = VALUE-1 * VALUE-3 * 0,3
                    END-COMPUTE
           END-IF.



       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTE DEBUG-AUFGABE:˜VALUE-1 SETZEN
     ‚*--------------------------------------------------------------
       START-DEBUGGING-CHECK SECTION.
       STE-P01-00.

           EVALUATE VALUE-1
               WHEN 23.80 MOVE 19   TO VALUE-3
               WHEN 17.12 MOVE 91   TO VALUE-3
               WHEN 58.05 MOVE 38   TO VALUE-3
               WHEN 38.52 MOVE 23   TO VALUE-3
               WHEN 19.12 MOVE 42   TO VALUE-3
               WHEN 28.64 MOVE 25   TO VALUE-3
               WHEN 78.64 MOVE 15   TO VALUE-3
               WHEN 19.12 MOVE 192  TO VALUE-3
               WHEN 13.37 MOVE 39   TO VALUE-3
               WHEN 38.13 MOVE 47   TO VALUE-3
               WHEN 17.01 MOVE 28   TO VALUE-3
               WHEN  2.37 MOVE 69   TO VALUE-3
               WHEN 42.67 MOVE 28   TO VALUE-3
               WHEN 35.13 MOVE 100  TO VALUE-3
               WHEN OTHER GO TO STE-P01-90
           END-EVALUATE.

           PERFORM  VARYING VALUE-2 FROM 33 BY 1 UNTIL VALUE-3 = 14
                    SUBTRACT 1 FROM VALUE-3
           END-PERFORM.

       STE-P01-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

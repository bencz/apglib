       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM FÜR MOVE                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM FÜR NUMERISCHEN MOVE                *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9005".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  POS                           PIC 9(4).

     ‚* MAX-MOVE
       01  NUM-ALPHA                     PIC X(40).
       01  NUM1                          REDEFINES NUM-ALPHA PIC 9(40).
       01  NUM2                          PIC 9(40).

     ‚* ALPHA-NUM-MOVE
       01  ALPHA-WRK                     PIC X(10).
       01  NUM-WRK                       PIC 9(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-AKTION                      PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-AKTION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           EVALUATE X-AKTION
                    WHEN "MAX-MOVE"  PERFORM MAX-MOVE
                    WHEN "ALPHA-NUM" PERFORM ALPHA-NUM-MOVE
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* WIEVIEL STELLEN WERDEN NUMERISCH GEMOVED?
     ‚*--------------------------------------------------------------
       MAX-MOVE SECTION.
       MAX-MOV-00.

     ‚* INITIALISIEREN
           MOVE     ZEROES TO NUM1, NUM2.
           MOVE     "1111010000100011111110011000100101100111" TO
                    NUM-ALPHA(1:40).
           MOVE     NUM1 TO NUM2.

     ‚* ES WERDEN HIER NUR DIE ERSTEN 18(!!!) STELLEN GEMOVED
           MOVE     1111010000100011111110011000100101100111 TO
                    NUM1.

           MOVE     ZEROES TO NUM1, NUM2, POS.
           MOVE     "1111010000100011111110011000100101100111" TO
                    NUM-ALPHA(1:40).
           PERFORM  LENGTH OF NUM1 TIMES
                    ADD 1 TO POS
                    MOVE NUM1(POS:1) TO NUM2(POS:1)
           END-PERFORM.

       MAX-MOV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WIE WIRD EIN NUMERISCHER WERT ÜBERTRAGEN?
     ‚*--------------------------------------------------------------
       ALPHA-NUM-MOVE SECTION.
       ALN-MOV-00.

     ‚* INITIALISIEREN
           MOVE     ZEROES TO POS.
           MOVE     ZEROES TO NUM-WRK.
           MOVE     "13" TO ALPHA-WRK.

     ‚* DURCH SCHLEIFE PRÜFEN WIE LANG DAS IST
           IF       ALPHA-WRK NOT = SPACES
                    PERFORM WITH TEST BEFORE VARYING POS
                            FROM 1 BY 1
                            UNTIL POS = LENGTH OF ALPHA-WRK OR
                            ALPHA-WRK(POS:1) NOT NUMERIC
                            CONTINUE
                    END-PERFORM
                    SUBTRACT 1 FROM POS
                    IF   POS > ZEROES
                         MOVE ALPHA-WRK(1:POS) TO NUM-WRK
                    END-IF
           END-IF.

       ALN-MOV-90.
           EXIT.
      /

       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DATUMSUMSETZUNG                                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  UMSETZEN EINES DATUMS                        *
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
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9005".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-DATE-ALPHA-10               LIKE DATUM-ALPHA-10.
       01  X-DATE-ALPHA-8                LIKE DATUM-ALPHA-8.
       01  X-DATE-NUM-8                  LIKE DATE-DB.
       01  X-DATE-NUM-6                  LIKE DATE-DB-6.
       01  X-CONVERT                     LIKE DATUM-CONVERT.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-DATE-ALPHA-10
                                X-DATE-ALPHA-8
                                X-DATE-NUM-8
                                X-DATE-NUM-6
                                X-CONVERT.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9005" TO PGM-WRK.

     ‚* UMSETZEN DES DATUMS
           EVALUATE X-CONVERT
     ‚*             INS DATENBANKFORMAT FORMATIEREN
                    WHEN "TO-DB"
                         INITIALIZE X-DATE-NUM-8
                         INITIALIZE X-DATE-NUM-6
                         PERFORM  SETZE-NUMERIC
     ‚*             INS DISPLAY-FORMAT FORMATIEREN
                    WHEN "TO-DSP"
                         INITIALIZE X-DATE-ALPHA-10
                         INITIALIZE X-DATE-ALPHA-8
                         PERFORM  SETZE-ALPHA
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SETZEN DES DATUMS AUF NUMERISCHE WERTE
     ‚*--------------------------------------------------------------
       SETZE-NUMERIC SECTION.
       SET-NUM-00.

           INITIALIZE RET-CODE.
     ‚* IST DAS FELD GEFÜLLT?
           IF    X-DATE-ALPHA-10 NOT = SPACES
     ‚*          IST DER STRING SCHON NUMERISCH GEFÜLLT?
                 IF   X-DATE-ALPHA-10(1:8) IS NUMERIC
                      MOVE X-DATE-ALPHA-10(1:8) TO X-DATE-NUM-8
                      GO TO SET-NUM-90
                 END-IF
     ‚*          IST DER STRING DD.MM.YYYY GEFÜLLT?
                 IF   X-DATE-ALPHA-10(1:2) IS NUMERIC
                  AND X-DATE-ALPHA-10(4:2) IS NUMERIC
                  AND X-DATE-ALPHA-10(7:4) IS NUMERIC
                  AND X-DATE-ALPHA-10(3:1) NOT = SPACES
                  AND X-DATE-ALPHA-10(6:1) NOT = SPACES
                      MOVE X-DATE-ALPHA-10(7:4) TO X-DATE-NUM-8(1:4)
                      MOVE X-DATE-ALPHA-10(4:2) TO X-DATE-NUM-8(5:2)
                      MOVE X-DATE-ALPHA-10(1:2) TO X-DATE-NUM-8(7:2)
                      GO TO SET-NUM-90
                 END-IF
           END-IF.

       SET-NUM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* UMSETZEN DATUM AUF ALPHA-FELD (AUFBEREITET)
     ‚*--------------------------------------------------------------
       SETZE-ALPHA SECTION.
       SET-ALP-00.

     ‚* WERT ÜBERNEHMEN
           IF       X-DATE-NUM-6 NOT = ZEROES
            AND     X-DATE-NUM-8     = ZEROES
                    MOVE X-DATE-NUM-6 TO X-DATE-NUM-8(3:6)
                    IF   X-DATE-NUM-6(1:2) > 90
                         MOVE 19      TO X-DATE-NUM-8(1:2)
                    ELSE
                         MOVE 20      TO X-DATE-NUM-8(1:2)
                    END-IF
           END-IF.

     ‚* 10-STELLIG ALPHA
           MOVE     X-DATE-NUM-8(7:2) TO X-DATE-ALPHA-10(1:2).
           MOVE     "."               TO X-DATE-ALPHA-10(3:1).
           MOVE     X-DATE-NUM-8(5:2) TO X-DATE-ALPHA-10(4:2).
           MOVE     "."               TO X-DATE-ALPHA-10(6:1).
           MOVE     X-DATE-NUM-8(1:4) TO X-DATE-ALPHA-10(7:4).

     ‚* 8-STELLIG ALPHA
           MOVE     X-DATE-NUM-8(7:2) TO X-DATE-ALPHA-8(1:2).
           MOVE     "."               TO X-DATE-ALPHA-8(3:1).
           MOVE     X-DATE-NUM-8(5:2) TO X-DATE-ALPHA-8(4:2).
           MOVE     "."               TO X-DATE-ALPHA-8(6:1).
           MOVE     X-DATE-NUM-8(3:4) TO X-DATE-ALPHA-8(7:2).

       SET-ALP-90.
           EXIT.
      /

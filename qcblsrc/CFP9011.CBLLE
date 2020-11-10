       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9011.
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

       01  PGM-WRK                       PIC X(10) VALUE "CFP9011".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TIME-ALPHA-8                LIKE TIME-ALPHA-8.
       01  X-TIME-NUM-6                  LIKE TIME-DB.
       01  X-CONVERT                     LIKE DATUM-CONVERT.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TIME-ALPHA-8
                                X-TIME-NUM-6
                                X-CONVERT.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9011" TO PGM-WRK.

     ‚* UMSETZEN DES DATUMS
           EVALUATE X-CONVERT
     ‚*             INS DATENBANKFORMAT FORMATIEREN
                    WHEN "TO-DB"
                         INITIALIZE X-TIME-NUM-6
                         PERFORM  SETZE-NUMERIC
     ‚*             INS DISPLAY-FORMAT FORMATIEREN
                    WHEN "TO-DSP"
                         INITIALIZE X-TIME-ALPHA-8
                         PERFORM  SETZE-ALPHA
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SETZEN DER ZEIT AUF NUMERISCHE WERTE
     ‚*--------------------------------------------------------------
       SETZE-NUMERIC SECTION.
       SET-NUM-00.

           INITIALIZE RET-CODE.
     ‚* IST DAS FELD GEFÜLLT?
     ‚*€   IF    X-DATE-ALPHA-10 NOT = SPACES
     ‚*          IST DER STRING SCHON NUMERISCH GEFÜLLT?
     ‚*€         IF   X-DATE-ALPHA-10(1:8) IS NUMERIC
     ‚*€              MOVE X-DATE-ALPHA-10(1:8) TO X-DATE-NUM-8
     ‚*€              GO TO SET-NUM-90
     ‚*€         END-IF
     ‚*          IST DER STRING DD.MM.YYYY GEFÜLLT?
     ‚*€         IF   X-DATE-ALPHA-10(1:2) IS NUMERIC
     ‚*€          AND X-DATE-ALPHA-10(4:2) IS NUMERIC
     ‚*€          AND X-DATE-ALPHA-10(7:4) IS NUMERIC
     ‚*€          AND X-DATE-ALPHA-10(3:1) NOT = SPACES
     ‚*€          AND X-DATE-ALPHA-10(6:1) NOT = SPACES
     ‚*€              MOVE X-DATE-ALPHA-10(7:4) TO X-DATE-NUM-8(1:4)
     ‚*€              MOVE X-DATE-ALPHA-10(4:2) TO X-DATE-NUM-8(5:2)
     ‚*€              MOVE X-DATE-ALPHA-10(1:2) TO X-DATE-NUM-8(7:2)
     ‚*€              GO TO SET-NUM-90
     ‚*€         END-IF
     ‚*€   END-IF.

       SET-NUM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* UMSETZEN ZEIT AUF ALPHA-FELD (AUFBEREITET)
     ‚*--------------------------------------------------------------
       SETZE-ALPHA SECTION.
       SET-ALP-00.

           MOVE     X-TIME-NUM-6(1:2) TO X-TIME-ALPHA-8(1:2).
           MOVE     ":"               TO X-TIME-ALPHA-8(3:1).
           MOVE     X-TIME-NUM-6(3:2) TO X-TIME-ALPHA-8(4:2).
           MOVE     ":"               TO X-TIME-ALPHA-8(6:1).
           MOVE     X-TIME-NUM-6(5:2) TO X-TIME-ALPHA-8(7:2).

       SET-ALP-90.
           EXIT.
      /

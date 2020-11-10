       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN KW ZU EINEM TAG                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  25.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN EINER KW                           *
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

       01  PGM-WRK                       PIC X(10) VALUE "CFP8007".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  DATUM-WRK                     LIKE DATE-8.
       01  DATUM-RED REDEFINES DATUM-WRK.
        05 JAHR                          PIC 9(4).
        05 MONAT                         PIC 9(2).
        05 TAG                           PIC 9(2).

       01  KW-WRK                        PIC X(2).
       01  INT-JAHR                      PIC S9(9) BINARY.
       01  INT-MONAT                     PIC S9(9) BINARY.
       01  INT-TAG                       PIC S9(9) BINARY.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-DATUM                       LIKE DATE-8.
       01  X-KW                          PIC 9(2).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-DATUM
                                X-KW.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP8007" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-DATUM = ZEROES
                    GO TO ENDE
           END-IF.

           INITIALIZE KW-WRK, X-KW.
           MOVE     X-DATUM TO DATUM-WRK
           MOVE     JAHR  OF DATUM-RED TO INT-JAHR.
           MOVE     MONAT OF DATUM-RED TO INT-MONAT.
           MOVE     TAG   OF DATUM-RED TO INT-TAG.
     ‚* DATUM IN DIE STRUKTUR FÜLLEN
           CALL     PROCEDURE "CCP0001" USING INT-JAHR
                                              INT-MONAT
                                              INT-TAG
                                              KW-WRK
           END-CALL.
     ‚* KALENDERWOCHE ÜBERNEHMEN
           IF       KW-WRK IS NUMERIC
                    MOVE KW-WRK TO X-KW
                    IF   KW-WRK = "00"
                         MOVE 53 TO X-KW
                    END-IF
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

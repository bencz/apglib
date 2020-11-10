     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             AZP0102.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* RÜCKGABE FARBCODE JE AZUBI-BERUF                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  RÜCKGABE FARBCODE                            *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* CONFIG
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0102".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
       01  X-COLOR                       PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-COLOR.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0102" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT-SIMPLE.

           INITIALIZE X-COLOR.
     ‚* CONFIG LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "V100"            TO CFID   OF CFG-CPY.
           MOVE     X-USER            TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY NOT = ZEROES
                    EVALUATE CFIN03 OF CFG-CPY
                             WHEN "FI-AE" MOVE COL-BLU TO X-COLOR
                             WHEN "FI-SI" MOVE COL-TRQ TO X-COLOR
                             WHEN "IT-SK" MOVE COL-YLW TO X-COLOR
                             WHEN "KFI"   MOVE COL-RED TO X-COLOR
                             WHEN "KFB"   MOVE COL-PNK TO X-COLOR
                             WHEN OTHER   MOVE COL-GRN TO X-COLOR
                    END-EVALUATE
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT-SIMPLE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN KONFIGURATIONSDATEI
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

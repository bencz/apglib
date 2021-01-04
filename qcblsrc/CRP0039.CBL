       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0036.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BEFEHLSPOINTER                                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SEU JE NACH EINSTELLUNG STARTEN              *
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

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0036".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* FÜR LIKE-DEKLARATION
       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* PARAMETER FÜR DAS CRP0010
       01  SRCLIB-WRK                    PIC X(10).
       01  SRCRCN-WRK                    PIC X(10).
       01  SRCMBR-WRK                    PIC X(10).
       01  OPTION-WRK                    PIC X(1).
       01  SRCSEQ-WRK                    LIKE SRCSEQ OF SRC-WRK.
       01  F3                            PIC X(1).
       01  F12                           PIC X(1).

     ‚* PARAMETER FÜR DAS CLP0008
       01  SEUOPT-WRK                    PIC X(2).
       01  ERROR-TEXT-SHORT              PIC X(255).
       01  ERROR-TEXT-LONG               PIC X(3000).

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-SRCSEQ                      LIKE SRCSEQ OF SRC-WRK.
       01  X-OPTION                      PIC X(1).
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-SRCSEQ
                                X-OPTION
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0036" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DIE CFG-CPY LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

           INITIALIZE X-F3, X-F12.
     ‚* TEILDATEI ÜBERNEHMEN
           MOVE     X-SRCLIB TO SRCLIB-WRK.
           MOVE     X-SRCRCN TO SRCRCN-WRK.
           MOVE     X-SRCMBR TO SRCMBR-WRK.

           EVALUATE CFFK26 OF CFG-CPY
                    WHEN ZEROES PERFORM START-ISEU
                    WHEN 1      PERFORM START-SEU
                    WHEN OTHER  PERFORM START-SEU
           END-EVALUATE.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ISEU STARTEN
     ‚*--------------------------------------------------------------
       START-ISEU SECTION.
       STR-ISE-00.

     ‚* WEITERE ISEU OPTIONEN ÜBERNEHMEN
           MOVE     X-SRCSEQ TO SRCSEQ-WRK.
           MOVE     X-OPTION TO OPTION-WRK.

     ‚* ISEU AUFRUFEN
           CALL     "CRP0010" USING SRCLIB-WRK
                                    SRCRCN-WRK
                                    SRCMBR-WRK
                                    SRCSEQ-WRK
                                    OPTION-WRK
                                    X-F3
                                    X-F12
           END-CALL.

       STR-ISE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN DES SEU ÜBER EIN CL
     ‚*--------------------------------------------------------------
       START-SEU SECTION.
       STR-SEU-00.

           INITIALIZE RET-CODE.
     ‚* MIT DEM SEU DIE DATEN ÄNDERN
           EVALUATE X-OPTION
                    WHEN "2"
                    WHEN "5"
                         MOVE X-OPTION TO SEUOPT-WRK
                    WHEN OTHER
                         MOVE "5"      TO SEUOPT-WRK
           END-EVALUATE.

     ‚* SEU STARTEN
           CALL     "CLP0008"  USING SRCLIB-WRK
                                     SRCRCN-WRK
                                     SRCMBR-WRK
                                     SEUOPT-WRK
                                     ERROR-TEXT-SHORT
                                     ERROR-TEXT-LONG
                                     RET-CODE
           END-CALL.

       STR-SEU-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

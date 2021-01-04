       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0014.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SICHERN EINER SOURCE                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SICHERN EINER SOURCE                         *
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
     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI SEU-SICHERUNGEN
     ‚*-------------------------------------------------------------
           SELECT PSEUHST-DP
                  ASSIGN       TO  DATABASE-PSEUHST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- SEU-SICHERUNGEN
       FD  PSEUHST-DP
           LABEL RECORDS ARE STANDARD.
       01  PSEUHST-P.
           COPY DDS-ALL-FORMATS OF PSEUHST.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0014".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  SEUHST-WRK                    PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0014" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     I-O   PSEUHST-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* KONFIGURATION ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE  "QTEMP"     TO CFDAT4 OF CFG-CPY
                    MOVE  "*SAME"     TO CFDAT5 OF CFG-CPY
           END-IF.

     ‚* LETZTE SICHEURNGSNUMMER ERMITTELN
           MOVE     "SEUHST"          TO LFDN-NAME.
           MOVE     T-USER            TO LFDN-ID.
           CALL     "CFP0021"   USING LFDN-NAME
                                      LFDN-ID
                                      LFDN-WRK
           END-CALL.
           MOVE     "H"               TO SEUHST-WRK(1:1).
           MOVE     LFDN-WRK          TO SEUHST-WRK(2:9).

     ‚* CPYF ZUSAMMENBAUEN
           INITIALIZE CPYF-REC.
           MOVE     X-SRCLIB               TO FROMLIB  OF CPYF-REC.
           MOVE     X-SRCRCN               TO FROMFILE OF CPYF-REC.
           MOVE     X-SRCMBR               TO FROMMBR  OF CPYF-REC.
     ‚* ZIELBIBLIOTHEK ÜBERNEHMEN
           IF       CFDAT4 OF CFG-CPY = "*SAME"
                    MOVE X-SRCLIB          TO TOLIB    OF CPYF-REC
           ELSE
                    MOVE CFDAT4 OF CFG-CPY TO TOLIB    OF CPYF-REC
           END-IF.
     ‚* ZIELDATEI ÜBERNEHMEN
           IF       CFDAT5 OF CFG-CPY = "*SAME"
                    MOVE X-SRCRCN          TO TOFILE   OF CPYF-REC
           ELSE
                    MOVE CFDAT5 OF CFG-CPY TO TOFILE   OF CPYF-REC
           END-IF.
           MOVE     SEUHST-WRK             TO TOMBR    OF CPYF-REC.
           MOVE     "*REPLACE"             TO SRCOPT   OF CPYF-REC.
           MOVE     "*NONE"                TO FMTOPT   OF CPYF-REC.
           MOVE     "*YES"                 TO CRTFIL   OF CPYF-REC.
           CALL     "CLP0053" USING CPYF-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    PERFORM WRITE-SEUHST
           END-IF.

       ENDE.
           CLOSE                 PSEUHST-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES DATENSATZES
     ‚*--------------------------------------------------------------
       WRITE-SEUHST SECTION.
       WRT-SEU-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PSEUHST" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.

           INITIALIZE PSEUHSTF OF PSEUHST-P.
           MOVE     LFDN-WRK            TO SHLFDN OF PSEUHST-P.
           MOVE     SEUHST-WRK          TO SHHLFD OF PSEUHST-P.
     ‚* VON-SOURCE
           MOVE     X-SRCLIB            TO SHFLIB OF PSEUHST-P.
           MOVE     X-SRCRCN            TO SHFRCN OF PSEUHST-P.
           MOVE     X-SRCMBR            TO SHFMBR OF PSEUHST-P.
     ‚* NACH-SOURCE
           MOVE     TOLIB   OF CPYF-REC TO SHTLIB OF PSEUHST-P.
           MOVE     TOFILE  OF CPYF-REC TO SHTRCN OF PSEUHST-P.
           MOVE     TOMBR   OF CPYF-REC TO SHTMBR OF PSEUHST-P.
     ‚* DATUM/USER/ETC
           MOVE     T-USER              TO SHAUSR OF PSEUHST-P
           MOVE     T-TERM              TO SHABS  OF PSEUHST-P
           MOVE     PGM-WRK             TO SHAPGM OF PSEUHST-P
           MOVE     DATE-8              TO SHADTA OF PSEUHST-P
           MOVE     TIME-6              TO SHATIA OF PSEUHST-P
           WRITE    PSEUHST-P.

       WRT-SEU-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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

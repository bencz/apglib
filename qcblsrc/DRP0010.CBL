     ‚*PROCESS   XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             DRP0010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LABEL DRUCKEN                                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DRUCKEN EINES LABELS                         *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
          SOURCE-COMPUTER. IBM-AS400.
          OBJECT-COMPUTER. IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚*    DATEI QCBLSRC (NUR ZUM AUSLESEN)
     ‚*-------------------------------------------------------------
           SELECT QSETSRC-DP
                  ASSIGN       TO  DATABASE-QSETSRC
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
     ‚*---------------------------------------------------------------*
     ‚*    PRINTERFILE
     ‚*---------------------------------------------------------------*
           SELECT DRUCK-DAT
                  ASSIGN        TO FORMATFILE-DRF0010
                  ORGANIZATION  IS SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL.
      /
       DATA DIVISION.
        FILE SECTION.
     ‚*----DATEI QCBLSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
     ‚*------DRUCKAUSGABE----
       FD  DRUCK-DAT
           LABEL RECORDS ARE OMITTED.
       01  DRUCK-REC.
            COPY DDS-ALL-FORMATS OF DRF0010.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "DRP0010".
       01  PGM-TYP                       PIC X(8)  VALUE "PRINT".

       01  PRT-WRK                       PIC X(10) VALUE "DRF0010".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF DRF0010
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF DRF0010
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF DRF0010
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-INDIC.

       01  OVR-OUTQ                      PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-OUTQ                        PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING        X-SRCLIB
                                       X-SRCRCN
                                       X-SRCMBR
                                       X-OUTQ.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "DRP0010" TO PGM-WRK.
           MOVE     "DRF0010" TO PRT-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* OUTQ ERMITTELN?
           IF       X-OUTQ = SPACES
                    PERFORM COPY-GET-OUTQ
                    MOVE CPY-OUTQ TO OVR-OUTQ
           ELSE
                    MOVE X-OUTQ   TO OVR-OUTQ
           END-IF.

           INITIALIZE RET-CODE.
     ‚* OVERWRITE PRINTERFILE AUSFÜHREN
           INITIALIZE OVRPRTF-REC.
           MOVE     "DRF0010" TO FROMFILE OF OVRPRTF-REC.
           MOVE     OVR-OUTQ  TO TOOUTQ   OF OVRPRTF-REC.
           CALL     "CLP0041" USING OVRPRTF-REC
                                    RET-CODE
           END-CALL.

     ‚* PRINTERFILE ÖFFNEN
           OPEN     OUTPUT DRUCK-DAT.

           INITIALIZE FORMAT1.
           INITIALIZE FORMAT2.
           INITIALIZE FORMAT3.

     ‚* ALLE SOURCEZEILEN LESEN
           PERFORM  LESE-DATEN.

       ENDE.
           CLOSE                 DRUCK-DAT.

     ‚* OVERWRITE LÖSCHEN
           INITIALIZE DELOVR-REC.
           MOVE     "DRF0010" TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER SOURCZEILEN
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE RET-CODE.
     ‚* SETZEN OVRWRT FÜR QCBLSRC UND ÖFFNEN DER DATEI
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO RET-CODE
                    GO TO LES-DAT-90
           END-IF.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     INPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           INITIALIZE QSETSRC-P.

       LES-DAT-20.
           READ     QSETSRC-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
            AND     FILE-STATUS NOT = "04"
                    GO TO LES-DAT-80
           END-IF.

     ‚* SOURCE IN FORMAT3 KOPIEREN UND DRUCKEN
           MOVE     SRCSEQ OF QSETSRC-P TO SRCSEQ OF FORMAT3.
           MOVE     SRCDTA OF QSETSRC-P TO SRCDTA OF FORMAT3.
           MOVE     SRCDAT OF QSETSRC-P TO SRCDAT OF FORMAT3.
           MOVE     CORR FORMAT3     TO FMT03-O.
           WRITE    DRUCK-REC FORMAT IS "FMT03".

           GO TO    LES-DAT-20.

     ‚* DATEI NUN WIEDER SCHLIEßEN
       LES-DAT-80.
           CLOSE    QSETSRC-DP.

           PERFORM  DLT-OVR-MBR.

       LES-DAT-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SETZEN OVRWRT FÜR TEILDATEI
     ‚*---------------------------------------------------------------
       SET-OVR-MBR SECTION.
       SET-OVR-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN DER TEILDATEI
           INITIALIZE OVRDBF-REC.
           MOVE     "QSETSRC"  TO FROMFILE OF OVRDBF-REC.
           MOVE     X-SRCLIB   TO TOLIB    OF OVRDBF-REC.
           MOVE     X-SRCRCN   TO TOFILE   OF OVRDBF-REC.
           MOVE     X-SRCMBR   TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ENTFERNEN OVERWRITE FÜR MEMBER
     ‚*---------------------------------------------------------------
       DLT-OVR-MBR SECTION.
       DEL-OVR-00.

     ‚* OVR ZURÜCKSETZEN
           INITIALIZE DELOVR-REC.
           MOVE     X-SRCRCN TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

       DEL-OVR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-OUTQ: ZENTRALE OUTQ-ERMITTLUNG
     ‚*---------------------------------------------------------------

           COPY     GETOUTQ    OF APG-QCPYSRC.
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

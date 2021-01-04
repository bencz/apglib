     ‚*PROCESS   XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             DRP0001.
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
     ‚*---------------------------------------------------------------*
     ‚*    LAYOUTDATEI
     ‚*---------------------------------------------------------------*
           SELECT PETIKLBL-DATP
                  ASSIGN        TO DATABASE-PETIKLBL
                  ORGANIZATION  IS SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FILE-STATUS.
     ‚*---------------------------------------------------------------*
     ‚*    PRINTERFILE
     ‚*---------------------------------------------------------------*
           SELECT DRUCK-DAT
                  ASSIGN        TO FORMATFILE-DRF0001
                  ORGANIZATION  IS SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL.
      /
       DATA DIVISION.
        FILE SECTION.
     ‚*-----DRUCKDATEN----
       FD  PETIKLBL-DATP
           LABEL RECORD STANDARD.
       01  PETIKLBL-RECP.
           COPY DDS-ALL-FORMATS OF PETIKLBL.
      /
     ‚*------DRUCKAUSGABE----
       FD  DRUCK-DAT
           LABEL RECORDS ARE OMITTED.
       01  DRUCK-REC.
            COPY DDS-ALL-FORMATS OF DRF0001.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "DRP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "PRINT".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF DRF0001
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-INDIC.

       01  OVR-OUTQ                      PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-OUTQ                        PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING        X-OUTQ.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "DRP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* OUTQ SETZEN
           IF       ADDRESS OF X-OUTQ = NULL
                    MOVE "PRT01" TO OVR-OUTQ
           ELSE
                    IF   X-OUTQ = SPACES
                         MOVE "PRT01"    TO OVR-OUTQ
                    ELSE
                         MOVE X-OUTQ     TO OVR-OUTQ
                    END-IF
           END-IF.

           INITIALIZE RET-CODE.
     ‚* OVERWRITE PRINTERFILE AUSFÜHREN
           INITIALIZE OVRPRTF-REC.
           MOVE     "DRF0001" TO FROMFILE OF OVRPRTF-REC.
           MOVE     OVR-OUTQ  TO TOOUTQ   OF OVRPRTF-REC.
           CALL     "CLP0041" USING OVRPRTF-REC
                                    RET-CODE
           END-CALL.

     ‚* PRINTERFILE ÖFFNEN
           OPEN     INPUT  PETIKLBL-DATP.
           OPEN     OUTPUT DRUCK-DAT.

           INITIALIZE FORMAT1.
           INITIALIZE PETIKF OF PETIKLBL-RECP.

       ANF020.
           READ     PETIKLBL-DATP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           MOVE     PKPARM OF PETIKLBL-RECP TO AUSTEXT OF FORMAT1.
           MOVE     CORR FORMAT1     TO FMT01-O.
           WRITE    DRUCK-REC FORMAT IS "FMT01".

     ‚* NÄCHSTE ZEILE LESEN
           GO TO    ANF020.

       ENDE.
           CLOSE                 DRUCK-DAT
                                 PETIKLBL-DATP.

     ‚* OVERWRITE LÖSCHEN
           INITIALIZE DELOVR-REC.
           MOVE     "DRF0001" TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
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

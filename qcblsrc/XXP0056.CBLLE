       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0056.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BERICHTSHEFTDATEN                                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  05.01.2014                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANLEGEN VON BERICHTSHEFTDATEN                *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD0056DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* TEMPORÄRE DATEI PTMPSRV
     ‚*-------------------------------------------------------------
           SELECT PTMPSRV-DP
                  ASSIGN       TO  DATABASE-PTMPSRV
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD0056DE.
      /
     ‚*--- TEMPORÄRE PTMPSRV
       FD  PTMPSRV-DP
           LABEL RECORDS ARE STANDARD.
       01  PTMPSRV-P.
           COPY DDS-ALL-FORMATS OF PTMPSRV.
      /
       WORKING-STORAGE SECTION.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0056DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0056DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "XXP0056".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  DLTF-REC                      TYPE DLTF-TYPE.
       01  CRTPF-REC                     TYPE CRTPF-TYPE.
       01  CPYSPLF-REC                   TYPE CPYSPLF-TYPE.
       01  DLTSPLF-REC                   TYPE DLTSPLF-TYPE.
       01  WRKSRVTBLE-REC                TYPE WRKSRVTBLE-TYPE.

     ‚* SCHALTER
       01  FILE-CREATED                   PIC 9(1).
       01  SERVICE-FOUND                  PIC 9(1).

     ‚* ZÄHL- UND POSITIONSVARIABLEN
       01  LINE-AFTER-SERVICE             PIC 9(2).

     ‚* HILFSVARIABLEN
       01  SRV-NAME                       PIC X(30).
       01  PORT-WRK                       PIC X(30).
       01  PROTOKOLL-WRK                  PIC X(30).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-SERVICENAME                  PIC X(20).
       01  X-F3                           PIC X(1).
       01  X-F12                          PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-SERVICENAME
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0056" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0056DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           INITIALIZE X-F3, X-F12.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* TEMPORÄRE DATEI ERSTELLEN
           INITIALIZE FILE-CREATED.
           PERFORM  LOAD-PTMPSRV.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.
     ‚* ÜBERSCHRIFT
           MOVE     "XXP0056*1"     TO MAINUEB OF FORMAT1.
           MOVE     X-SERVICENAME   TO PRTSRV  OF FORMAT1.

           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

       ANF010.
           IF       PRTSRV OF FORMAT1 NOT = SPACES
                    OPEN INPUT PTMPSRV-DP
                    IF   FILE-STATUS = ZEROES
                         PERFORM GET-PORTNAME
                         CLOSE   PTMPSRV-DP
                    END-IF
     ‚*             SERVICE GEFUNDEN?
                    IF   SERVICE-FOUND = 1
                         MOVE SRV-NAME      TO PRTSRV OF FORMAT1
                         MOVE PORT-WRK      TO PRTNUM OF FORMAT1
                         MOVE PROTOKOLL-WRK TO PRTPRT OF FORMAT1
                    END-IF
           END-IF.

           MOVE     "FMT01"         TO FMT-WRK.
           MOVE     FORMAT1         TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* F5 DATEI NEU EINLESEN
           IF       IN05 OF FORMAT1-I-INDIC  = AN
                    PERFORM RELOAD-PTMPSRV
                    GO TO ANF010
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

           INITIALIZE PRTNUM OF FORMAT1.
           INITIALIZE PRTPRT OF FORMAT1.

           GO TO    ANF010.

       ENDE.
           IF       FILE-CREATED = 1
                    PERFORM DELETE-PTMPSRV
           END-IF.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN DER TEMP. DATEI MITTELS SPOOLFILE
     ‚*--------------------------------------------------------------
       LOAD-PTMPSRV SECTION.
       LOD-PTM-00.

     ‚* ERSTELLEN DER TEMP. DATEI FÜR DEN ZU KOPIERENDEN SPOOL
           PERFORM  CREATE-TEMPFILE.
           IF       RET-CODE NOT = SPACES
                    GO TO LOD-PTM-90
           ELSE
                    MOVE 1 TO FILE-CREATED
           END-IF.

     ‚* WRKUSRJOB AUSFÜHREN
           PERFORM  CREATE-WRKSRVTBLE.
           IF       RET-CODE NOT = SPACES
                    GO TO LOD-PTM-90
           END-IF.

     ‚* DURCH WRKSRVTBLE WURDE EIN SPOOLFILE ERSTELLT, DIESES JETZT
     ‚* IN EINE TEMPORÄRE PHYSISCHE DATEI KOPIEREN, "QSYSPRT"
           PERFORM  COPY-SPOOL.
           IF       RET-CODE NOT = SPACES
                    GO TO LOD-PTM-90
           ELSE
                    PERFORM DELETE-SPOOL
           END-IF.

       LOD-PTM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN DER TEMP. DATEI
     ‚*--------------------------------------------------------------
       CREATE-TEMPFILE SECTION.
       CRT-TMF-00.

           INITIALIZE RET-CODE.
     ‚* ERSTELLEN DER "PTMPSRV"
           INITIALIZE CRTPF-REC.
           MOVE     "QTEMP"         TO TOLIB    OF CRTPF-REC.
           MOVE     "PTMPSRV"       TO TOPGM    OF CRTPF-REC.
           MOVE     132             TO RECSIZE  OF CRTPF-REC.
           CALL     "CLP0031" USING CRTPF-REC
                                    RET-CODE
           END-CALL.

       CRT-TMF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WRKSRVTBLE AUSFÜHREN UND SPOOL ERSTELLEN
     ‚*--------------------------------------------------------------
       CREATE-WRKSRVTBLE SECTION.
       CRT-WST-00.

           INITIALIZE WRKSRVTBLE-REC.
           MOVE     "*ALL"       TO SERVICENAME OF WRKSRVTBLE-REC.
           CALL     "CLP0071" USING WRKSRVTBLE-REC
                                    RET-CODE
           END-CALL.

       CRT-WST-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SPOOL IN DIE TEMPORÄRE DATEI KOPIEREN
     ‚*--------------------------------------------------------------
       COPY-SPOOL SECTION.
       COP-SPL-00.

           INITIALIZE RET-CODE.
     ‚* CPYSPLF AUSFÜHREN
           INITIALIZE CPYSPLF-REC.
           MOVE     "QSYSPRT"     TO SPOOLNAME OF CPYSPLF-REC.
           MOVE     "QTEMP"       TO TOLIB     OF CPYSPLF-REC.
           MOVE     "PTMPSRV"     TO TOFILE    OF CPYSPLF-REC.
           MOVE     "*REPLACE"    TO MBROPT    OF CPYSPLF-REC.
           CALL     "CLP0046" USING CPYSPLF-REC
                                    RET-CODE
           END-CALL.

       COP-SPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SPOOL LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-SPOOL SECTION.
       DLT-SPL-00.

           INITIALIZE DLTSPLF-REC.
           MOVE     "*SELECT"       TO FILENAME OF DLTSPLF-REC.
           MOVE     T-USER          TO TUSER    OF DLTSPLF-REC.
           MOVE     "WRKSRVTBLE"    TO USRDTA   OF DLTSPLF-REC.
           CALL     "CLP0072" USING DLTSPLF-REC
                                    RET-CODE
           END-CALL.

       DLT-SPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEI LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-PTMPSRV SECTION.
       DLT-PTM-00.

           INITIALIZE RET-CODE.
     ‚* DLTF AUSFÜHREN
           INITIALIZE DLTF-REC.
           MOVE     "PTMPSRV"       TO FILENAME OF DLTF-REC.
           MOVE     "QTEMP"         TO LIBNAME  OF DLTF-REC.
           CALL     "CLP0044" USING DLTF-REC
                                    RET-CODE
           END-CALL.

       DLT-PTM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEI NEU EINLESEN
     ‚*--------------------------------------------------------------
       RELOAD-PTMPSRV SECTION.
       RLD-PTM-00.

           PERFORM  DELETE-PTMPSRV.
           PERFORM  LOAD-PTMPSRV.

       RLD-PTM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES SPOOLFILES
     ‚*--------------------------------------------------------------
       GET-PORTNAME SECTION.
       GET-PRT-00.

           INITIALIZE SERVICE-FOUND, PROTOKOLL-WRK, PORT-WRK.

           INITIALIZE PTMPSRV-P.

       GET-PRT-20.
           READ     PTMPSRV-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-PRT-90
           END-IF.

     ‚* WENN NICHT GEFUNDEN, HAT DIES FOLGENDES FORMAT:
     ‚* 1   (TABELLE ENTHÄLT KEINE EINTRÄGE)
           IF       PTMPSRV-P(2:1) = "("
                    GO TO GET-PRT-90
           END-IF.

     ‚* WENN GEFUNDEN, HAT DIES FOLGENDES FORMAT:
     ‚* 1  SERVICE . . . . . . . . . . . . . :   AS-ADMIN-HTTP
     ‚* 2  PORT  . . . . . . . . . . . . . . :   2001
     ‚* 3  PROTOKOLL . . . . . . . . . . . . :   TCP
     ‚* 4  TEXT  . . . . . . . . . . . . . . :   AS400 ADMIN HTTP
     ‚* 5  SERVER
     ‚* 6  ALIASNAMEN:
     ‚* 7    ALIASNAME . . . . . . . . . . . :   WWW-HTTP-ADMIN
     ‚* 8    ALIASNAME . . . . . . . . . . . :
     ‚* 9    ALIASNAME . . . . . . . . . . . :
     ‚* 10   ALIASNAME . . . . . . . . . . . :
           IF       SERVICE-FOUND = ZEROES
                    MOVE PTMPSRV-P(40:30) TO SRV-NAME
                    IF   SRV-NAME = PRTSRV OF FORMAT1
     ‚*              MULTISERVER
                     OR  (    PRTSRV OF FORMAT1(4:) = SPACES
                          AND SRV-NAME(1:3) = PRTSRV OF FORMAT1(1:3)
                          AND SRV-NAME(4:6) = "MLTSVR")
                         MOVE 1      TO SERVICE-FOUND
                         MOVE ZEROES TO LINE-AFTER-SERVICE
                    END-IF
           ELSE
                    ADD 1 TO LINE-AFTER-SERVICE
                    EVALUATE LINE-AFTER-SERVICE
                        WHEN 1    MOVE PTMPSRV-P(40:) TO PORT-WRK
                        WHEN 2    MOVE PTMPSRV-P(40:) TO PROTOKOLL-WRK
                    END-EVALUATE
           END-IF.
     ‚* WENN WIR ALLE DATEN HABEN, KÖNNEN WIR AUFHÖREN
           IF       SERVICE-FOUND      = 1
            AND     LINE-AFTER-SERVICE = 2
                    GO TO GET-PRT-90
           END-IF.

           GO TO    GET-PRT-20.

       GET-PRT-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /

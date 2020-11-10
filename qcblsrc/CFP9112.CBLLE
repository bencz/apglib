       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9112.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE EINES ABSTURZWINDOWS                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.11.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE QSECOFR                              *
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
                  ASSIGN       TO  WORKSTATION-CFD9112DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF CFD9112DE.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9112".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CFD9112DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CFD9112DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        PIC X(10).
       01  X-TERM                        PIC X(10).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-TERM.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD9112DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE FORMAT1.
           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP9112"       TO MAINUEB OF FORMAT1.

           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
     ‚*€   MOVE     AUS TO INXX.

     ‚* F12 ABBRECHEN ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /

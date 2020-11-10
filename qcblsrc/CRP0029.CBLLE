       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0027.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGEN VON SOURCE-INFORMATIONEN                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  27.05.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCE-INFORMATIONEN ANZEIGEN                *
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
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-CRD0027DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF CRD0027DE.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  CRD0027DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CRD0027DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0027".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-F3                          PIC X(1).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-F3
                                X-F12.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0027"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CRD0027DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.
           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           PERFORM  SHOW-MEMBER-INFOS.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* MEMBERINFORMATIONEN ANZEIGEN
     ‚*--------------------------------------------------------------
       SHOW-MEMBER-INFOS SECTION.
       SHW-INF-00.

           INITIALIZE FORMAT1.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CRP0027"         TO MAINUEB OF FORMAT1.
           MOVE     "CRP0027*2"       TO SUBUEB  OF FORMAT1.

     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     X-SRCLIB     TO OBJLIB   OF RTVMBRD-REC.
           MOVE     X-SRCRCN     TO FILENAME OF RTVMBRD-REC.
           MOVE     X-SRCMBR     TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

     ‚* DATEN AUS RTVMBRD ANZEIGEN
           MOVE     OBJLIB    OF RTVMBRD-REC TO RTVLIB   OF FORMAT1.
           MOVE     FILENAME  OF RTVMBRD-REC TO RTVRCN   OF FORMAT1.
           MOVE     MBRNAME   OF RTVMBRD-REC TO RTVMBR   OF FORMAT1.
           MOVE     SRCTYPE   OF RTVMBRD-REC TO RTVTYPE  OF FORMAT1.
           MOVE     NBRCURRCD OF RTVMBRD-REC TO RTVCNT   OF FORMAT1.
           MOVE     NBRDLTRCD OF RTVMBRD-REC TO RTVDEL   OF FORMAT1.
           MOVE     MBRTEXT   OF RTVMBRD-REC TO RTVTXT   OF FORMAT1.

     ‚* DATUM AUFBEREITEN
           MOVE     CRTDATE OF RTVMBRD-REC(2:6) TO DT_CRT  OF FORMAT1.
           MOVE     CHGDATE OF RTVMBRD-REC(2:6) TO DT_CHG  OF FORMAT1.
           MOVE     SAVDATE OF RTVMBRD-REC(2:6) TO DT_SAV  OF FORMAT1.
           MOVE     RSTDATE OF RTVMBRD-REC(2:6) TO DT_RST  OF FORMAT1.

     ‚* UHRZEIT AUFBEREITEN
           MOVE     CRTDATE OF RTVMBRD-REC(8:6) TO TI_CRT  OF FORMAT1.
           MOVE     CHGDATE OF RTVMBRD-REC(8:6) TO TI_CHG  OF FORMAT1.
           MOVE     SAVDATE OF RTVMBRD-REC(8:6) TO TI_SAV  OF FORMAT1.
           MOVE     RSTDATE OF RTVMBRD-REC(8:6) TO TI_RST  OF FORMAT1.

       SHW-INF-10.
           MOVE     PGM-TYP         TO PGMTYP OF FORMAT1.

           MOVE     CORR INXX       TO FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO FMT-WRK.
           MOVE     FORMAT1         TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO SHW-INF-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO SHW-INF-90
           END-IF.

     ‚* JETZT DEN TEXT ÄNDERN
           INITIALIZE CHGPFM-REC.
           MOVE     RTVLIB OF FORMAT1 TO LIBNAME  OF CHGPFM-REC.
           MOVE     RTVRCN OF FORMAT1 TO FILENAME OF CHGPFM-REC.
           MOVE     RTVMBR OF FORMAT1 TO MBRNAME  OF CHGPFM-REC.
           MOVE     RTVTXT OF FORMAT1 TO SRCTXT   OF CHGPFM-REC.
           CALL     "CLP0049" USING CHGPFM-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    INITIALIZE RTVMBRD-REC
                    MOVE  RTVLIB OF FORMAT1 TO OBJLIB   OF RTVMBRD-REC
                    MOVE  RTVRCN OF FORMAT1 TO FILENAME OF RTVMBRD-REC
                    MOVE  RTVMBR OF FORMAT1 TO MBRNAME  OF RTVMBRD-REC
                    CALL "CLP0002" USING RTVMBRD-REC
                                         RET-CODE
                    END-CALL
           END-IF.

       SHW-INF-90.
           EXIT.
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

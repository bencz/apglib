       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0012.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE WINDOW ZUR EINGABE EINER TEILDATEI                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  WINDOW ZUR EINGABE TEILDATEI                 *
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
                  ASSIGN       TO  WORKSTATION-CFD0012DE
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
            COPY DDS-ALL-FORMATS OF CFD0012DE.
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

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0012".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  INXX.
        05 IN40                          PIC 1.
        05 IN41                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CFD0012DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF CFD0012DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        PIC X(20).
       01  X-AKTION                      PIC X(20).
       01  X-F12                         PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-AKTION
                                X-F12.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0012" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0012DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FORMAT1 INITIALISIEREN
           INITIALIZE X-F12.
           INITIALIZE FORMAT1.
           MOVE     AUS TO INXX.

           MOVE     "FMT02"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP0012*1"     TO MAINUEB OF FORMAT1.
           MOVE     X-USER          TO TUSER   OF FORMAT1.

       ANF010.
           EVALUATE X-AKTION
                    WHEN "USER"   MOVE AN TO IN40 OF INXX
                    WHEN "GROUP"  MOVE AN TO IN41 OF INXX
           END-EVALUATE.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

           EVALUATE X-AKTION
                    WHEN "USER"   PERFORM CHECK-USER
                    WHEN "GROUP"  PERFORM CHECK-GROUP
           END-EVALUATE.
           IF       RET-CODE NOT = SPACES
                    GO TO ANF010
           END-IF.

     ‚* WERTE ÜBERNEHMEN
           MOVE     TUSER  OF FORMAT1 TO X-USER.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES EINEN USER GIBT
     ‚*--------------------------------------------------------------
       CHECK-USER SECTION.
       CHK-USR-00.

           INITIALIZE RET-CODE.
     ‚* ES DÜRFEN NUR USERNAMES EINGETRAGEN WERDEN, DAHER PRÜFEN
     ‚* OB ES DEN USER GIBT
           INITIALIZE CHKOBJ-REC.
           MOVE     TUSER OF FORMAT1   TO OBJ     OF CHKOBJ-REC.
           MOVE     "QSYS"             TO LIB     OF CHKOBJ-REC.
           MOVE     "*USRPRF"          TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-USR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE GRUPPE GIBT
     ‚*--------------------------------------------------------------
       CHECK-GROUP SECTION.
       CHK-GRP-00.

           INITIALIZE RET-CODE.
     ‚* KOPFSATZ LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H051"            TO CFID   OF CFG-CPY.
           MOVE     TUSER OF FORMAT1  TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-GRP-90.
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

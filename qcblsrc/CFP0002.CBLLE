     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CFP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSFÜHREN VON ADD-ERR-TAB ETC                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  15.12.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DSPMAIN FUNKTIONEN AUS COPY-MODUL AUSFÜHREN  *
     ‚*                  DAMIT DAS IM DEBUGGER SCHNELLER GEHT         *
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

       01  PGM-WRK                       PIC X(10) VALUE "CFP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-DSP-ATTR                    LIKE DSP-ATTR.
       01  X-AKTION                      LIKE DSP-AKTION.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-DSP-ATTR
                                         X-AKTION.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* WERTE ÜBERNEHMEN
           MOVE     X-DSP-ATTR TO DSP-ATTR.

     ‚* AKTION PRÜFEN
           EVALUATE X-AKTION
                    WHEN "ADD-ERR-TAB"
                         PERFORM DSP-ADD-ERR-TAB
           END-EVALUATE.

     ‚* TABELLE ZURÜCKGEBEN
           MOVE     DSP-ATTR TO X-DSP-ATTR.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHLER-FELD HINZUFÜGEN
     ‚*--------------------------------------------------------------
       DSP-ADD-ERR-TAB SECTION.
       DSP-ADE-00.

     ‚* KEIN FEHLERFELD?
           IF       DSP-ERR-FLD = SPACES
                    GO TO DSP-ADE-90
           END-IF.

           ADD      1 TO DSP-ERR-POS.
           MOVE     DSP-ERR-FLD     TO DSI-ERR-FLD(DSP-ERR-POS).

     ‚* JETZT DAS FELD WIEDER ZURÜCKSETZEN
           INITIALIZE DSP-ERR-FLD.

       DSP-ADE-90.
           EXIT.
      /

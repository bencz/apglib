       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SCHLÜSSELKONFIGURATION                                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSWAHL + ANZEIGE PROGRAMME                  *
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
                  ASSIGN       TO  WORKSTATION-CFD0001DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI TAGS SORTIERT NACH TAG
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DISPLAY
       FD  WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF CFD0001DE.
      /
     ‚*--- PROGRAMME
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
     ‚*--- TAGS
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN40                          PIC 1.
        05 IN41                          PIC 1.
        05 IN42                          PIC 1.
        05 IN91                          PIC 1.
        05 IN94                          PIC 1.
        05 IN95                          PIC 1.
        05 IN96                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF CFD0001DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CFD0001DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  CFD0001DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  CFD0001DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  CFD0001DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  CFD0001DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* FMT10: AUTOMATISCHES ANMELDEN
       01  FORMAT10X.
           COPY  DDS-FMT10-O OF  CFD0001DE
           REPLACING FMT10-O BY FORMAT10
                     FMT10-O-INDIC BY FORMAT10-O-INDIC.

       01  FORMAT10Y.
           COPY  DDS-FMT10-I OF  CFD0001DE
           REPLACING FMT10-I BY FORMAT10-I
                     FMT10-I-INDIC BY FORMAT10-I-INDIC.

     ‚* FMT11: HERMES-VORBELEGUNG
       01  FORMAT11X.
           COPY  DDS-FMT11-O OF  CFD0001DE
           REPLACING FMT11-O BY FORMAT11
                     FMT11-O-INDIC BY FORMAT11-O-INDIC.

       01  FORMAT11Y.
           COPY  DDS-FMT11-I OF  CFD0001DE
           REPLACING FMT11-I BY FORMAT11-I
                     FMT11-I-INDIC BY FORMAT11-I-INDIC.

     ‚* FMT12: GENERELLE-EINSTELLUNGEN
       01  FORMAT12X.
           COPY  DDS-FMT12-O OF  CFD0001DE
           REPLACING FMT12-O BY FORMAT12
                     FMT12-O-INDIC BY FORMAT12-O-INDIC.

       01  FORMAT12Y.
           COPY  DDS-FMT12-I OF  CFD0001DE
           REPLACING FMT12-I BY FORMAT12-I
                     FMT12-I-INDIC BY FORMAT12-I-INDIC.

     ‚* FMT13: PROGRAMMAUFRUFE
       01  FORMAT13X.
           COPY  DDS-FMT13-O OF  CFD0001DE
           REPLACING FMT13-O BY FORMAT13
                     FMT13-O-INDIC BY FORMAT13-O-INDIC.

       01  FORMAT13Y.
           COPY  DDS-FMT13-I OF  CFD0001DE
           REPLACING FMT13-I BY FORMAT13-I
                     FMT13-I-INDIC BY FORMAT13-I-INDIC.

     ‚* FMT14: SEU-EINSTELLUNGEN
       01  FORMAT14X.
           COPY  DDS-FMT14-O OF  CFD0001DE
           REPLACING FMT14-O BY FORMAT14
                     FMT14-O-INDIC BY FORMAT14-O-INDIC.

       01  FORMAT14Y.
           COPY  DDS-FMT14-I OF  CFD0001DE
           REPLACING FMT14-I BY FORMAT14-I
                     FMT14-I-INDIC BY FORMAT14-I-INDIC.

     ‚* FMT15: USER-DATEN (PBIRTH-IMPORT)
       01  FORMAT15X.
           COPY  DDS-FMT15-O OF  CFD0001DE
           REPLACING FMT15-O BY FORMAT15
                     FMT15-O-INDIC BY FORMAT15-O-INDIC.

       01  FORMAT15Y.
           COPY  DDS-FMT15-I OF  CFD0001DE
           REPLACING FMT15-I BY FORMAT15-I
                     FMT15-I-INDIC BY FORMAT15-I-INDIC.

       01  FORMAT15AX.
           COPY  DDS-FMT15A-O OF  CFD0001DE
           REPLACING FMT15A-O BY FORMAT15A
                     FMT15A-O-INDIC BY FORMAT15A-O-INDIC.

       01  FORMAT15AY.
           COPY  DDS-FMT15A-I OF  CFD0001DE
           REPLACING FMT15A-I BY FORMAT15A-I
                     FMT15A-I-INDIC BY FORMAT15A-I-INDIC.

       01  FORMAT15BX.
           COPY  DDS-FMT15B-O OF  CFD0001DE
           REPLACING FMT15B-O BY FORMAT15B
                     FMT15B-O-INDIC BY FORMAT15B-O-INDIC.

       01  FORMAT15BY.
           COPY  DDS-FMT15B-I OF  CFD0001DE
           REPLACING FMT15B-I BY FORMAT15B-I
                     FMT15B-I-INDIC BY FORMAT15B-I-INDIC.

       01  FORMAT15CX.
           COPY  DDS-FMT15C-O OF  CFD0001DE
           REPLACING FMT15C-O BY FORMAT15C
                     FMT15C-O-INDIC BY FORMAT15C-O-INDIC.

       01  FORMAT15CY.
           COPY  DDS-FMT15C-I OF  CFD0001DE
           REPLACING FMT15C-I BY FORMAT15C-I
                     FMT15C-I-INDIC BY FORMAT15C-I-INDIC.

     ‚* FMT16: RAUMEINSTELLUNGEN
       01  FORMAT16X.
           COPY  DDS-FMT16-O OF  CFD0001DE
           REPLACING FMT16-O BY FORMAT16
                     FMT16-O-INDIC BY FORMAT16-O-INDIC.

       01  FORMAT16Y.
           COPY  DDS-FMT16-I OF  CFD0001DE
           REPLACING FMT16-I BY FORMAT16-I
                     FMT16-I-INDIC BY FORMAT16-I-INDIC.

     ‚* FMT17: ARTIKELNUMMERN
       01  FORMAT17X.
           COPY  DDS-FMT17-O OF  CFD0001DE
           REPLACING FMT17-O BY FORMAT17
                     FMT17-O-INDIC BY FORMAT17-O-INDIC.

       01  FORMAT17Y.
           COPY  DDS-FMT17-I OF  CFD0001DE
           REPLACING FMT17-I BY FORMAT17-I
                     FMT17-I-INDIC BY FORMAT17-I-INDIC.

     ‚* FMT18: BESTELLTYPEN
       01  FORMAT18X.
           COPY  DDS-FMT18-O OF  CFD0001DE
           REPLACING FMT18-O BY FORMAT18
                     FMT18-O-INDIC BY FORMAT18-O-INDIC.

       01  FORMAT18Y.
           COPY  DDS-FMT18-I OF  CFD0001DE
           REPLACING FMT18-I BY FORMAT18-I
                     FMT18-I-INDIC BY FORMAT18-I-INDIC.

     ‚* FMT19: IMPORT PVEREIN
       01  FORMAT19X.
           COPY  DDS-FMT19-O OF  CFD0001DE
           REPLACING FMT19-O BY FORMAT19
                     FMT19-O-INDIC BY FORMAT19-O-INDIC.

       01  FORMAT19Y.
           COPY  DDS-FMT19-I OF  CFD0001DE
           REPLACING FMT19-I BY FORMAT19-I
                     FMT19-I-INDIC BY FORMAT19-I-INDIC.

     ‚* FMT20: GRUPPENVERWALTUNG
       01  FORMAT20X.
           COPY  DDS-FMT20-O OF  CFD0001DE
           REPLACING FMT20-O BY FORMAT20
                     FMT20-O-INDIC BY FORMAT20-O-INDIC.

       01  FORMAT20Y.
           COPY  DDS-FMT20-I OF  CFD0001DE
           REPLACING FMT20-I BY FORMAT20-I
                     FMT20-I-INDIC BY FORMAT20-I-INDIC.

     ‚* FMT21: STRPCCMD-PFADVERWALTUNG
       01  FORMAT21X.
           COPY  DDS-FMT21-O OF  CFD0001DE
           REPLACING FMT21-O BY FORMAT21
                     FMT21-O-INDIC BY FORMAT21-O-INDIC.

       01  FORMAT21Y.
           COPY  DDS-FMT21-I OF  CFD0001DE
           REPLACING FMT21-I BY FORMAT21-I
                     FMT21-I-INDIC BY FORMAT21-I-INDIC.

     ‚* FMT22: MASCHINENSTATUS
       01  FORMAT22X.
           COPY  DDS-FMT22-O OF  CFD0001DE
           REPLACING FMT22-O BY FORMAT22
                     FMT22-O-INDIC BY FORMAT22-O-INDIC.

       01  FORMAT22Y.
           COPY  DDS-FMT22-I OF  CFD0001DE
           REPLACING FMT22-I BY FORMAT22-I
                     FMT22-I-INDIC BY FORMAT22-I-INDIC.

     ‚* FMT23: SPRACHEN
       01  FORMAT23X.
           COPY  DDS-FMT23-O OF  CFD0001DE
           REPLACING FMT23-O BY FORMAT23
                     FMT23-O-INDIC BY FORMAT23-O-INDIC.

       01  FORMAT23Y.
           COPY  DDS-FMT23-I OF  CFD0001DE
           REPLACING FMT23-I BY FORMAT23-I
                     FMT23-I-INDIC BY FORMAT23-I-INDIC.

     ‚* FMT24: DRUCHWAHLEN
       01  FORMAT24X.
           COPY  DDS-FMT24-O OF  CFD0001DE
           REPLACING FMT24-O BY FORMAT24
                     FMT24-O-INDIC BY FORMAT24-O-INDIC.

       01  FORMAT24Y.
           COPY  DDS-FMT24-I OF  CFD0001DE
           REPLACING FMT24-I BY FORMAT24-I
                     FMT24-I-INDIC BY FORMAT24-I-INDIC.

     ‚* FMT25: TODO-KATEGORIEN
       01  FORMAT25X.
           COPY  DDS-FMT25-O OF  CFD0001DE
           REPLACING FMT25-O BY FORMAT25
                     FMT25-O-INDIC BY FORMAT25-O-INDIC.

       01  FORMAT25Y.
           COPY  DDS-FMT25-I OF  CFD0001DE
           REPLACING FMT25-I BY FORMAT25-I
                     FMT25-I-INDIC BY FORMAT25-I-INDIC.

     ‚* FMT26: MAIL-KONFIGURATION
       01  FORMAT26X.
           COPY  DDS-FMT26-O OF  CFD0001DE
           REPLACING FMT26-O BY FORMAT26
                     FMT26-O-INDIC BY FORMAT26-O-INDIC.

       01  FORMAT26Y.
           COPY  DDS-FMT26-I OF  CFD0001DE
           REPLACING FMT26-I BY FORMAT26-I
                     FMT26-I-INDIC BY FORMAT26-I-INDIC.

     ‚* FMT27: AZUBI-EINSTELLUNGEN
       01  FORMAT27X.
           COPY  DDS-FMT27-O OF  CFD0001DE
           REPLACING FMT27-O BY FORMAT27
                     FMT27-O-INDIC BY FORMAT27-O-INDIC.

       01  FORMAT27Y.
           COPY  DDS-FMT27-I OF  CFD0001DE
           REPLACING FMT27-I BY FORMAT27-I
                     FMT27-I-INDIC BY FORMAT27-I-INDIC.

     ‚* FMT28: BERUFSSCHULEN
       01  FORMAT28X.
           COPY  DDS-FMT28-O OF  CFD0001DE
           REPLACING FMT28-O BY FORMAT28
                     FMT28-O-INDIC BY FORMAT28-O-INDIC.

       01  FORMAT28Y.
           COPY  DDS-FMT28-I OF  CFD0001DE
           REPLACING FMT28-I BY FORMAT28-I
                     FMT28-I-INDIC BY FORMAT28-I-INDIC.

     ‚* FMT29: AUSBILDUNGSBERUFE
       01  FORMAT29X.
           COPY  DDS-FMT29-O OF  CFD0001DE
           REPLACING FMT29-O BY FORMAT29
                     FMT29-O-INDIC BY FORMAT29-O-INDIC.

       01  FORMAT29Y.
           COPY  DDS-FMT29-I OF  CFD0001DE
           REPLACING FMT29-I BY FORMAT29-I
                     FMT29-I-INDIC BY FORMAT29-I-INDIC.

     ‚* FMT30: KINO-ANMELDUNG
       01  FORMAT30X.
           COPY  DDS-FMT30-O OF  CFD0001DE
           REPLACING FMT30-O BY FORMAT30
                     FMT30-O-INDIC BY FORMAT30-O-INDIC.

       01  FORMAT30Y.
           COPY  DDS-FMT30-I OF  CFD0001DE
           REPLACING FMT30-I BY FORMAT30-I
                     FMT30-I-INDIC BY FORMAT30-I-INDIC.

     ‚* FMT31: DIENSTEINTRÄGE
       01  FORMAT31X.
           COPY  DDS-FMT31-O OF  CFD0001DE
           REPLACING FMT31-O BY FORMAT31
                     FMT31-O-INDIC BY FORMAT31-O-INDIC.

       01  FORMAT31Y.
           COPY  DDS-FMT31-I OF  CFD0001DE
           REPLACING FMT31-I BY FORMAT31-I
                     FMT31-I-INDIC BY FORMAT31-I-INDIC.

     ‚* FMT32: EINSTELLUNGEN FÜR AZUBI-VERWALTUNG
       01  FORMAT32X.
           COPY  DDS-FMT32-O OF  CFD0001DE
           REPLACING FMT32-O BY FORMAT32
                     FMT32-O-INDIC BY FORMAT32-O-INDIC.

       01  FORMAT32Y.
           COPY  DDS-FMT32-I OF  CFD0001DE
           REPLACING FMT32-I BY FORMAT32-I
                     FMT32-I-INDIC BY FORMAT32-I-INDIC.

     ‚* FMT33: TODO-FEHLERNACHRICHTEN
       01  FORMAT33X.
           COPY  DDS-FMT33-O OF  CFD0001DE
           REPLACING FMT33-O BY FORMAT33
                     FMT33-O-INDIC BY FORMAT33-O-INDIC.

       01  FORMAT33Y.
           COPY  DDS-FMT33-I OF  CFD0001DE
           REPLACING FMT33-I BY FORMAT33-I
                     FMT33-I-INDIC BY FORMAT33-I-INDIC.

     ‚* FMT34: SPEISEPLAN
       01  FORMAT34X.
           COPY  DDS-FMT34-O OF  CFD0001DE
           REPLACING FMT34-O BY FORMAT34
                     FMT34-O-INDIC BY FORMAT34-O-INDIC.

       01  FORMAT34Y.
           COPY  DDS-FMT34-I OF  CFD0001DE
           REPLACING FMT34-I BY FORMAT34-I
                     FMT34-I-INDIC BY FORMAT34-I-INDIC.

     ‚* FMT35: RESTAURANTS
       01  FORMAT35X.
           COPY  DDS-FMT35-O OF  CFD0001DE
           REPLACING FMT35-O BY FORMAT35
                     FMT35-O-INDIC BY FORMAT35-O-INDIC.

       01  FORMAT35Y.
           COPY  DDS-FMT35-I OF  CFD0001DE
           REPLACING FMT35-I BY FORMAT35-I
                     FMT35-I-INDIC BY FORMAT35-I-INDIC.

     ‚* FMT36: TODO-EINSTELLUNGEN
       01  FORMAT36X.
           COPY  DDS-FMT36-O OF  CFD0001DE
           REPLACING FMT36-O BY FORMAT36
                     FMT36-O-INDIC BY FORMAT36-O-INDIC.

       01  FORMAT36Y.
           COPY  DDS-FMT36-I OF  CFD0001DE
           REPLACING FMT36-I BY FORMAT36-I
                     FMT36-I-INDIC BY FORMAT36-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0001".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PCONFIG-WRK
       01  PCONFIG-WRK.
            COPY DDS-ALL-FORMATS OF PCONFIG.

     ‚* CL-PARAMETER
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.

       01  AUSWAHL                       PIC X(2).
       01  NEUANLAGE                     PIC 9(1).
       01  NEUANLAGE-WRK                 PIC 9(1).

     ‚* PARAMETER FÜR DAS HLP004
       01  TELEPHONE-WRK                 PIC X(20).

     ‚* PARAMETER FÜR DAS CFP0007
       01  CFKEY-DUMMY                   LIKE CFKEY  OF PCONFIG-P.
       01  CFKEY2-DUMMY                  LIKE CFKEY2 OF PCONFIG-P.
       01  CFKEY3-DUMMY                  LIKE CFKEY3 OF PCONFIG-P.
       01  CFKEY4-DUMMY                  LIKE CFKEY4 OF PCONFIG-P.

     ‚* PARAMETER FÜR DAS CFP0008
       01  CFID-WRK                      LIKE CFID   OF PCONFIG-P.
       01  CFKEY-WRK                     LIKE CFKEY  OF PCONFIG-P.
       01  CFKEY2-WRK                    LIKE CFKEY2 OF PCONFIG-P.
       01  CFKEY3-WRK                    LIKE CFKEY3 OF PCONFIG-P.
       01  CFKEY4-WRK                    LIKE CFKEY4 OF PCONFIG-P.

     ‚* PARAMETER FÜR DAS CFP0027
       01  FORMAT-DUMMY                  PIC X(10).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CFD0001DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCONFIG-DP.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.

       ANF010.
     ‚* VORAUSWAHL: SELEKTIEREN DES KEYS
           PERFORM  VORAUSWAHL.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN
           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
                    IF   F3  = "1"
                         GO TO ENDE
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANF010
                    END-IF
                    GO TO ANF020
           END-IF.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-SUBFILE.
           IF       F3 = "1"
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
           END-IF.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO ANF010
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SELEKTIEREN ALLER NÖTIGEN DATEN
     ‚*--------------------------------------------------------------
       VORAUSWAHL SECTION.
       VOR-AUS-00.

           INITIALIZE F3, F5, F12.
           MOVE     1 TO SFL-ZEILE.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT1.
           MOVE     "CFP0001*1"       TO SUBUEB  OF FORMAT1.

       VOR-AUS-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO VOR-AUS-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO VOR-AUS-90
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR SCHLÜSSEL?
           IF       CFID   OF FORMAT1(1:1) = "?"
                    MOVE   SPACES TO CFKEY-DUMMY
                    MOVE   SPACES TO CFKEY2-DUMMY
                    MOVE   SPACES TO CFKEY3-DUMMY
                    MOVE   SPACES TO CFKEY4-DUMMY
                    CALL   "CFP0007" USING CFID   OF FORMAT1
                                           CFKEY-DUMMY
                                           CFKEY2-DUMMY
                                           CFKEY3-DUMMY
                                           CFKEY4-DUMMY
                                           F12
                    END-CALL
                    INITIALIZE F12
                    GO TO VOR-AUS-10
           END-IF.

       VOR-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* PCONFIG LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT1   TO CFID   OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCONFIG-DP NEXT RECORD WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = CFID   OF FORMAT1
                    GO TO LES-DAT-90
           END-IF.

     ‚* KOPFSÄTZE ÜBERLESEN
           IF       CFKOPF OF PCONFIG-P NOT = ZEROES
                    GO TO LES-DAT-20
           END-IF.

     ‚* KEY SELEKTIERT?
           IF       CFKEY  OF FORMAT1   NOT = SPACES
            AND     CFKEY  OF PCONFIG-P NOT = CFKEY  OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* KEY2 SELEKTIERT?
           IF       CFKEY2 OF FORMAT1   NOT = SPACES
            AND     CFKEY2 OF PCONFIG-P NOT = CFKEY2 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* KEY3 SELEKTIERT?
           IF       CFKEY3 OF FORMAT1   NOT = SPACES
            AND     CFKEY3 OF PCONFIG-P NOT = CFKEY3 OF FORMAT1
                    GO TO LES-DAT-20
           END-IF.

     ‚* NACH TEXT SELEKTIEREN?
           IF       TXTSEL OF FORMAT1   NOT = SPACES
                    MOVE PCONFIG-P           TO RECORD-WRK
                    MOVE LENGTH OF PCONFIG-P TO RECORD-LEN
                    MOVE TXTSEL OF FORMAT1   TO SELEKTION-WRK
                    CALL PROCEDURE "HLP0001" USING RECORD-WRK
                                                   RECORD-LEN
                                                   SELEKTION-WRK
                                                   RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* AUSGABE EINER SUBFILE-ZEILE
           PERFORM  AUSGABE-SFL.
           IF       SUBFILE-RECORD-NUMBER < 9999
                    GO TO LES-DAT-20
           END-IF.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER SFL-ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-SFL SECTION.
       AUS-SFL-00.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
           MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO ANZREC-WRK.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           MOVE     AUS TO INXX.

       AUS-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.

           MOVE     AUS    TO INXX.
  ******   MOVE     1      TO  SUBFILE-RECORD-NUMBER.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           EVALUATE SFL-ZEILE
                    WHEN 1
                         MOVE SUBFILE-RECORD-NUMBER TO SFL-ZEILE
                         MOVE 1 TO SUBFILE-RECORD-NUMBER
                    WHEN OTHER
                         IF   SFL-ZEILE <= ANZREC-WRK
                              MOVE SFL-ZEILE  TO SUBFILE-RECORD-NUMBER
                         ELSE
                              MOVE ANZREC-WRK TO SUBFILE-RECORD-NUMBER
                         END-IF
           END-EVALUATE.

           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

     ‚* F3 ENDE
           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT3-I-INDIC = AN
                    PERFORM COPY-DISPLAY-CURSER
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 OF FORMAT3-I-INDIC = AN
                    MOVE    1 TO NEUANLAGE
                    PERFORM CHANGE-KEY
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                         GO TO ANZ-SFL-20
                    END-IF
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.

           IF       AUSW OF FORMAT2 = SPACES
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

     ‚* BERECHTIGUNG FÜR DIESE FUNKTION ÜBERPRÜFEN
           IF       AUSW OF FORMAT2 NOT = SPACES
                    MOVE  SPACES          TO RET-CODE
                    MOVE  AUSW OF FORMAT2 TO AUSWAHL
                    CALL "CFP9002" USING T-USER
                                         AUSWAHL
                                         PGM-WRK
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         MOVE     SPACES TO AUSW OF FORMAT2
                         MOVE     "FMT02" TO FMT-WRK
                         MOVE     FORMAT2 TO WSREC-WRK
                         PERFORM  COPY-DISPLAY-SFLREWRITE
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* ÄNDERN DES DATENSATZES
           IF       AUSW OF FORMAT2 = "1"
                    MOVE     ZEROES TO NEUANLAGE
                    PERFORM  CHANGE-KEY
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
                    IF   F12 = "1"
                         INITIALIZE F12
                    END-IF
           END-IF.

     ‚* LÖSCHEN DES SATZES
           IF       AUSW OF FORMAT2 = "4"
                    PERFORM  DELETE-KEY
           END-IF.

     ‚* DETAILANZEIGE RECORD
           IF       AUSW OF FORMAT2 = "5"
                    PERFORM  SHOW-KEY-RECORD
                    IF   F3  = "1"
                         GO TO ANZ-SFL-90
                    END-IF
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHLERMELDUNG WENN KEINE SFL-ZEILEN VORHANDEN
     ‚*--------------------------------------------------------------
       FEHLERMELDUNG SECTION.
       FEH-MEL-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT3.

     ‚* AUSGABE INFOZEILE DASS LE LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT3.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FORMAT3-O-INDIC.
           MOVE     "FMT03"        TO FMT-WRK.
           MOVE     FORMAT3        TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.

           IF       IN03 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO FEH-MEL-90
           END-IF.

     ‚* F8 NEUANLAGE
           IF       IN08 OF FORMAT3-I-INDIC = AN
                    MOVE    1 TO NEUANLAGE
                    PERFORM CHANGE-KEY
                    IF   F3  = "1"
                         GO TO FEH-MEL-90
                    END-IF
                    INITIALIZE F12
                    GO TO FEH-MEL-90
           END-IF.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT3.
           MOVE     "CFP0001*3"       TO SUBUEB  OF FORMAT3.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NEUANLAGE EINES PROGRAMMS
     ‚*--------------------------------------------------------------
       CHANGE-KEY SECTION.
       CHG-KEY-00.

           INITIALIZE F3, F12.
     ‚* LESEN DES SCHLÜSSELS
           IF       NEUANLAGE = ZEROES
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE  CFID   OF FORMAT2 TO CFID   OF PCONFIG-P
                    MOVE  CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P
                    MOVE  CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P
                    MOVE  CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P
                    MOVE  CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P
                    READ  PCONFIG-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
                         MOVE 1 TO NEUANLAGE
                    END-IF
           END-IF.

     ‚* JE NACH SCHLÜSSEL ANDERES FORMAT AUFRUFEN
           EVALUATE CFID   OF FORMAT1
     ‚*             AUTOMATISCHES ANMELDEN
                    WHEN "A100"
                         PERFORM EDIT-A100
     ‚*             PROGRAMMAUFRUFE FÜR COMMANDS
                    WHEN "A200"
                         PERFORM EDIT-A200
     ‚*             PFADANGABEN FÜR STRPCCMD-PFADE
                    WHEN "A202"
                         PERFORM EDIT-A202
     ‚*             SPRACHEINSTELLUNGEN
                    WHEN "A300"
                         PERFORM EDIT-A300
     ‚*             MAIL-KONFIGURATION
                    WHEN "A400"
                         PERFORM EDIT-A400
     ‚*             GENERELLE-EINSTELLUNGEN
                    WHEN "H000"
                         PERFORM EDIT-H000
     ‚*             GENERELLE USER-EINSTELLUNGEN
                    WHEN "H050"
                         PERFORM EDIT-H050
     ‚*             GENERELLE GRUPPEN-EINSTELLUNGEN
                    WHEN "H051"
                         PERFORM EDIT-H051
     ‚*             GENERELLE RAUM-EINSTELLUNGEN
                    WHEN "H052"
                         PERFORM EDIT-H052
     ‚*             GENERELLE TELEFON-EINSTELLUNGEN
                    WHEN "H053"
                         PERFORM EDIT-H053
     ‚*             HERMES-VORBELEGUNG
                    WHEN "H100"
                         PERFORM EDIT-H100
     ‚*             SEU-EINSTELLUNGEN
                    WHEN "H101"
                         PERFORM EDIT-H101
     ‚*             TODO-KATEGORIEN
                    WHEN "H102"
                         PERFORM EDIT-H102
     ‚*             TODO-FEHLERNACHRICHTEN
                    WHEN "H102.1"
                         PERFORM EDIT-H102-1
     ‚*             TODO-EINSTELLUNGEN
                    WHEN "H102.2"
                         PERFORM EDIT-H102-2
     ‚*             AZUBIVERWALTUNG-EINSTELLUNGEN
                    WHEN "H103"
                         PERFORM EDIT-H103
     ‚*             IMPORT PVEREIN
                    WHEN "I101"
                         PERFORM EDIT-I101
     ‚*             KINO-ANMELDUNG
                    WHEN "K100"
                         PERFORM EDIT-K100
     ‚*             E+P DIENSTINFORMATIONEN
                    WHEN "K110"
                         PERFORM EDIT-K110
     ‚*             MASCHINENSTATUS
                    WHEN "K111"
                         PERFORM EDIT-K111
     ‚*             BESTELLTYPEN
                    WHEN "K120"
                         PERFORM EDIT-K120
     ‚*             ARTIKELNUMMERN
                    WHEN "K130"
                         PERFORM EDIT-K130
     ‚*             SPEISEPLAN
                    WHEN "K200"
                         PERFORM EDIT-K200
     ‚*             RESTAURANTS
                    WHEN "K201"
                         PERFORM EDIT-K201
     ‚*             AZUBI-EINSTELLUNGEN
                    WHEN "V100"
                         PERFORM EDIT-V100
     ‚*             BERUFSSCHULEN
                    WHEN "V110"
                         PERFORM EDIT-V110
     ‚*             BERUFSBEZEICHNUNGEN
                    WHEN "V112"
                         PERFORM EDIT-V112
           END-EVALUATE.

       CHG-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHLÜSSELEINTRAG ERSTELLEN BZW. AKTUALISIEREN
     ‚*--------------------------------------------------------------
       UPDATE-KEY SECTION.
       UPD-KEY-00.

           INITIALIZE RET-CODE.
     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF PCONFIG-WRK TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF PCONFIG-WRK TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF PCONFIG-WRK TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF PCONFIG-WRK TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF PCONFIG-WRK TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             NEUANLAGE? DANN DARF DER HIER NICHT DA SEIN
                    IF   NEUANLAGE NOT = ZEROES
                         MOVE "1" TO RET-CODE
                         GO TO UPD-KEY-90
                    END-IF
     ‚*             DATEN FÜLLEN
                    MOVE CORR PCONFIGF OF PCONFIG-WRK TO
                              PCONFIGF OF PCONFIG-P
     ‚*             USER/DATUM/UHRZEIT FÜLLEN
                    MOVE T-USER            TO CFMSER OF PCONFIG-P
                    MOVE T-TERM            TO CFMBS  OF PCONFIG-P
                    MOVE PGM-WRK           TO CFMPGM OF PCONFIG-P
                    MOVE DATE-8            TO CFMDTA OF PCONFIG-P
                    MOVE TIME-6            TO CFMTIA OF PCONFIG-P
     ‚*             AKTUALISIEREN
                    REWRITE PCONFIG-P
           ELSE
     ‚*             KEINE NEUANLAGE? DANN SIND WIR HIER FALSCH
                    IF   NEUANLAGE = ZEROES
                         MOVE "1" TO RET-CODE
                         GO TO UPD-KEY-90
                    END-IF
     ‚*             DATEN FÜLLEN
                    MOVE CORR PCONFIGF OF PCONFIG-WRK TO
                              PCONFIGF OF PCONFIG-P
     ‚*             USER/DATUM/UHRZEIT FÜLLEN
                    MOVE T-USER            TO CFAUSR OF PCONFIG-P
                    MOVE T-TERM            TO CFABS  OF PCONFIG-P
                    MOVE PGM-WRK           TO CFAPGM OF PCONFIG-P
                    MOVE DATE-8            TO CFADTA OF PCONFIG-P
                    MOVE TIME-6            TO CFATIA OF PCONFIG-P
     ‚*             SCHREIBEN DES SATZES
                    WRITE   PCONFIG-P
                    MOVE ZEROES            TO NEUANLAGE
           END-IF.

       UPD-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN EINES SCHLÜSSELEINTRAGS
     ‚*--------------------------------------------------------------
       DELETE-KEY SECTION.
       DEL-KEY-00.

     ‚* LÖSCHEN DER PCONFIG
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCONFIG-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PCONFIG"   TO FILE-WRK
                    MOVE PCONFIG-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       DEL-KEY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* A100: AUTOMATISCHES ANMELDEN
     ‚*--------------------------------------------------------------
       EDIT-A100 SECTION.
       EDT-A10-00.

           INITIALIZE FORMAT10.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT10.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT10.
           MOVE     "CFP0001*10"      TO SUBUEB  OF FORMAT10.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLA100"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT10.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT10
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-A10-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT10.

           MOVE     CORR INXX       TO  FORMAT10-O-INDIC.
           MOVE     "FMT10"         TO  FMT-WRK.
           MOVE     FORMAT10        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT10-I.
           MOVE     CORR FORMAT10-I TO FORMAT10.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT10-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-A10-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT10-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-A10-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT10 = SPACES
                    GO TO EDT-A10-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT10 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-A10-10
           END-IF.

       EDT-A10-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* A200: PROGRAMMAUFRUFE
     ‚*--------------------------------------------------------------
       EDIT-A200 SECTION.
       EDT-A20-00.

           INITIALIZE FORMAT13.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT13.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT13.
           MOVE     "CFP0001*13"      TO SUBUEB  OF FORMAT13.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLA200"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT13.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT13
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-A20-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT13.

           MOVE     CORR INXX       TO  FORMAT13-O-INDIC.
           MOVE     "FMT13"         TO  FMT-WRK.
           MOVE     FORMAT13        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT13-I.
           MOVE     CORR FORMAT13-I TO FORMAT13.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT13-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-A20-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT13-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-A20-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT13 = SPACES
                    GO TO EDT-A20-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT13 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-A20-10
           END-IF.

       EDT-A20-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* A202: PFADANGABE FÜR STRPCCMD
     ‚*--------------------------------------------------------------
       EDIT-A202 SECTION.
       EDT-A22-00.

           INITIALIZE FORMAT21.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT21.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT21.
           MOVE     "CFP0001*21"      TO SUBUEB  OF FORMAT21.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLA202"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT21.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT21
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-A22-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT21.

           MOVE     CORR INXX       TO  FORMAT21-O-INDIC.
           MOVE     "FMT21"         TO  FMT-WRK.
           MOVE     FORMAT21        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT21-I.
           MOVE     CORR FORMAT21-I TO FORMAT21.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT21-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-A22-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT21-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-A22-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT21 = SPACES
                    GO TO EDT-A22-10
           END-IF.

     ‚* IST DER PFAD LEER?
           IF       CFBG01 OF FORMAT21 = SPACES
                    GO TO EDT-A22-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT21 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-A22-10
           END-IF.

       EDT-A22-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* A300: SPRACHEINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-A300 SECTION.
       EDT-A30-00.

           INITIALIZE FORMAT23.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT23.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT23.
           MOVE     "CFP0001*23"      TO SUBUEB  OF FORMAT23.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLA300"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT23.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT23
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-A30-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT23.

           MOVE     CORR INXX       TO  FORMAT23-O-INDIC.
           MOVE     "FMT23"         TO  FMT-WRK.
           MOVE     FORMAT23        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT23-I.
           MOVE     CORR FORMAT23-I TO FORMAT23.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT23-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-A30-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT23-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-A30-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT23 = SPACES
                    GO TO EDT-A30-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT23 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-A30-10
           END-IF.

       EDT-A30-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* A400: MAILKONFIGURATION
     ‚*--------------------------------------------------------------
       EDIT-A400 SECTION.
       EDT-A40-00.

           INITIALIZE FORMAT26.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT26.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT26.
           MOVE     "CFP0001*26"      TO SUBUEB  OF FORMAT26.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLA400"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT26.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT26
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-A40-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT26.

           MOVE     CORR INXX       TO  FORMAT26-O-INDIC.
           MOVE     "FMT26"         TO  FMT-WRK.
           MOVE     FORMAT26        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT26-I.
           MOVE     CORR FORMAT26-I TO FORMAT26.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT26-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-A40-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT26-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-A40-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT26 = SPACES
                    GO TO EDT-A40-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT26 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-A40-10
           END-IF.

       EDT-A40-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H000: GENERELLE EINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H000 SECTION.
       EDT-H00-00.

           INITIALIZE FORMAT12.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT12.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT12.
           MOVE     "CFP0001*12"      TO SUBUEB  OF FORMAT12.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH000"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT12.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT12
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H00-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT12.

           MOVE     CORR INXX       TO  FORMAT12-O-INDIC.
           MOVE     "FMT12"         TO  FMT-WRK.
           MOVE     FORMAT12        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT12-I.
           MOVE     CORR FORMAT12-I TO FORMAT12.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT12-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H00-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT12-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H00-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT12 = SPACES
                    GO TO EDT-H00-10
           END-IF.

     ‚* ES DÜRFEN NUR USERNAMES EINGETRAGEN WERDEN, DAHER PRÜFEN
     ‚* OB ES DEN USER GIBT
           INITIALIZE CHKOBJ-REC.
           MOVE     CFKEY  OF FORMAT12 TO OBJ     OF CHKOBJ-REC.
           MOVE     "QSYS"             TO LIB     OF CHKOBJ-REC.
           MOVE     "*USRPRF"          TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    GO TO EDT-H00-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT12 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H00-10
           END-IF.

       EDT-H00-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H050: USER-EINSTELLUNGEN (PBIRTH)
     ‚*--------------------------------------------------------------
       EDIT-H050 SECTION.
       EDT-H05-00.

           INITIALIZE FORMAT15.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT15.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT15.
           MOVE     "CFP0001*15"      TO SUBUEB  OF FORMAT15.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH050"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT15.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT15
                    MOVE CFIN04 OF PCONFIG-P TO DT_GEB OF FORMAT15
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

     ‚* NOT LONGER ALLOWED...
           IF       CFKEY  OF FORMAT15 NOT = T-USER
            AND     T-USER             NOT = "APG"
                    GO TO EDT-H05-90
           END-IF.

     ‚* TELEFONNUMMERN STANDARDISIEREN
           IF       CFIN23 OF FORMAT15 NOT = SPACES
                    MOVE CFIN23 OF FORMAT15  TO TELEPHONE-WRK
                    CALL "HLP0004" USING TELEPHONE-WRK
                    END-CALL
                    MOVE TELEPHONE-WRK       TO NORMHM OF FORMAT15
           END-IF.
           IF       CFIN24 OF FORMAT15 NOT = SPACES
                    MOVE CFIN24 OF FORMAT15  TO TELEPHONE-WRK
                    CALL "HLP0004" USING TELEPHONE-WRK
                    END-CALL
                    MOVE TELEPHONE-WRK       TO NORMMO OF FORMAT15
           END-IF.
           IF       CFIN25 OF FORMAT15 NOT = SPACES
                    MOVE CFIN25 OF FORMAT15  TO TELEPHONE-WRK
                    CALL "HLP0004" USING TELEPHONE-WRK
                    END-CALL
                    MOVE TELEPHONE-WRK       TO NORMBB OF FORMAT15
           END-IF.

       EDT-H05-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT15.

           MOVE     CORR INXX       TO  FORMAT15-O-INDIC.
           MOVE     "FMT15"         TO  FMT-WRK.
           MOVE     FORMAT15        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT15-I.
           MOVE     CORR FORMAT15-I  TO FORMAT15.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT15-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H05-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT15-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H05-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT15 = SPACES
                    GO TO EDT-H05-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT15 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H05-10
           END-IF.

     ‚* FOLGESEITE ANZEIGEN
           PERFORM  EDIT-H050-2.
           IF       F12 = "1"
                    GO TO EDT-H05-00
           END-IF.

       EDT-H05-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H050: USER-EINSTELLUNGEN (PBIRTH)
     ‚*--------------------------------------------------------------
       EDIT-H050-2 SECTION.
       ED2-H05-00.

           INITIALIZE FORMAT15A.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT15A.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT15A.
           MOVE     "CFP0001*15A"     TO SUBUEB  OF FORMAT15A.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH050"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT15A.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT15A
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       ED2-H05-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT15A.

           MOVE     CORR INXX       TO  FORMAT15A-O-INDIC.
           MOVE     "FMT15A"        TO  FMT-WRK.
           MOVE     FORMAT15A       TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK        TO FORMAT15A-I.
           MOVE     CORR FORMAT15A-I TO FORMAT15A.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT15A-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ED2-H05-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT15A-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO ED2-H05-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT15A = SPACES
                    GO TO ED2-H05-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT15A TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO ED2-H05-10
           END-IF.

     ‚* FOLGESEITE ANZEIGEN
           PERFORM  EDIT-H050-3.
           IF       F12 = "1"
                    GO TO ED2-H05-00
           END-IF.

       ED2-H05-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H050: USER-EINSTELLUNGEN (PBIRTH)
     ‚*--------------------------------------------------------------
       EDIT-H050-3 SECTION.
       ED3-H05-00.

           INITIALIZE FORMAT15B.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT15B.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT15B.
           MOVE     "CFP0001*15B"     TO SUBUEB  OF FORMAT15B.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH050"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT15B.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT15B
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       ED3-H05-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT15B.

           MOVE     CORR INXX       TO  FORMAT15B-O-INDIC.
           MOVE     "FMT15B"        TO  FMT-WRK.
           MOVE     FORMAT15B       TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK        TO FORMAT15B-I.
           MOVE     CORR FORMAT15B-I TO FORMAT15B.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT15B-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ED3-H05-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT15B-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO ED3-H05-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT15B = SPACES
                    GO TO ED3-H05-10
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR DIE SPRACHE
           IF       CFDAT7 OF FORMAT15B(1:1) = "?"
                    MOVE "A300" TO CFID-WRK
                    PERFORM SHOW-KEYS
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO CFDAT7 OF FORMAT15B
                    END-IF
                    MOVE "CFDAT7" TO DSP-CUR-FLD
                    GO TO ED3-H05-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT15B TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO ED3-H05-10
           END-IF.

     ‚* FOLGESEITE ANZEIGEN
           PERFORM  EDIT-H050-4.
           IF       F12 = "1"
                    GO TO ED3-H05-00
           END-IF.

       ED3-H05-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H050: USER-EINSTELLUNGEN (PBIRTH)
     ‚*--------------------------------------------------------------
       EDIT-H050-4 SECTION.
       ED4-H05-00.

           INITIALIZE FORMAT15C.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT15C.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT15C.
           MOVE     "CFP0001*15C"     TO SUBUEB  OF FORMAT15C.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH050"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT15C.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT15C
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       ED4-H05-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT15C.

           MOVE     CORR INXX       TO  FORMAT15C-O-INDIC.
           MOVE     "FMT15C"        TO  FMT-WRK.
           MOVE     FORMAT15C       TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK        TO FORMAT15C-I.
           MOVE     CORR FORMAT15C-I TO FORMAT15C.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT15C-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO ED4-H05-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT15C-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO ED4-H05-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT15C = SPACES
                    GO TO ED4-H05-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT15C TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO ED4-H05-10
           END-IF.

       ED4-H05-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H051: GRUPPENEINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H051 SECTION.
       EDT-H51-00.

           INITIALIZE FORMAT20.
           INITIALIZE NEUANLAGE-WRK.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT20.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT20.
           MOVE     "CFP0001*20"      TO SUBUEB  OF FORMAT20.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH051"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT20.

       EDT-H51-05.
     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT20
                    MOVE AN  TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    IF   CFKEY OF FORMAT20 = "BERICHTSHEFT"
                         MOVE AN TO IN42 OF INXX
                    END-IF
                    IF   NEUANLAGE-WRK = 1
                         MOVE DATE-8(1:4) TO CFNM04 OF FORMAT20
                         MOVE T-USER      TO CFDAT1  OF FORMAT20
                         MOVE ZEROES TO NEUANLAGE-WRK
                    END-IF
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
                    MOVE AUS TO IN42 OF INXX
                    MOVE 1   TO NEUANLAGE-WRK
           END-IF.

       EDT-H51-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT20.

           MOVE     CORR INXX       TO  FORMAT20-O-INDIC.
           MOVE     "FMT20"         TO  FMT-WRK.
           MOVE     FORMAT20        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT20-I.
           MOVE     CORR FORMAT20-I TO FORMAT20.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT20-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H51-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT20-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H51-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT20 = SPACES
                    GO TO EDT-H51-10
           END-IF.

     ‚* TEXTFELD MIT BESCHREIBUNG FÜLLEN
           IF       CFKEY OF FORMAT20 = "BERICHTSHEFT"
                    STRING   FUNCTION TRIM(CFDAT1 OF FORMAT20)
                                                DELIMITED BY SIZE
                         " "                    DELIMITED BY SIZE
                         CFNM04 OF FORMAT20     DELIMITED BY SIZE
                         INTO CFTXT1 OF FORMAT20
                    END-STRING
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT20 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H51-10
           END-IF.
           IF       CFKEY OF FORMAT20 = "BERICHTSHEFT"
            AND     NEUANLAGE-WRK = 1
                    GO TO EDT-H51-05
           END-IF.

       EDT-H51-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H052: RAUMEINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H052 SECTION.
       EDT-H52-00.

           INITIALIZE FORMAT16.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT16.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT16.
           MOVE     "CFP0001*16"      TO SUBUEB  OF FORMAT16.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH052"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT16.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT16
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H52-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT16.

           MOVE     CORR INXX       TO  FORMAT16-O-INDIC.
           MOVE     "FMT16"         TO  FMT-WRK.
           MOVE     FORMAT16        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT16-I.
           MOVE     CORR FORMAT16-I TO FORMAT16.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT16-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H52-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT16-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H52-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT16 = SPACES
                    GO TO EDT-H52-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT16 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H52-10
           END-IF.

       EDT-H52-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H053: TELEFONEINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H053 SECTION.
       EDT-H53-00.

           INITIALIZE FORMAT24.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT24.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT24.
           MOVE     "CFP0001*24"      TO SUBUEB  OF FORMAT24.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH053"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT24.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT24
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H53-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT24.

           MOVE     CORR INXX       TO  FORMAT24-O-INDIC.
           MOVE     "FMT24"         TO  FMT-WRK.
           MOVE     FORMAT24        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT24-I.
           MOVE     CORR FORMAT24-I TO FORMAT24.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT24-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H53-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT24-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H53-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT24 = SPACES
                    GO TO EDT-H53-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT24 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H53-10
           END-IF.

       EDT-H53-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H100: HERMES-VORBELEGUNG
     ‚*--------------------------------------------------------------
       EDIT-H100 SECTION.
       EDT-H10-00.

           INITIALIZE FORMAT11.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT11.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT11.
           MOVE     "CFP0001*11"      TO SUBUEB  OF FORMAT11.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH100"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT11.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT11
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H10-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT11.

           MOVE     CORR INXX       TO  FORMAT11-O-INDIC.
           MOVE     "FMT11"         TO  FMT-WRK.
           MOVE     FORMAT11        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT11-I.
           MOVE     CORR FORMAT11-I TO FORMAT11.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT11-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H10-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT11-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H10-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT11 = SPACES
                    GO TO EDT-H10-10
           END-IF.

     ‚* ES DÜRFEN NUR USERNAMES EINGETRAGEN WERDEN, DAHER PRÜFEN
     ‚* OB ES DEN USER GIBT
           INITIALIZE CHKOBJ-REC.
           MOVE     CFKEY  OF FORMAT11 TO OBJ     OF CHKOBJ-REC.
           MOVE     "QSYS"             TO LIB     OF CHKOBJ-REC.
           MOVE     "*USRPRF"          TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    GO TO EDT-H10-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT11 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H10-10
           END-IF.

       EDT-H10-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H101: SEU-EINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H101 SECTION.
       EDT-H11-00.

           INITIALIZE FORMAT14.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT14.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT14.
           MOVE     "CFP0001*14"      TO SUBUEB  OF FORMAT14.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH101"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT14.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT14
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H11-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT14.

           MOVE     CORR INXX       TO  FORMAT14-O-INDIC.
           MOVE     "FMT14"         TO  FMT-WRK.
           MOVE     FORMAT14        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT14-I.
           MOVE     CORR FORMAT14-I TO FORMAT14.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT14-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H11-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT14-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H11-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT14 = SPACES
                    GO TO EDT-H11-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT14 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H11-10
           END-IF.

       EDT-H11-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H102: TODO-KATEGORIEN
     ‚*--------------------------------------------------------------
       EDIT-H102 SECTION.
       EDT-H12-00.

           INITIALIZE FORMAT25.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT25.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT25.
           MOVE     "CFP0001*25"      TO SUBUEB  OF FORMAT25.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH102"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT25.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT25
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H12-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT25.

           MOVE     CORR INXX       TO  FORMAT25-O-INDIC.
           MOVE     "FMT25"         TO  FMT-WRK.
           MOVE     FORMAT25        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT25-I.
           MOVE     CORR FORMAT25-I TO FORMAT25.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT25-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H12-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT25-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H12-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT25 = SPACES
                    GO TO EDT-H12-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT25 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H12-10
           END-IF.

       EDT-H12-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H102.1: TODO-FEHLERNACHRICHTEN
     ‚*--------------------------------------------------------------
       EDIT-H102-1 SECTION.
       EDT-121-00.

           INITIALIZE FORMAT33.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT33.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT33.
           MOVE     "CFP0001*33"      TO SUBUEB  OF FORMAT33.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH102.1"        TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT33.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT33
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-121-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT33.

           MOVE     CORR INXX       TO  FORMAT33-O-INDIC.
           MOVE     "FMT33"         TO  FMT-WRK.
           MOVE     FORMAT33        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT33-I.
           MOVE     CORR FORMAT33-I TO FORMAT33.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT33-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-121-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT33-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-121-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT33 = SPACES
                    GO TO EDT-121-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT33 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-121-10
           END-IF.

       EDT-121-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H102.2: TODO-EINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H102-2 SECTION.
       EDT-122-00.

           INITIALIZE FORMAT36.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT36.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT36.
           MOVE     "CFP0001*36"      TO SUBUEB  OF FORMAT36.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH102.2"        TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT36.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT36
     ‚*             DIE FARBEINSTELLUNGEN FÜR PROZENT UND PRIO SIND
     ‚*             IN EINEM LANGEN FELD GESPEICHERT
                    IF   CFIN21 OF FORMAT36(1:24) IS NUMERIC
     ‚*                  PROZENT
                         MOVE CFIN21 OF FORMAT36( 1:2) TO
                              PRZRD1 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36( 3:2) TO
                              PRZRD2 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36( 5:2) TO
                              PRZYW1 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36( 7:2) TO
                              PRZYW2 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36( 9:2) TO
                              PRZGN1 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36(11:2) TO
                              PRZGN2 OF FORMAT36
     ‚*                  PRIORITÄT
                         MOVE CFIN21 OF FORMAT36(13:2) TO
                              PRIRD1 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36(15:2) TO
                              PRIRD2 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36(17:2) TO
                              PRIYW1 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36(19:2) TO
                              PRIYW2 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36(21:2) TO
                              PRIGN1 OF FORMAT36
                         MOVE CFIN21 OF FORMAT36(23:2) TO
                              PRIGN2 OF FORMAT36
                    END-IF
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-121-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT36.

           MOVE     CORR INXX       TO  FORMAT36-O-INDIC.
           MOVE     "FMT36"         TO  FMT-WRK.
           MOVE     FORMAT36        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT36-I.
           MOVE     CORR FORMAT36-I TO FORMAT36.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT36-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-121-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT36-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-121-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT36 = SPACES
                    GO TO EDT-121-10
           END-IF.

     ‚* ÜBERNEHMEN DER FELDER IN DEN RECORD
           MOVE     PRZRD1 OF FORMAT36 TO CFIN21 OF FORMAT36( 1:2).
           MOVE     PRZRD2 OF FORMAT36 TO CFIN21 OF FORMAT36( 3:2).
           MOVE     PRZYW1 OF FORMAT36 TO CFIN21 OF FORMAT36( 5:2).
           MOVE     PRZYW2 OF FORMAT36 TO CFIN21 OF FORMAT36( 7:2).
           MOVE     PRZGN1 OF FORMAT36 TO CFIN21 OF FORMAT36( 9:2).
           MOVE     PRZGN2 OF FORMAT36 TO CFIN21 OF FORMAT36(11:2).
           MOVE     PRZRD1 OF FORMAT36 TO CFIN21 OF FORMAT36(13:2).
           MOVE     PRZRD2 OF FORMAT36 TO CFIN21 OF FORMAT36(15:2).
           MOVE     PRZYW1 OF FORMAT36 TO CFIN21 OF FORMAT36(17:2).
           MOVE     PRZYW2 OF FORMAT36 TO CFIN21 OF FORMAT36(19:2).
           MOVE     PRZGN1 OF FORMAT36 TO CFIN21 OF FORMAT36(21:2).
           MOVE     PRZGN2 OF FORMAT36 TO CFIN21 OF FORMAT36(23:2).

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT36 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-121-10
           END-IF.

       EDT-121-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* H103: AZUBIVERWALTUNG-EINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-H103 SECTION.
       EDT-H13-00.

           INITIALIZE FORMAT32.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT32.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT32.
           MOVE     "CFP0001*32"      TO SUBUEB  OF FORMAT32.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLH103"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT32.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT32
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-H13-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT32.

           MOVE     CORR INXX       TO  FORMAT32-O-INDIC.
           MOVE     "FMT32"         TO  FMT-WRK.
           MOVE     FORMAT32        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT32-I.
           MOVE     CORR FORMAT32-I TO FORMAT32.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT32-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-H13-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT32-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-H13-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT32 = SPACES
                    GO TO EDT-H13-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT32 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-H13-10
           END-IF.

       EDT-H13-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* I101: PVEREIN-IMPORTS
     ‚*--------------------------------------------------------------
       EDIT-I101 SECTION.
       EDT-I11-00.

           INITIALIZE FORMAT19.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT19.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT19.
           MOVE     "CFP0001*19"      TO SUBUEB  OF FORMAT19.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLI101"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT19.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT19
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-I11-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT19.

           MOVE     CORR INXX       TO  FORMAT19-O-INDIC.
           MOVE     "FMT19"         TO  FMT-WRK.
           MOVE     FORMAT19        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT19-I.
           MOVE     CORR FORMAT19-I TO FORMAT19.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT19-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-I11-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT19-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-I11-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT19 = SPACES
                    GO TO EDT-I11-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT19 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-I11-10
           END-IF.

       EDT-I11-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K100: KINO-ANMELDUNG
     ‚*--------------------------------------------------------------
       EDIT-K100 SECTION.
       EDT-K10-00.

           INITIALIZE FORMAT30.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT30.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT30.
           MOVE     "CFP0001*30"      TO SUBUEB  OF FORMAT30.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK100"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT30.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT30
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-K10-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT30.

           MOVE     CORR INXX       TO  FORMAT30-O-INDIC.
           MOVE     "FMT30"         TO  FMT-WRK.
           MOVE     FORMAT30        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT30-I.
           MOVE     CORR FORMAT30-I TO FORMAT30.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT30-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-K10-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT30-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-K10-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT30 = SPACES
                    GO TO EDT-K10-10
           END-IF.

     ‚* ES DÜRFEN NUR USERNAMES EINGETRAGEN WERDEN, DAHER PRÜFEN
     ‚* OB ES DEN USER GIBT
           INITIALIZE CHKOBJ-REC.
           MOVE     CFKEY  OF FORMAT30 TO OBJ     OF CHKOBJ-REC.
           MOVE     "QSYS"             TO LIB     OF CHKOBJ-REC.
           MOVE     "*USRPRF"          TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    GO TO EDT-K10-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT30 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-K10-10
           END-IF.

       EDT-K10-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K110: DIENSTINFORMATIONEN
     ‚*--------------------------------------------------------------
       EDIT-K110 SECTION.
       EDT-K11-00.

           INITIALIZE FORMAT31.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT31.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT31.
           MOVE     "CFP0001*31"      TO SUBUEB  OF FORMAT31.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK110"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT31.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT31
                    MOVE AN  TO IN41 OF INXX
     ‚*             BEI USER KEYS DIE EINSTELLUNG AUSBLENDEN
                    IF   CFKEY2 OF PCONFIG-P NOT = SPACES
                         MOVE AUS TO IN40 OF INXX
                    ELSE
                         MOVE AN  TO IN40 OF INXX
                    END-IF
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
                    MOVE AN  TO IN40 OF INXX
           END-IF.

       EDT-K11-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT31.

           MOVE     CORR INXX       TO  FORMAT31-O-INDIC.
           MOVE     "FMT31"         TO  FMT-WRK.
           MOVE     FORMAT31        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT31-I.
           MOVE     CORR FORMAT31-I TO FORMAT31.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT31-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-K11-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT31-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-K11-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT31 = SPACES
                    GO TO EDT-K11-10
           END-IF.

     ‚* DARF NICHT EINGESTELLT WERDEN BEI USEREINSTELLUNGEN
           IF       CFKEY2 OF FORMAT31 NOT = SPACES
            AND     CFFK02 OF FORMAT31 NOT = ZEROES
                    GO TO EDT-K11-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT31 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-K11-10
           END-IF.

       EDT-K11-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K111: MASCHINENSTATUS
     ‚*--------------------------------------------------------------
       EDIT-K111 SECTION.
       EDK-111-00.

           INITIALIZE FORMAT22.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT22.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT22.
           MOVE     "CFP0001*22"      TO SUBUEB  OF FORMAT22.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK111"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT22.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT22
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDK-111-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT22.

           MOVE     CORR INXX       TO  FORMAT22-O-INDIC.
           MOVE     "FMT22"         TO  FMT-WRK.
           MOVE     FORMAT22        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT22-I.
           MOVE     CORR FORMAT22-I TO FORMAT22.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT22-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDK-111-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT22-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDK-111-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT22 = SPACES
                    GO TO EDK-111-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT22 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDK-111-10
           END-IF.

       EDK-111-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K120: BESTELLTYPEN
     ‚*--------------------------------------------------------------
       EDIT-K120 SECTION.
       EDT-K12-00.

           INITIALIZE FORMAT18.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT18.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT18.
           MOVE     "CFP0001*18"      TO SUBUEB  OF FORMAT18.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK120"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT18.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT18
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-K12-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT18.

           MOVE     CORR INXX       TO  FORMAT17-O-INDIC.
           MOVE     "FMT18"         TO  FMT-WRK.
           MOVE     FORMAT18        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT18-I.
           MOVE     CORR FORMAT18-I TO FORMAT18.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT18-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-K12-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT18-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-K12-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT18 = SPACES
                    GO TO EDT-K12-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT18 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-K12-10
           END-IF.

       EDT-K12-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K130: ARTIKELNUMMERN
     ‚*--------------------------------------------------------------
       EDIT-K130 SECTION.
       EDT-K13-00.

           INITIALIZE FORMAT17.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT17.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT17.
           MOVE     "CFP0001*17"      TO SUBUEB  OF FORMAT17.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK130"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT17.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT17
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-K13-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT17.

           MOVE     CORR INXX       TO  FORMAT17-O-INDIC.
           MOVE     "FMT17"         TO  FMT-WRK.
           MOVE     FORMAT17        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT17-I.
           MOVE     CORR FORMAT17-I TO FORMAT17.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT17-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-K13-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT17-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-K13-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT17 = SPACES
                    GO TO EDT-K13-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT17 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-K13-10
           END-IF.

       EDT-K13-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K200: SPEISEPLAN
     ‚*--------------------------------------------------------------
       EDIT-K200 SECTION.
       EDT-K20-00.

           INITIALIZE FORMAT34.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT34.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT34.
           MOVE     "CFP0001*34"      TO SUBUEB  OF FORMAT34.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK200"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT34.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT34
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-K20-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT34.

           MOVE     CORR INXX       TO  FORMAT34-O-INDIC.
           MOVE     "FMT34"         TO  FMT-WRK.
           MOVE     FORMAT34        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT34-I.
           MOVE     CORR FORMAT34-I TO FORMAT34.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT34-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-K20-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT34-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-K20-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT34 = SPACES
                    GO TO EDT-K20-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT34 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-K20-10
           END-IF.

       EDT-K20-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* K201: RESTAURANS
     ‚*--------------------------------------------------------------
       EDIT-K201 SECTION.
       EDT-K21-00.

           INITIALIZE FORMAT35.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT35.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT35.
           MOVE     "CFP0001*35"      TO SUBUEB  OF FORMAT35.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLK201"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT35.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT35
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-K21-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT35.

           MOVE     CORR INXX       TO  FORMAT35-O-INDIC.
           MOVE     "FMT35"         TO  FMT-WRK.
           MOVE     FORMAT35        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT35-I.
           MOVE     CORR FORMAT35-I TO FORMAT35.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT35-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-K21-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT35-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-K21-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT35 = SPACES
                    GO TO EDT-K21-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT35 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-K21-10
           END-IF.

       EDT-K21-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* V100: AZUBI-EINSTELLUNGEN
     ‚*--------------------------------------------------------------
       EDIT-V100 SECTION.
       EDT-V10-00.

           INITIALIZE FORMAT27.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT27.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT27.
           MOVE     "CFP0001*27"      TO SUBUEB  OF FORMAT27.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLV100"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT27.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT27
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-V10-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT27.

           MOVE     CORR INXX       TO  FORMAT27-O-INDIC.
           MOVE     "FMT27"         TO  FMT-WRK.
           MOVE     FORMAT27        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT27-I.
           MOVE     CORR FORMAT27-I TO FORMAT27.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT27-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-V10-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT27-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-V10-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT27 = SPACES
                    MOVE    "CFKEY"   TO DSP-ERR-FLD
                    MOVE    "ERR0006" TO DSP-ERR-ID
                    PERFORM COPY-ADD-ERR-TAB
                    GO TO EDT-V10-10
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR DIE SCHULE
           IF       CFIN01 OF FORMAT27(1:1) = "?"
                    MOVE "V110" TO CFID-WRK
                    PERFORM SHOW-KEYS
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO CFIN01 OF FORMAT27
                    END-IF
                    MOVE "CFIN01" TO DSP-CUR-FLD
                    GO TO EDT-V10-10
           END-IF.

     ‚* FRAGEZEICHENFUNKTION FÜR DIE BERUFSBEZEICHNUNG
           IF       CFIN03 OF FORMAT27(1:1) = "?"
                    MOVE "V112" TO CFID-WRK
                    PERFORM SHOW-KEYS
                    IF   CFKEY-WRK NOT = SPACES
                         MOVE CFKEY-WRK TO CFIN03 OF FORMAT27
                    END-IF
                    MOVE "CFIN03" TO DSP-CUR-FLD
                    GO TO EDT-V10-10
           END-IF.

           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "V110"             TO CFID   OF CFG-CPY.
           MOVE     CFIN01 OF FORMAT27 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE    "CFIN01"  TO DSP-ERR-FLD
                    MOVE    "ERR0006" TO DSP-ERR-ID
                    PERFORM COPY-ADD-ERR-TAB
                    GO TO EDT-V10-10
           END-IF.

           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "V112"             TO CFID   OF CFG-CPY.
           MOVE     CFIN03 OF FORMAT27 TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE    "CFIN03"  TO DSP-ERR-FLD
                    MOVE    "ERR0006" TO DSP-ERR-ID
                    PERFORM COPY-ADD-ERR-TAB
                    GO TO EDT-V10-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT27 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-V10-10
           END-IF.

       EDT-V10-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* V110: BERUFSSCHULEN
     ‚*--------------------------------------------------------------
       EDIT-V110 SECTION.
       EDT-V11-00.

           INITIALIZE FORMAT28.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT28.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT28.
           MOVE     "CFP0001*28"      TO SUBUEB  OF FORMAT28.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLV110"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT28.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT28
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-V11-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT28.

           MOVE     CORR INXX       TO  FORMAT28-O-INDIC.
           MOVE     "FMT28"         TO  FMT-WRK.
           MOVE     FORMAT28        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT28-I.
           MOVE     CORR FORMAT28-I TO FORMAT28.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT28-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-V11-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT28-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-V11-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT28 = SPACES
                    GO TO EDT-V11-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT28 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-V11-10
           END-IF.

       EDT-V11-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* V112: BERUFSSCHULEN
     ‚*--------------------------------------------------------------
       EDIT-V112 SECTION.
       EDT-V12-00.

           INITIALIZE FORMAT29.
           MOVE     CFID   OF FORMAT1 TO CFID   OF FORMAT29.
           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           MOVE     "CFP0000"         TO MAINUEB OF FORMAT29.
           MOVE     "CFP0001*29"      TO SUBUEB  OF FORMAT29.
     ‚* SCHLÜSSELBESCHREIBUNG
           MOVE     "SLV112"          TO CPY-TXID.
           MOVE     SPRACHE-WRK       TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT        TO KEYTXT  OF FORMAT29.

     ‚* FORMAT FÜLLEN
           IF       NEUANLAGE = ZEROES
                    MOVE CORR PCONFIGF OF PCONFIG-P TO FORMAT29
                    MOVE AN  TO IN41 OF INXX
           ELSE
                    INITIALIZE PCONFIGF OF PCONFIG-P
                    MOVE AUS TO IN41 OF INXX
           END-IF.

       EDT-V12-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT29.

           MOVE     CORR INXX       TO  FORMAT29-O-INDIC.
           MOVE     "FMT29"         TO  FMT-WRK.
           MOVE     FORMAT29        TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT29-I.
           MOVE     CORR FORMAT29-I TO FORMAT29.
           MOVE     AUS TO INXX.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT29-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO EDT-V12-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT29-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO EDT-V12-90
           END-IF.

     ‚* IST DER KEY LEER?
           IF       CFKEY  OF FORMAT29 = SPACES
                    GO TO EDT-V12-10
           END-IF.

     ‚* EINTRAG JETZT ERSTELLEN/AKTUALISIEREN
           INITIALIZE PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR PCONFIGF OF PCONFIG-P TO
                         PCONFIGF OF PCONFIG-WRK.
           MOVE     CORR FORMAT29 TO PCONFIGF OF PCONFIG-WRK.
           PERFORM  UPDATE-KEY.
           IF       RET-CODE NOT = SPACES
                    GO TO EDT-V12-10
           END-IF.

       EDT-V12-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SCHLÜSSELS IM SUBFILE ÜBER PCFGFFD)
     ‚*--------------------------------------------------------------
       SHOW-KEY-RECORD SECTION.
       SHW-KRC-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     CFID   OF FORMAT2 TO CFID   OF PCONFIG-P.
           MOVE     CFKEY  OF FORMAT2 TO CFKEY  OF PCONFIG-P.
           MOVE     CFKEY2 OF FORMAT2 TO CFKEY2 OF PCONFIG-P.
           MOVE     CFKEY3 OF FORMAT2 TO CFKEY3 OF PCONFIG-P.
           MOVE     CFKEY4 OF FORMAT2 TO CFKEY4 OF PCONFIG-P.
           READ     PCONFIG-DP WITH NO LOCK.
           IF       FILE-STATUS = ZEROES
                    MOVE "PCONFIG" TO FILE-WRK
                    MOVE SPACES    TO FORMAT-DUMMY
                    MOVE PCONFIG-P TO RECORD-WRK
                    CALL "CFP0027" USING RECORD-WRK
                                         CONST-LIB
                                         FILE-WRK
                                         FORMAT-DUMMY
                                         F3
                    END-CALL
                    IF   F3 = "1"
                         GO TO SHW-KRC-90
                    END-IF
           END-IF.

       SHW-KRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUF CFP0008 ZUR ANZEIGE DER KEYS
     ‚*--------------------------------------------------------------
       SHOW-KEYS SECTION.
       SHW-KEY-00.

           INITIALIZE F12.
     ‚* ANZEIGEN DER SCHLÜSSEL
           MOVE     SPACES TO CFKEY-WRK.
           MOVE     SPACES TO CFKEY2-WRK.
           MOVE     SPACES TO CFKEY3-WRK.
           MOVE     SPACES TO CFKEY4-WRK.
           MOVE     SPACES TO F12.
           CALL     "CFP0008" USING CFID-WRK
                                    CFKEY-WRK
                                    CFKEY2-WRK
                                    CFKEY3-WRK
                                    CFKEY4-WRK
                                    F12
           END-CALL.

       SHW-KEY-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
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

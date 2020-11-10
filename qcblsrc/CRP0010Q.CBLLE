       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0010.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE ANZEIGEN/EDITIEREN                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGEN UND ÄNDERN EINER SOURCE (SEU)       *
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
                  ASSIGN       TO  WORKSTATION-CRD0010DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI SYNTAX-HIGHLIGHTING
     ‚*-------------------------------------------------------------
           SELECT PCHKHGH-DL
                  ASSIGN       TO  DATABASE-PCHKHGHL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI CHKSRC-EINTRÄGE
     ‚*-------------------------------------------------------------
           SELECT PCHKSRC-DL
                  ASSIGN       TO  DATABASE-PCHKSRCL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚*    DATEI QSETSRC (NUR ZUM AUSLESEN)
     ‚*-------------------------------------------------------------
           SELECT QSETSRC-DP
                  ASSIGN       TO  DATABASE-QSETSRC
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI PCFGFFD NACH LIB/FILE
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DL
                  ASSIGN       TO  DATABASE-PCFGFFDL1
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
           COPY DDS-ALL-FORMATS OF CRD0010DE.
      /
     ‚*--- SYNTAXHIGHLIGHTING
       FD  PCHKHGH-DL
           LABEL RECORDS ARE STANDARD.
       01  PCHKHGH-L.
           COPY DDS-ALL-FORMATS OF PCHKHGHL1.
      /
     ‚*--- CHKSRC
       FD  PCHKSRC-DL
           LABEL RECORDS ARE STANDARD.
       01  PCHKSRC-L.
           COPY DDS-ALL-FORMATS OF PCHKSRCL1.
      /
     ‚*--- PCFGFFD NACH LIB/FILE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL1.
      /
     ‚*----DATEI QCBLSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.
     ‚* INDIK. FÜR ANZEIGE OB INFOS VORHANDEN SIND
        05 IN40                          PIC 1.
     ‚* INDIK. FÜR ANZEIGE OB DATEI GEÄNDERT
        05 IN41                          PIC 1.

       01  OUTXX.
     ‚* INDIK. FÜR DIE FELDÄNDERUNG (IN50 = ZEILE1, IN69 = ZEILE20)
        05 IN50                          PIC 1.
        05 IN51                          PIC 1.
        05 IN52                          PIC 1.
        05 IN53                          PIC 1.
        05 IN54                          PIC 1.
        05 IN55                          PIC 1.
        05 IN56                          PIC 1.
        05 IN57                          PIC 1.
        05 IN58                          PIC 1.
        05 IN59                          PIC 1.
        05 IN60                          PIC 1.
        05 IN61                          PIC 1.
        05 IN62                          PIC 1.
        05 IN63                          PIC 1.
        05 IN64                          PIC 1.
        05 IN65                          PIC 1.
        05 IN66                          PIC 1.
        05 IN67                          PIC 1.
        05 IN68                          PIC 1.
        05 IN69                          PIC 1.
     ‚* ENDE INDIKATOREN FÜR DIE FELDÄNDERUNG
     ‚* INDIK. FÜR DIE ÄNDERUNG DER NR (IN70= ZEILE1, IN89 = ZEILE20)
        05 IN70                          PIC 1.
        05 IN71                          PIC 1.
        05 IN72                          PIC 1.
        05 IN73                          PIC 1.
        05 IN74                          PIC 1.
        05 IN75                          PIC 1.
        05 IN76                          PIC 1.
        05 IN77                          PIC 1.
        05 IN78                          PIC 1.
        05 IN79                          PIC 1.
        05 IN80                          PIC 1.
        05 IN81                          PIC 1.
        05 IN82                          PIC 1.
        05 IN83                          PIC 1.
        05 IN84                          PIC 1.
        05 IN85                          PIC 1.
        05 IN86                          PIC 1.
        05 IN87                          PIC 1.
        05 IN88                          PIC 1.
        05 IN89                          PIC 1.
     ‚* ENDE INDIKATOREN FÜR DIE ZEILENNR-ÄNDERUNG

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  CRD0010DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  CRD0010DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CODE-REVIEW VARIABLEN
           COPY WRKCRCPY   OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

     ‚* API-STRUKTUR
           COPY WRKAPISEU  OF APG-QAPISRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0010".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* FÜR LIKE-DEKLARATION
       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.
        01 PCMPSCM-WRK.
           COPY DDS-ALL-FORMATS OF PCMPSCM.

     ‚* VARIABLEN FÜR DIE SUCHROUTINE
       01  SUCH-TAB.
        05  SUCH-REC OCCURS 10.
         06  SUCH                        LIKE SRCDTA OF QSETSRC-P.
         06  S-POS                       PIC 9(3) COMP.
         06  E-POS                       PIC 9(3) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE FÜR OFFENE SOURCEN
       01  MAX-OPN-SRC-TAB               PIC 9(4) VALUE 9.
       01  OPN-CHOICE                    PIC 9(4) VALUE 1.
       01  OPN-SRC-TAB.
        05 OPN-SRC-MAX                   PIC 9(4).
        05 OPN-SRC-REC OCCURS 9 TIMES.
         06 OPN-SLIB                     PIC X(10).
         06 OPN-SRCN                     PIC X(10).
         06 OPN-SMBR                     PIC X(10).
         06 OPN-AUSW                     PIC 9(1).
         06 OPN-POINTER                  USAGE POINTER.

       01  STR-TAB.
        05  STR-ANZ                      PIC 9(3) COMP.
        05  STR-ANZ2                     PIC 9(3) COMP.
        05  STR-REC OCCURS 100.
         06  STR                         LIKE SRCDTA OF QSETSRC-P.
         06  S-LEN                       PIC 9(3) COMP.
         06  E-LEN                       PIC 9(3) COMP.

       01  SOURCE-TMP                    LIKE SRCDTA OF QSETSRC-P.
       01  SOURCE-WRK                    LIKE SRCDTA OF QSETSRC-P.
       01  SOURCE-FND                    LIKE SRCDTA OF QSETSRC-P.
       01  GEFUNDEN                      PIC 9(01) COMP.
       01  POS                           PIC 9(3) COMP.
       01  POS2                          PIC 9(3) COMP.
       01  LEN                           PIC 9(3) COMP.
       01  LEN2                          PIC 9(3) COMP.
       01  STR-POS                       PIC 9(3) COMP.
       01  STR-ANF                       PIC 9(3) COMP.
       01  STR-LEN                       PIC 9(3) COMP.
       01  STR-WRK                       LIKE SRCDTA OF QSETSRC-P.

     ‚* VARIABLEN FÜR DIE SEUCMD-SUCHE
       01  SEU-CMD-GEFUNDEN              PIC 9(1).
       01  SEU-CMD-GEFUNDEN-POS          PIC 9(4).
       01  CMD-SUCHE-TAB.
        05  CMD-REC OCCURS 10.
         06  CMD-SUCHE                   LIKE SEUCMD OF FORMAT1.
         06  CMD-LAENGE                  PIC 9(4).

     ‚* VARIABLEN FÜR DIE ZEILENBEFEHLE
       01  USRCMD-TAB                    TYPE SEUCMD-TYPE.

     ‚* LAUFVARIABLEN
       01  TAB-COUNTER                   PIC 9(4).
       01  ZEILE-WRK                     PIC 9(4).
       01  MAX-ZEILEN                    PIC 9(4) VALUE 20.
       01  LFDN-MAP                      LIKE MAP-LFDN OF MAPDTA-REC.
       01  IND                           PIC 9(4).
       01  AKT-ZEILE-MIN                 LIKE SRCSEQ OF QSETSRC-P.
       01  AKT-ZEILE-MAX                 LIKE SRCSEQ OF QSETSRC-P.
       01  AKT-LFDN-MIN                  LIKE MAP-LFDN OF MAPDTA-REC.
       01  AKT-LFDN-MAX                  LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* CODE-REVIEW VARIABLEN
       01  CR-VORG                       LIKE CRVORG OF PCHKSRC-L.
       01  CR-LFDN                       LIKE CRLFDN OF PCHKSRC-L.

     ‚* LÄNGEN-VARIABLEN
       01  SRCLEN-WRK                    PIC 9(4).
       01  LEN-TMP                       PIC 9(4).
       01  POS-TMP                       PIC 9(4).
       01  MAXLEN                        PIC 9(4).
       01  REST-LEN                      PIC 9(4).
       01  FIRST-SPACE-POS               PIC 9(4).

     ‚* VARIABLEN FÜR TEILDATEIPRÜFUNG
       01  CHECK-LIB                     PIC X(10).
       01  CHECK-RCN                     PIC X(10).
       01  CHECK-MBR                     PIC X(10).

     ‚* VARIABLEN FÜR DIE ERMITTLUNG DES LETZTEN KEY
       01  LAST-KEY                      LIKE MAP-KEY  OF MAPDTA-REC.
       01  LAST-LFDN                     LIKE MAP-LFDN OF MAPDTA-REC.
       01  LAST-REC                      LIKE MAP-REC  OF MAPDTA-REC.

     ‚* VARIABLEN FÜR DAS LOCK SETZEN
       01  LCK-SET                       PIC 9(1) VALUE ZEROES.

     ‚* SONSTIGE VARIABLEN
       01  OUTQ-DUMMY                    LIKE CPY-OUTQ.
       01  AUSWAHL                       PIC X(2).
       01  SRCSEQ-WRK                    PIC X(7).
       01  SRCSEQ-TMP                    LIKE SRCSEQ-WRK.
       01  USRCMD-WRK                    LIKE SRCSEQ-WRK.
       01  OPEN-COMMAND                  LIKE SRCSEQ-WRK.
       01  RET-COMMAND                   LIKE SRCSEQ-WRK.
       01  SYTYP-WRK                     LIKE SYTYP  OF PCHKHGH-L.
       01  COBOL-COMMAND                 PIC X(50).
       01  COBOL-LAENGE                  PIC 9(4).
       01  SRCDAT-WRK                    LIKE DATE-8.
       01  DATUM-HGH-VGL                 LIKE DATE-8.
       01  ANZREC-SAV                    PIC 9(9).
       01  STRING-WRK                    PIC X(100).
       01  TT-VALUE                      PIC X(5).

       01  RELOAD-WRK                    PIC 9(1).
       01  AENDERN-WRK                   PIC 9(1).
       01  AENDERN-TMP                   PIC 9(1).
       01  IGNORE-ZEILE                  PIC 9(1).
       01  IS-COMMENT                    PIC 9(1).
       01  DEBUG-WRK                     PIC 9(1).
       01  HGH-DATEN                     PIC 9(1).
       01  NEUANZEIGE-WRK                PIC 9(1).
       01  LEERZEILEN-WRK                PIC 9(1).
       01  CHANGE-AKTIV                  PIC 9(1).
       01  CHANGE-ZEILE-AKTIV            PIC 9(1).
       01  CHANGE-SRC-AKTIV              PIC 9(1).
       01  CHANGE-USRCMD-AKTIV           PIC 9(1).
       01  F10-MODUS                     PIC 9(1).
       01  F10-CURFLD                    LIKE CURFLD  OF FORMAT1.
       01  F10-CURPOS                    LIKE CURPOS  OF FORMAT1.
       01  HEAD-WRK                      LIKE SRCHEAD OF FORMAT1.
       01  CHOICE-TEXT-WRK               LIKE C_SRC1  OF FORMAT1.

       01  RECORD-FLAGS.
        05 CHANGE-FLAG                   PIC X(1).
        05 CHANGE-TYPE-FLAG              PIC X(1).
        05 VIEW-FLAG                     PIC X(1).
        05 FILLER-FLAG                   PIC X(7).

       01  DATINXX                       PIC X(1).
       01  AENXX                         PIC X(1).
       01  SEQXX                         PIC X(1).
       01  AUSWXX                        PIC X(1).

       01  CURX-WRK                      LIKE CURX OF FORMAT1.
       01  CURY-WRK                      LIKE CURY OF FORMAT1.

       01  SRCLIB-WRK                    PIC X(10).
       01  SRCRCN-WRK                    PIC X(10).
       01  SRCMBR-WRK                    PIC X(10).
       01  SRCZEI-WRK                    LIKE SRCSEQ OF QSETSRC-P.
       01  LIBFILE-WRK                   LIKE LIBFILE OF FORMAT1.
       01  MBRNAME-WRK                   LIKE MBRNAME OF FORMAT1.

       01  START-ZEILE                   PIC S9(9).
       01  START-ERFOLGT                 PIC 9(1).
       01  LAST-STARTZEILE               LIKE START-ZEILE.
       01  FIRST-SEQNBR                  LIKE SRCSEQ OF QSETSRC-P.
       01  LAST-SEQNBR                   LIKE SRCSEQ OF QSETSRC-P.

     ‚* POINTER ZUR ANZEIGE DER SOURCE, ETC.
       01  SOURCE-PTR                    USAGE POINTER.
       01  RESTORE-PTR                   USAGE POINTER.
       01  BEFEHL-PTR                    USAGE POINTER.
       01  SECTION-PTR                   USAGE POINTER.
       01  NOTIZEN-PTR                   USAGE POINTER.
       01  HIGHLIGHT-PTR                 USAGE POINTER.
       01  SPRUNG-PTR                    USAGE POINTER.
       01  LAST-PTR                      USAGE POINTER.
       01  TEMP-PTR                      USAGE POINTER.

     ‚* VARIABLEN FÜR DIE COMMAND-ERMITTLUNG
       01  SEUCMD-WRK                    LIKE SEUCMD OF FORMAT1.
       01  SEUCMD-NEUSTART               PIC 9(1).

     ‚* VARIABLEN FÜR DIE SUCHE
       01  FNDSTR-WRK                    LIKE SRCDTA OF QSETSRC-P.
       01  FIND-STARTZEILE               LIKE SRCSEQ OF QSETSRC-P.
       01  FIND-GEFUNDEN                 PIC 9(1).
       01  FIND-GEFUNDEN-POS             PIC 9(4).
       01  FIND-GEFUNDEN-ZEILE           LIKE SRCSEQ OF QSETSRC-P.
       01  FIND-ZEILE-LEN                PIC 9(4).
       01  FIND-ZEILE-POS-ANFANG         PIC 9(4).

     ‚* VARIABLEN FÜR DIE XML-FELDERMITTLUNG
       01  XML-FELD                      PIC X(10).
       01  XML-FOUND                     PIC 9(1).

     ‚* VARIABLEN FÜR GET-ZEILE
       01  GET-ZEILE-WRK                 LIKE SRCSEQ OF QSETSRC-P.
       01  GET-ZEILE-HLP                 PIC 9(1).

     ‚* VARIABLEN FÜR GET-SECTION
       01  SECTION-NAME                  PIC X(70).
       01  SECTION-SEQ                   LIKE SRCSEQ OF QSETSRC-P.

     ‚* FELDER FÜR GET-NEXT-WORD
       01  WORD                          PIC X(40).
       01  GNW-IDX                       PIC 9(4).
       01  GNW-POS                       PIC 9(4).
       01  GNW-NEW-READ                  PIC 9(1).

     ‚* VARIABLEN FÜR DIE AUSWAHL
       01  AUSW-WRK                      PIC X(1).
       01  AUSW-SRCSEQ                   LIKE SRCSEQ OF QSETSRC-P.

     ‚* VARIABLEN FÜR DAS SCRAMBLEN
       01  OPTIONS-OK                    PIC 9(1).

     ‚* VARIABLEN FÜR DIE ERMITTLUNG EINES GO BEFEHLS
       01  GO-ZEILE-SEQ                  LIKE SRCSEQ OF QSETSRC-P.
       01  GO-ZEILE-LEN                  PIC 9(4).
       01  GO-ZEILE-LEN-KOMMA            PIC 9(4).
       01  GO-ZEILE-POS-ANFANG           PIC 9(4).
       01  GO-ZEILE-POS-KOMMA            PIC 9(4).

     ‚* VARIABLEN FÜR DIE ERMITTLUNG DES COPY-MODULS
       01  COPY-DDS-LIB                  PIC X(10).
       01  COPY-DDS-RCN                  PIC X(10).
       01  COPY-DDS-MBR                  PIC X(10).
       01  COPY-DDS-LEN                  PIC 9(4).
       01  COPY-DDS-POS                  PIC 9(4).
       01  COPY-MODUL-OFFEN              PIC 9(1).
       01  COPY-START                    PIC 9(1).

     ‚* TABELLE  FÜR ALLE GEÖFFNETEN COPY-MODULE
       01  COPY-TAB                      TYPE SEU-COPY-TAB-TYPE.

     ‚* VARIABLEN FÜR DEN CR-MODUS
       01  CFG-SAV                       LIKE CFG-CPY.
       01  CR-MODE-AKTIV                 PIC 9(1).
       01  CR-MODE-SCHALTER              PIC 9(1).

     ‚* HEX-WERTE
       01  SYHEX-WRK                     LIKE SYHEX  OF PCHKHGH-L.
       01  GREEN-HEX-WRK                 PIC X(1).
       01  WHITE-HEX-WRK                 PIC X(1).
       01  RED-RI-HEX-WRK                PIC X(1).
       01  GREEN-RI-HEX-WRK              PIC X(1).

     ‚* VARIABLEN FÜR DIE API-KOMMUNIKATION
       01  SEU-API-INPUT                 TYPE SEU-API-INPUT-STRUCT.
       01  SEU-API-OUTPUT                TYPE SEU-API-OUTPUT-STRUCT.
       01  SEU-API-XMLFFD                TYPE API-INPUT-EXPL-TYPE.
       01  API-XMLFFD                    PIC 9(1).
       01  API-AKTION                    PIC 9(1).

     ‚* PARAMETER FÜR DAS CLP0008
       01  SEUOPT-WRK                    PIC X(2).
       01  ERROR-TEXT-SHORT              PIC X(255).
       01  ERROR-TEXT-LONG               PIC X(3000).

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).

     ‚* PARAMETER FÜR DAS CRP0011
       01  NEWSRCLIB-WRK                 PIC X(10).
       01  NEWSRCRCN-WRK                 PIC X(10).
       01  NEWSRCMBR-WRK                 PIC X(10).
       01  NOCHECK-WRK                   PIC X(1).

     ‚* PARAMETER FÜR DAS CRP0007
       01  MAPKEY-WRK                    LIKE MAP-KEY OF MAPDTA-REC.

     ‚* PARAMETER FÜR DAS CFP9010
       01  CR-TYP                        LIKE PGM-TYP.
       01  CR-PGM                        LIKE PGM-WRK.

     ‚* PARAMETER FÜR DAS CRP0008
       01  LCK-AKTION                    PIC 9(1).
       01  LCK-LIB                       PIC X(10).
       01  LCK-RCN                       PIC X(10).
       01  LCK-MBR                       PIC X(10).

     ‚* PARAMETER FÜR DAS CRP0009
       01  MAP-SUCHE-WRK                 LIKE MAP-REC  OF MAPDTA-REC.
       01  MAP-KEY-WRK                   LIKE MAP-KEY  OF MAPDTA-REC.
       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* PARAMETER FÜR DAS HMP0003
       01  HMP-LIB                       PIC X(10).
       01  HMP-PGM                       PIC X(10).

     ‚* PARAMETER FÜR DAS CMP0007
       01  SCMKEY-WRK                    LIKE SMKEY  OF PCMPSCM-WRK.
       01  SCM-PTR                       USAGE POINTER.
       01  SCM-INP-PTR                   USAGE POINTER.
       01  SCM-AKTION                    PIC X(20).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-SRCSEQ                      LIKE SRCSEQ OF QSETSRC-P.
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
           MOVE     "CRP0010"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "CRD0010DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PCHKSRC-DL
                          PCHKHGH-DL
                          PCFGFFD-DL.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.
           INITIALIZE X-F3, X-F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÜBERGABEPARAMETER ÜBERNEHMEN
           SET      SOURCE-PTR TO NULL.
           SET      TEMP-PTR   TO NULL.
           MOVE     X-SRCLIB TO SRCLIB-WRK.
           MOVE     X-SRCRCN TO SRCRCN-WRK.
           MOVE     X-SRCMBR TO SRCMBR-WRK.

           INITIALIZE OPN-SRC-TAB.
       ANF010.
           PERFORM  INIT-VAR.

     ‚* LESE-DATEN: LESEN ALLER BENÖTIGTEN DATEN IN MAPDTA
           PERFORM  LESE-DATEN.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ANF020.
     ‚* ENTSCHEIDEN WELCHEN POINTER WIR HIER WOLLEN
           SET      PTR-READ TO TRUE.
           MOVE     "SOURCE-"  TO PTR-KEY OF PTRMAP-REC.
           MOVE     OPN-CHOICE TO PTR-KEY OF PTRMAP-REC(8:).
           PERFORM  COPY-PTR-MAPPING.
           SET      SOURCE-PTR TO PTR-RETURN OF PTRMAP-REC.

     ‚* JETZT ANFANGEN DIE ZEILEN ZU LESEN UND INS DISPLAY ZU SCHREIBEN
           PERFORM  FILL-FMT01.

     ‚* ANZEIGEN SUBFILE
           PERFORM  ANZEIGE-DISPLAY.
           IF       F3 = "1"
                    MOVE "1" TO X-F3
                    GO TO ENDE
           END-IF.
           IF       F5 = "1"
                    INITIALIZE F5
                    GO TO ANF020
           END-IF.
           IF       F12 = "1"
                    MOVE "1" TO X-F12
                    GO TO ENDE
           END-IF.

     ‚* MUSS NEUGESTARTET WERDEN?
           IF       NEWSRCLIB-WRK NOT = SPACES
            AND     NEWSRCRCN-WRK NOT = SPACES
            AND     NEWSRCMBR-WRK NOT = SPACES
     ‚*             NEUE WERTE ÜBERNEHMEN
                    MOVE NEWSRCLIB-WRK TO SRCLIB-WRK
                    MOVE NEWSRCRCN-WRK TO SRCRCN-WRK
                    MOVE NEWSRCMBR-WRK TO SRCMBR-WRK
                    MOVE NEWSRCLIB-WRK TO X-SRCLIB
                    MOVE NEWSRCRCN-WRK TO X-SRCRCN
                    MOVE NEWSRCMBR-WRK TO X-SRCMBR
                    MOVE AENDERN-TMP   TO AENDERN-WRK
                    IF   AENDERN-WRK NOT = ZEROES
                         MOVE "2" TO X-OPTION
                    END-IF
                    IF   AENDERN-WRK     = ZEROES
                         MOVE "5" TO X-OPTION
                    END-IF
                    GO TO ANF010
           END-IF.

     ‚* NEU ANZEIGEN?
           IF       NEUANZEIGE-WRK NOT = ZEROES
                    INITIALIZE NEUANZEIGE-WRK
                    GO TO ANF020
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           IF       CFFK12 OF CFG-CPY     = ZEROES
            OR      AENDERN-WRK       NOT = ZEROES
                    GO TO ANF020
           END-IF.

       ENDE.
     ‚* LOCK ENTFERNEN SOFERN GESETZT
           IF       LCK-SET NOT = ZEROES
                    PERFORM DLT-MBR-LOCK
           END-IF.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           CLOSE                 PCHKHGH-DL
                                 PCFGFFD-DL
                                 PCHKSRC-DL.

     ‚* POINTER LEEREN?
           PERFORM  CLEAR-VAR.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER SOURCZEILEN
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE RET-CODE.
           INITIALIZE LAST-SEQNBR.
           INITIALIZE FIRST-SEQNBR.
           INITIALIZE STR-ANZ, STR-ANZ2.

           SET      TEMP-PTR    TO NULL.
           SET      SECTION-PTR TO NULL.

     ‚* EINLESEN DER SOURCE
           CALL     "CRP0025" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    SRCLIB-WRK
                                    SRCRCN-WRK
                                    SRCMBR-WRK
                                    SRCZEI-WRK
                                    ANZREC-SAV
                                    FIRST-SEQNBR
                                    LAST-SEQNBR
                                    TEMP-PTR
                                    SECTION-PTR
                                    RET-CODE
           END-CALL.
     ‚* DEN SOURCE-POINTER JETZT DER ZENTRALEN POINTERVERWALTUNG
     ‚* ANHÄNGEN
           IF       SECTION-PTR NOT = NULL
                    SET     PTR-ADD TO TRUE
                    SET     PTR-INPUT OF PTRMAP-REC TO SECTION-PTR
                    MOVE    "SECTION"  TO PTR-KEY OF PTRMAP-REC
                    PERFORM COPY-PTR-MAPPING
           END-IF.

     ‚* NUR BEI DER ORIGINALDATEI INITIALISIEREN, ALLES ANDERE WIRD
     ‚* HINZUGEFÜGT
           IF       X-SRCLIB     = SRCLIB-WRK
            AND     X-SRCRCN     = SRCRCN-WRK
            AND     X-SRCMBR     = SRCMBR-WRK
                    INITIALIZE DATEN, ANZREC-SAV
     ‚*             SETZEN EINES LOCKS
                    MOVE     X-SRCLIB TO LCK-LIB
                    MOVE     X-SRCRCN TO LCK-RCN
                    MOVE     X-SRCMBR TO LCK-MBR
                    PERFORM  SET-MBR-LOCK
                    IF   RET-CODE NOT = SPACES
                         MOVE "1" TO RET-CODE
                         GO TO LES-DAT-90
                    END-IF
           END-IF.

     ‚* BEIM STARTEN DES SEU DIE SOURCE HINZUFÜGEN
           PERFORM  ADD-OPNSRC-TAB.

     ‚* DIE SOURCE EINMAL SICHERN
           PERFORM  SOURCE-KOPIEREN.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DES HEADERS
     ‚*--------------------------------------------------------------
       FILL-FMT01-HEADER SECTION.
       FIL-FH1-00.

           INITIALIZE LEN-TMP, MAXLEN, LIBFILE-WRK, MBRNAME-WRK.
     ‚* NAMEN DER SOURCE ANZEIGEN
           MOVE     SRCMBR-WRK TO MBRNAME-WRK.
           STRING   SRCLIB-WRK      DELIMITED BY SPACES
                    "/"             DELIMITED BY SIZE
                    SRCRCN-WRK      DELIMITED BY SPACES
           INTO     LIBFILE-WRK.

     ‚* LÄNGE BERECHNEN
           COMPUTE  LEN-TMP = LENGTH OF LIBFILE-WRK.
           PERFORM  UNTIL LEN-TMP                    = ZEROES
                     OR   LIBFILE-WRK(LEN-TMP:1) NOT = SPACES
                    SUBTRACT 1 FROM LEN-TMP
           END-PERFORM.

     ‚* JETZT STRING ZEICHEN FÜR ZEICHEN VERSCHIEBEN
           COMPUTE  MAXLEN = LENGTH OF LIBFILE-WRK - LEN-TMP + 1
           END-COMPUTE.
           IF       MAXLEN < LENGTH OF LIBFILE-WRK
            AND     MAXLEN > ZEROES
                    MOVE LIBFILE-WRK TO LIBFILE OF FORMAT1(MAXLEN:)
           END-IF.

           INITIALIZE LEN-TMP, MAXLEN.
     ‚* DAS GLEICHE MIT DEM SOURCENAME
           COMPUTE  LEN-TMP = LENGTH OF MBRNAME-WRK.
           PERFORM  UNTIL MBRNAME-WRK(LEN-TMP:1) NOT = SPACES
                     OR   LEN-TMP                    = ZEROES
                    SUBTRACT 1 FROM LEN-TMP
           END-PERFORM.

           COMPUTE  MAXLEN = LENGTH OF MBRNAME-WRK - LEN-TMP + 1
           END-COMPUTE.
           IF       MAXLEN < LENGTH OF MBRNAME-WRK
            AND     MAXLEN > ZEROES
                    MOVE MBRNAME-WRK TO MBRNAME OF FORMAT1(MAXLEN:)
           END-IF.

           INITIALIZE RET-CODE.
     ‚* SIND INFORMATIONEN VORHANDEN?
           CALL     "CFP0010" USING PGM-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE RET-CODE
                    MOVE AN  TO IN40 OF INXX
           ELSE
                    MOVE AUS TO IN40 OF INXX
           END-IF.

     ‚* WURDE DIE SOURCE GEÄNDERT?
           MOVE     AUS TO IN41 OF INXX.
           IF       CHANGE-SRC-AKTIV NOT = ZEROES
                    MOVE AN      TO IN02 OF INXX
                    MOVE COL-WHT TO A_CHGINF OF FORMAT1
                    MOVE "*"     TO CHGINF   OF FORMAT1
                    MOVE AN      TO IN41 OF INXX
     ‚*             PROGRAMMTEXT FÜR GEÄNDERT LESEN
                    MOVE     "PGM0020"     TO CPY-TXID
                    MOVE     SPRACHE-WRK   TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT    TO SRCCHG  OF FORMAT1
           END-IF.

           IF       AENDERN-WRK NOT = ZEROES
                    MOVE     "PGM0019"     TO CPY-TXID
                    MOVE     SPRACHE-WRK   TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT    TO SRCVIEW   OF FORMAT1
                    MOVE     COL-RED-RI    TO A_SRCVIEW OF FORMAT1
                    MOVE     AN            TO IN02 OF INXX
           ELSE
                    MOVE     "PGM0018"     TO CPY-TXID
                    MOVE     SPRACHE-WRK   TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT    TO SRCVIEW OF FORMAT1
                    MOVE     COL-WHT-RI    TO A_SRCVIEW OF FORMAT1
                    MOVE     AN            TO IN02 OF INXX
           END-IF.

     ‚* HABEN WIR EINEN SUCHSTRING?
           IF       FNDSTR-WRK NOT = SPACES
                    MOVE     FNDSTR-WRK    TO FNDSTR OF FORMAT1
                    MOVE     COL-GRN       TO A_FNDSTR OF FORMAT1
                    MOVE     AN            TO IN02 OF INXX
           ELSE
                    MOVE     ATR-ND        TO A_FNDSTR OF FORMAT1
                    MOVE     AN            TO IN02 OF INXX
           END-IF.

     ‚* ERSTE ZEILE PROTECTEN?
           IF       AENDERN-WRK = ZEROES
                    MOVE     COL-GRN-PR    TO A_SRCSEQ00 OF FORMAT1
                    MOVE     AN            TO IN02 OF INXX
           END-IF.

     ‚* DEN TT-WERT, ETC. ANZEIGEN
           MOVE     TT-VALUE               TO TTVAL  OF FORMAT1.
           MOVE     MBRTEXT OF RTVMBRD-REC TO MBRTXT OF FORMAT1.

     ‚* ANZAHL RECORDS
           CALL     "CRP0018" USING SOURCE-PTR
                                    ANZREC-SAV
           END-CALL.

     ‚* DIE OBERSTE ZEILE ERMITTELN
           EVALUATE SRCTYPE OF RTVMBRD-REC
                    WHEN "CBL"
                    WHEN "CBLLE"
                    WHEN "SQLCBLLE"
                         MOVE ".....A*. 1 B..+... 2" TO HEAD-WRK(01:20)
                         MOVE " ...+... 3 ...+... 4" TO HEAD-WRK(21:20)
                         MOVE " ...+... 5 ...+... 6" TO HEAD-WRK(41:20)
                         MOVE " ...+... 7 ...+... 8" TO HEAD-WRK(61:20)
                         MOVE "PRG-NA.. 9 ...+...10" TO HEAD-WRK(81:20)
                         MOVE " ...+...11 "          TO HEAD-WRK(101:)
                    WHEN "DSPF"
                    WHEN "PRTF"
                         MOVE ".....AAN01N02N03A.NA" TO HEAD-WRK(01:20)
                         MOVE "ME++++++RLÄNGEDDSFZE" TO HEAD-WRK(21:20)
                         MOVE "IPOSFUNKTIONEN++++++" TO HEAD-WRK(41:20)
                         MOVE "++++++++++++++++++++" TO HEAD-WRK(61:20)
                         MOVE "++++++++++++++++++++" TO HEAD-WRK(81:20)
                         MOVE "+++++++++++"          TO HEAD-WRK(101:)
                    WHEN OTHER
                         MOVE ".....A*. 1 ...+... 2" TO HEAD-WRK(01:20)
                         MOVE " ...+... 3 ...+... 4" TO HEAD-WRK(21:20)
                         MOVE " ...+... 5 ...+... 6" TO HEAD-WRK(41:20)
                         MOVE " ...+... 7 ...+... 8" TO HEAD-WRK(61:20)
                         MOVE "PRG-NA.. 9 ...+...10" TO HEAD-WRK(81:20)
                         MOVE " ...+...11 "          TO HEAD-WRK(101:)
           END-EVALUATE.

     ‚* HEADER ÜBERNEHMEN
           MOVE     HEAD-WRK TO SRCHEAD OF FORMAT1.

       FIL-FH1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN ALLER SOURCEZEILEN
     ‚*--------------------------------------------------------------
       FILL-FMT01 SECTION.
       FIL-FT1-00.

           INITIALIZE FORMAT1.
           INITIALIZE LEERZEILEN-WRK.
     ‚* LAUFENDE NUMMER INITIALISIEREN
           MOVE     ZEROES  TO START-ERFOLGT.
           MOVE     ZEROES  TO LFDN-MAP.
           MOVE     ZEROES  TO ZEILE-WRK.
           IF       START-ZEILE > ANZREC-SAV
                    MOVE ANZREC-SAV TO START-ZEILE
           END-IF.
           IF       START-ZEILE < ZEROES
                    MOVE ZEROES     TO START-ZEILE
           END-IF.

           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.

       FIL-FT1-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE     1             TO START-ERFOLGT
                    PERFORM  START-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FIL-FT1-80
                    END-IF
           ELSE
                    MOVE     MAP-READ-NEXT TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FIL-FT1-80
                    END-IF
           END-IF.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(1:10) TO RECORD-FLAGS.
           MOVE     MAP-REC OF MAPDTA-REC(10:)  TO SRC-WRK.
           IF       MAP-KEY OF MAPDTA-REC(50:) NOT = SPACES
                    MOVE COL-GRN-RI TO SRCDTA OF SRC-WRK(98:1)
                    MOVE MAP-KEY OF MAPDTA-REC(71:10) TO
                         SRCDTA OF SRC-WRK(99:10)
           END-IF.

     ‚* JETZT DIE ZEILE FÜLLEN
           PERFORM  AUSGABE-ZEILE.

     ‚* ZEILENNUMMERN MERKEN
           IF       ZEILE-WRK = 1
                    MOVE SRCSEQ   OF SRC-WRK    TO AKT-ZEILE-MIN
                    MOVE MAP-LFDN OF MAPDTA-REC TO AKT-LFDN-MIN
           END-IF.
           IF       ZEILE-WRK = MAX-ZEILEN
                    MOVE SRCSEQ   OF SRC-WRK    TO AKT-ZEILE-MAX
                    MOVE MAP-LFDN OF MAPDTA-REC TO AKT-LFDN-MAX
           END-IF.

     ‚* NÄCHSTE ZEILE LESEN
           IF       ZEILE-WRK < MAX-ZEILEN
                    ADD   1       TO LFDN-MAP
                    GO TO FIL-FT1-20
           END-IF.

       FIL-FT1-80.
           IF       ZEILE-WRK < MAX-ZEILEN
                    PERFORM FILL-EMPTY-FIELDS
           END-IF.

       FIL-FT1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER LEEREN ZEILEN
     ‚*--------------------------------------------------------------
       FILL-EMPTY-FIELDS SECTION.
       FIL-EMP-00.

           PERFORM  WITH TEST BEFORE UNTIL ZEILE-WRK > MAX-ZEILEN
                    INITIALIZE QSETSRC OF SRC-WRK
                    INITIALIZE RECORD-FLAGS
                    MOVE    1 TO LEERZEILEN-WRK
                    PERFORM AUSGABE-ZEILE
           END-PERFORM.

       FIL-EMP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN VON MAPDTA
     ‚*--------------------------------------------------------------
       START-MAPDTA SECTION.
       STR-MAP-00.

           INITIALIZE RET-CODE.
     ‚* LAUFENDE NUMMER ODER GEZIELTES READ?
           IF       GO-ZEILE-SEQ = ZEROES
                    INITIALIZE MAPDTA-REC
                    MOVE     START-ZEILE   TO MAP-LFDN OF MAPDTA-REC
                    MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO STR-MAP-90
                    END-IF
           ELSE
                    INITIALIZE MAPDTA-REC
                    MOVE    SRCLIB-WRK TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE    SRCRCN-WRK TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE    SRCMBR-WRK TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE    GO-ZEILE-SEQ TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE X'FFFFFFFFFFFF' TO MAP-KEY OF MAPDTA-REC(38:6)
                    MOVE    MAP-READ     TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
                    MOVE    ZEROES TO GO-ZEILE-SEQ
     ‚*             BEI FEHLER NEUSTART IN DER BISHERIGEN ZEILE
                    IF   RET-CODE NOT = SPACES
                         GO TO STR-MAP-00
                    ELSE
                         MOVE MAP-LFDN OF MAPDTA-REC TO START-ZEILE
                    END-IF
           END-IF.
           IF       RET-CODE NOT = SPACES
                    GO TO STR-MAP-90
           END-IF.

       STR-MAP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE DER ZEILE
     ‚*--------------------------------------------------------------
       AUSGABE-ZEILE SECTION.
       AUS-ZLE-00.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
     ‚*             DATUM AUFBEREITEN
                    IF   CFDAT1 OF CFG-CPY(1:8) IS NUMERIC
                         MOVE CFDAT1 OF CFG-CPY(1:8) TO DATUM-HGH-VGL
                    END-IF
           END-IF.

           MOVE     SRCDTA OF SRC-WRK TO SOURCE-TMP.
           MOVE     SRCDTA OF SRC-WRK TO SOURCE-WRK.

     ‚* SOLL DIE ZEILE IGNORIERT WERDEN?
           MOVE     ZEROES TO IGNORE-ZEILE.
           IF       CFFK11 OF CFG-CPY NOT = ZEROES
                    PERFORM DATUM-AUSBLENDEN
           END-IF.

     ‚* KOMMENTARE IGNORIEREN?
     ‚*€   IF       CFFKXX OF CFG-CPY NOT = ZEROES
     ‚*€    AND     SOURCE-WRK(7:1)       = "*"
     ‚*€            MOVE 1 TO IGNORE-ZEILE
     ‚*€   END-IF.

     ‚* NÄCHSTE ZEILE LESEN?
           IF       IGNORE-ZEILE NOT = ZEROES
                    GO TO AUS-ZLE-90
           END-IF.

     ‚* ZEILE SYNTAX HIGHLIGHTEN => BEFEHLE, SONSTIGE
           IF       CFFK01 OF CFG-CPY NOT = ZEROES
            OR      CFFK05 OF CFG-CPY NOT = ZEROES
                    PERFORM  SYNTAX-HIGHLIGHT-MEMBER
           END-IF.

     ‚* ZEILE SYNTAX HIGHLIGHTEN => KONSTANTEN, E+P
           IF       SRCTYPE OF RTVMBRD-REC = "CBL"
            OR      SRCTYPE OF RTVMBRD-REC = "CBLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "TEMPLATE"
                    PERFORM  SYNTAX-HIGHLIGHT-EUP
           END-IF.

     ‚* KOMMENTARE WEIß FÄRBEN?
           IF       CFFK04 OF CFG-CPY NOT = ZEROES
                    PERFORM  SYNTAX-COMMENT
           END-IF.

     ‚* TRENNLINIE HINZUFÜGEN?
           IF       CFFK10 OF CFG-CPY NOT = ZEROES
                    IF   SOURCE-TMP(72:1) = SPACES
                         MOVE "|" TO SOURCE-TMP(72:1)
                    END-IF
     ‚*             ZUSATZLINIE 1
                    IF   CFNM03 OF CFG-CPY NOT = ZEROES
                     AND CFNM03 OF CFG-CPY NOT > LENGTH OF SOURCE-TMP
                     AND SOURCE-TMP(CFNM03 OF CFG-CPY:1) = SPACES
                         MOVE "|" TO SOURCE-TMP(CFNM03 OF CFG-CPY:1)
                    END-IF
     ‚*             ZUSATZLINIE 2
                    IF   CFNM04 OF CFG-CPY NOT = ZEROES
                     AND CFNM04 OF CFG-CPY NOT > LENGTH OF SOURCE-TMP
                     AND SOURCE-TMP(CFNM04 OF CFG-CPY:1) = SPACES
                         MOVE "|" TO SOURCE-TMP(CFNM04 OF CFG-CPY:1)
                    END-IF
           END-IF.

     ‚* JETZT DIE GEÄNDERTE ZEILE ZURÜCKSPIELEN
           MOVE     SOURCE-TMP          TO SRCDTA OF SRC-WRK.

           INITIALIZE DATINXX, AENXX, SEQXX.
     ‚* IST DAS DATUMS-HIGHLIGHTING AKTIV, WIRD DAS DATUM ANGEZEIGT
     ‚* WENN ES IN DAS KRITERIUM PASST
           IF       CFFK06 OF CFG-CPY NOT = ZEROES
                    PERFORM  DATUM-HIGHLIGHT
           END-IF.
     ‚* SOLL ANGEZEIGT WERDEN OB NOTIZEN VORHANDEN SIND?
           IF       CFFK13 OF CFG-CPY NOT = ZEROES
                    PERFORM  NOTIZ-HIGHLIGHT
           END-IF.

     ‚* IST ÄNDERN ERLAUBT?
           IF       AENDERN-WRK = ZEROES
                    MOVE COL-GRN-PR TO AENXX
                    MOVE COL-GRN-PR TO SEQXX
                    MOVE AN         TO IN02 OF INXX
           END-IF.

     ‚* ZEILENNUMMER AUFBEREITEN
           MOVE     SRCSEQ OF SRC-WRK(1:4) TO SRCSEQ-WRK(1:4).
           MOVE     "."                    TO SRCSEQ-WRK(5:1).
           MOVE     SRCSEQ OF SRC-WRK(5:2) TO SRCSEQ-WRK(6:2).
           PERFORM  AUSGABE-ZEILE-MARKIEREN.

     ‚* AUSWAHL PROTECTEN UND UNSICHTBAR?
           INITIALIZE AUSWXX.
           IF       SRCSEQ OF SRC-WRK       = ZEROES
            OR      SRCSEQ-WRK              = SPACES
            OR      SRCDTA OF SRC-WRK(98:1) = COL-GRN-RI
                    MOVE COL-GRN-PR TO AENXX
                    MOVE COL-GRN-PR TO SEQXX
                    MOVE ATR-PR-ND  TO AUSWXX
                    MOVE AN         TO IN02 OF INXX
           END-IF.
     ‚* AUSWAHL GENERELL SCHÜTZEN?
           IF       CFFK14 OF CFG-CPY   NOT = ZEROES
                    MOVE ATR-PR-ND  TO AUSWXX
                    MOVE AN         TO IN02 OF INXX
           END-IF.

     ‚* LEERZEILE?
           IF       LEERZEILEN-WRK NOT = ZEROES
                    MOVE COL-GRN-PR TO AENXX
                    MOVE COL-GRN-PR TO SEQXX
                    MOVE ATR-PR-ND  TO AUSWXX
                    MOVE AN         TO IN02 OF INXX
           END-IF.

     ‚* ZEILE ALS MARKIERT ANZEIGEN?
           IF       CHANGE-FLAG OF RECORD-FLAGS NOT = SPACES
            AND     CFFK16 OF CFG-CPY           NOT = ZEROES
                    MOVE COL-BLU    TO SEQXX
                    MOVE AN         TO IN02 OF INXX
           END-IF.

           ADD      1                      TO ZEILE-WRK.
     ‚* JE NACHDEM WELCHE ZEILE WIR HABEN JETZT ANDERS FÜLLEN
           EVALUATE ZEILE-WRK
                    WHEN 1
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ01H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ01 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA01 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT01 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT01 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA01 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ01 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW01   OF FORMAT1
                    WHEN 2
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ02H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ02 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA02 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT02 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT02 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA02 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ02 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW02   OF FORMAT1
                    WHEN 3
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ03H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ03 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA03 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT03 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT03 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA03 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ03 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW03   OF FORMAT1
                    WHEN 4
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ04H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ04 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA04 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT04 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT04 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA04 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ04 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW04   OF FORMAT1
                    WHEN 5
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ05H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ05 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA05 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT05 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT05 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA05 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ05 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW05   OF FORMAT1
                    WHEN 6
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ06H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ06 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA06 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT06 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT06 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA06 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ06 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW06   OF FORMAT1
                    WHEN 7
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ07H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ07 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA07 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT07 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT07 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA07 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ07 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW07   OF FORMAT1
                    WHEN 8
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ08H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ08 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA08 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT08 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT08 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA08 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ08 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW08   OF FORMAT1
                    WHEN 9
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ09H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ09 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA09 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT09 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT09 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA09 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ09 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW09   OF FORMAT1
                    WHEN 10
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ10H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ10 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA10 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT10 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT10 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA10 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ10 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW10   OF FORMAT1
                    WHEN 11
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ11H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ11 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA11 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT11 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT11 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA11 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ11 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW11   OF FORMAT1
                    WHEN 12
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ12H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ12 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA12 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT12 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT12 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA12 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ12 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW12   OF FORMAT1
                    WHEN 13
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ13H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ13 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA13 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT13 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT13 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA13 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ13 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW13   OF FORMAT1
                    WHEN 14
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ14H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ14 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA14 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT14 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT14 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA14 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ14 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW14   OF FORMAT1
                    WHEN 15
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ15H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ15 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA15 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT15 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT15 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA15 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ15 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW15   OF FORMAT1
                    WHEN 16
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ16H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ16 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA16 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT16 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT16 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA16 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ16 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW16   OF FORMAT1
                    WHEN 17
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ17H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ17 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA17 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT17 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT17 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA17 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ17 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW17   OF FORMAT1
                    WHEN 18
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ18H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ18 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA18 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT18 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT18 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA18 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ18 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW18   OF FORMAT1
                    WHEN 19
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ19H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ19 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA19 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT19 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT19 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA19 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ19 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW19   OF FORMAT1
                    WHEN 20
                         MOVE SRCSEQ OF SRC-WRK TO SRCSEQ20H OF FORMAT1
                         MOVE SRCSEQ-WRK        TO SRCSEQ20 OF FORMAT1
                         MOVE SRCDTA OF SRC-WRK TO SRCDTA20 OF FORMAT1
                         MOVE SRCDAT OF SRC-WRK TO SRCDAT20 OF FORMAT1
                         MOVE DATINXX         TO A_SRCDAT20 OF FORMAT1
                         MOVE AENXX           TO A_SRCDTA20 OF FORMAT1
                         MOVE SEQXX           TO A_SRCSEQ20 OF FORMAT1
                         MOVE AUSWXX          TO A_AUSW20   OF FORMAT1
           END-EVALUATE.

       AUS-ZLE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* GGF. DIE ZEILENNUMMER DURCH DEN BEFEHL ERSETZEN
     ‚*--------------------------------------------------------------
       AUSGABE-ZEILE-MARKIEREN SECTION.
       AUS-ZMK-00.

     ‚* NUR WENN EIN OFFENER BEFEHL AUSGEFÜHRT WIRD
TODO ‚*€   IF       RET-COMMAND = SPACES
     ‚*€            GO TO AUS-ZMK-90
TODO ‚*€   END-IF.

           MOVE     1 TO IND.
           PERFORM  WITH TEST BEFORE UNTIL IND   >= 500
                     OR SRCSEQ OF TAB-REC OF USRCMD-TAB(IND) = ZEROES
     ‚*             BEFEHL EINTRAGEN
                    IF   SRCSEQ OF SRC-WRK = SRCSEQ OF TAB-REC OF
                         USRCMD-TAB(IND)
                         MOVE SRCCMD OF TAB-REC OF USRCMD-TAB(IND) TO
                              SRCSEQ-WRK
TODO  *                  MOVE ZEROES TO SRCSEQ OF TAB-REC OF
TODO  *                       USRCMD-TAB(IND)
                    END-IF
                    ADD 1 TO IND
           END-PERFORM.

       AUS-ZMK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES DISPLAYS
     ‚*--------------------------------------------------------------
       ANZEIGE-DISPLAY SECTION.
       ANZ-DSP-00.

       ANZ-DSP-10.
           MOVE     PGM-TYP           TO PGMTYP OF FORMAT1.
           PERFORM  FILL-FMT01-HEADER.

     ‚* SFL-ÜBERSCHRIFTEN FÜLLEN
           PERFORM  SFL-HEADER.
     ‚* CURSER AUF DEN TEXT SETZEN?
           IF       CURX-WRK = ZEROES
            AND     CURY-WRK = ZEROES
                    IF   FIND-GEFUNDEN-POS NOT = ZEROES
                         MOVE 6 TO CURY OF FORMAT1
                         COMPUTE CURX OF FORMAT1 = FIND-GEFUNDEN-POS
                         END-COMPUTE
                         MOVE AN  TO IN03 OF INXX
     ‚*                  JETZT FEHLERPOS ENTFERNEN
                         MOVE ZEROES TO FIND-GEFUNDEN-POS
                    ELSE
     ‚*                  ERMITTELN DER CURSER-POSITION
                         MOVE    "FMT01" TO FMT-WRK
                         PERFORM GET-CURSER-POS
     ‚*                  AUF NÄCHSTE ZEILE SETZEN BEIM INSERT
                         IF   RET-COMMAND = "I"
                              PERFORM GET-CURSER-POS-X
                              IF   CURY OF FORMAT1 < 25
                                   ADD  1  TO CURY OF FORMAT1
                              END-IF
                              MOVE 23      TO CURX OF FORMAT1
                         END-IF
     ‚*                  CURSER SETZEN
                         IF   CURX OF FORMAT1 NOT = ZEROES
                          AND CURY OF FORMAT1 NOT = ZEROES
                              MOVE AN  TO IN03 OF INXX
                         END-IF
                    END-IF
           ELSE
                    MOVE CURX-WRK TO CURX OF FORMAT1
                    MOVE CURY-WRK TO CURY OF FORMAT1
                    MOVE AN       TO IN03 OF INXX
     ‚*             JETZT WIEDER ZURÜCKSETZEN
                    MOVE ZEROES TO CURX-WRK
                    MOVE ZEROES TO CURY-WRK
           END-IF.

     ‚* RADIO-BUTTONS FÜLLEN
           MOVE     ZEROES TO IND.
           PERFORM  MAX-OPN-SRC-TAB TIMES
                    ADD 1 TO IND
                    IF   OPN-SMBR OF OPN-SRC-REC(IND) = SPACES
                         MOVE "<EMPTY>" TO CHOICE-TEXT-WRK
                    ELSE
                         STRING OPN-SLIB OF OPN-SRC-REC(IND)
                                    DELIMITED BY SPACES
                                "/" DELIMITED BY SIZE
                                OPN-SRCN OF OPN-SRC-REC(IND)
                                    DELIMITED BY SPACES
                                "/" DELIMITED BY SIZE
                                OPN-SMBR OF OPN-SRC-REC(IND)
                                    DELIMITED BY SPACES
                           INTO CHOICE-TEXT-WRK
                         END-STRING
                    END-IF
     ‚*             TEXT ANZEIGEN
                    EVALUATE IND
                         WHEN 1 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC1 OF FORMAT1
                         WHEN 2 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC2 OF FORMAT1
                         WHEN 3 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC3 OF FORMAT1
                         WHEN 4 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC4 OF FORMAT1
                         WHEN 5 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC5 OF FORMAT1
                         WHEN 6 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC6 OF FORMAT1
                         WHEN 7 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC7 OF FORMAT1
                         WHEN 8 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC8 OF FORMAT1
                         WHEN 9 MOVE CHOICE-TEXT-WRK TO
                                     C_SRC9 OF FORMAT1
                    END-EVALUATE
           END-PERFORM.

     ‚* API: CURSER GGF. MIT APIEINGABE ÜBERSCHREIBEN
           IF       CUR-X OF API-CURSER NOT = ZEROES
            AND     CUR-Y OF API-CURSER NOT = ZEROES
                    MOVE CUR-X OF API-CURSER TO CURX OF FORMAT1
                    MOVE CUR-Y OF API-CURSER TO CURX OF FORMAT1
                    MOVE AN                  TO IN03 OF INXX
           END-IF.

     ‚*
           MOVE     OPN-CHOICE TO SOURCETAB OF FORMAT1.

     ‚* ANZEIGE SUBFILE
           MOVE     ZEROES TO NEUANZEIGE-WRK.
           MOVE     ANZREC-SAV      TO  ANZREC OF FORMAT1.
           MOVE     CORR   INXX     TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  API-CALL-OUTPUT.
           IF       API-KEY-NEWDSP OF SEU-API-OUTPUT NOT = "3"
                    PERFORM  COPY-DISPLAY-WRITE
                    PERFORM  COPY-DISPLAY-READ
           END-IF.
           PERFORM  API-CALL-INPUT.
           MOVE     WSREC-WRK TO FORMAT1-I.
           MOVE     CORR FORMAT1-I-INDIC TO  OUTXX.
           MOVE     CORR FORMAT1-I TO FORMAT1.
           MOVE     AUS  TO INXX.

     ‚* F3 ENDE
           IF       IN03 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F3
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F5 AKTUALISIEREN
           IF       IN05 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F10 CURSER AUF EINGABEZEILE
           IF       IN10 OF FORMAT1-I-INDIC = AN
                    MOVE ZEROES TO CURX-WRK
                    MOVE ZEROES TO CURY-WRK
                    IF   F10-MODUS = ZEROES
                         MOVE    "SEUCMD" TO XML-FELD
                         PERFORM GET-XMLFFD
                         IF   XML-FOUND NOT = ZEROES
                              MOVE WHDROW OF PCFGFFD-L TO CURY-WRK
                              MOVE WHDCOL OF PCFGFFD-L TO CURX-WRK
                         END-IF
                         MOVE 1                   TO F10-MODUS
                         MOVE CURFLD OF FORMAT1-I TO F10-CURFLD
                         MOVE CURPOS OF FORMAT1-I TO F10-CURPOS
                    ELSE
                         MOVE ZEROES     TO F10-MODUS
                         MOVE F10-CURFLD TO CURFLD OF FORMAT1-I
                         MOVE F10-CURPOS TO CURPOS OF FORMAT1-I
                    END-IF
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 OF FORMAT1-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* SOLL NUR DAS DISPLAY NOCHMAL ANGEZEIGT WERDEN OHNE BESONDERE
     ‚* SEU-LOGIK AUSZUFÜHREN?
           IF       API-KEY-NEWDSP OF SEU-API-OUTPUT = "2"
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* PRÜFEN OB EINE ÄNDERUNG VORLIEGT
           PERFORM  CHECK-CHANGE.
           IF       CHANGE-AKTIV NOT = ZEROES
                    MOVE    1 TO CHANGE-SRC-AKTIV
                    PERFORM SOURCE-CHANGE
           ELSE
                    PERFORM CHECK-RET-COMMAND
           END-IF.

     ‚* GIBT ES NOCH EINE OFFENE ZEILE? Z.B. WURDE EIN I AUF EINE
     ‚* ZEILE AUSGEFÜHRT, JEDOCH NICHTS EINGETRAGEN? DANN KANN DIESE
     ‚* GGF. WIEDER GELÖSCHT WERDEN. ODER ES WURDE ETWAS EINGETRAGEN,
     ‚* DANN MUSS EINE WEITERE ZEILE HINZUGEFÜGT WERDEN
     ‚* GLEICHZEITIG WERDEN DIE BEARBEITETEN SÄTZE WIEDER ENTFERN
           IF       RET-COMMAND NOT = SPACES
                    MOVE SPACES TO RET-COMMAND
     ‚*€            PERFORM CHECK-RET-COMMAND
           END-IF.

     ‚* WURDE IN DER ZEILENNUMMER EIN BEFEHL EINGETRAGEN?
           PERFORM  CHECK-ZEILENBEFEHL.
           IF       CHANGE-ZEILE-AKTIV NOT = ZEROES
                    PERFORM SOURCE-ZEILENBEFEHL
                    IF   CHANGE-USRCMD-AKTIV NOT = ZEROES
                         PERFORM SOURCE-ZEILENBEFEHL-EXEC
                    END-IF
           END-IF.

     ‚* F13 EINSTELLUNGEN
           IF       IN13 OF FORMAT1-I-INDIC = AN
                    PERFORM CHANGE-SETTINGS
                    IF   F3 = "1"
                         GO TO ANZ-DSP-90
                    END-IF
                    INITIALIZE F12
                    MOVE "1" TO F5
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F18 CODE-REVIEW
           IF       IN18 OF FORMAT1-I-INDIC = AN
                    PERFORM START-CODEREVIEW
                    MOVE "1" TO F5
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F20 INFOS ANZEIGEN
           IF       IN20 OF FORMAT1-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* F21 KOMMANDOZEILE AUFRUFEN
           IF       IN21 OF FORMAT1-I-INDIC = AN
                    PERFORM KOMMANDOZEILE
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* F22 SEU-STARTEN
           IF       IN22 OF FORMAT1-I-INDIC = AN
                    PERFORM START-SEU
                    GO TO ANZ-DSP-10
           END-IF.

     ‚* BILD AUF / BILD AB
           IF       IN98 OF FORMAT1-I-INDIC = AN
                    PERFORM SCROLL-DOWN
                    GO TO ANZ-DSP-90
           END-IF.

           IF       IN99 OF FORMAT1-I-INDIC = AN
                    PERFORM SCROLL-UP
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* JETZT DIE VERSCHIEDENEN ZEILENBEFEHLE AUSFÜHREN
           IF       SEUCMD OF FORMAT1 NOT = SPACES
                    MOVE 1 TO NEUANZEIGE-WRK
                    MOVE SEUCMD OF FORMAT1 TO SEUCMD-WRK
                    PERFORM SEU-COMMAND
                    IF   F3 = "1"
                         GO TO ANZ-DSP-90
                    END-IF
                    IF   RET-CODE NOT = SPACES
                         GO TO ANZ-DSP-10
                    END-IF
                    IF   SEUCMD-NEUSTART NOT = ZEROES
                         GO TO ANZ-DSP-90
                    END-IF
           END-IF.

     ‚* PRÜFEN OB EINE AUSWAHL DURCHGEFÜHRT WERDEN SOLL
           PERFORM  CHECK-AUSWAHL.
           IF       NEUANZEIGE-WRK NOT = ZEROES
            AND     AUSW-WRK       NOT = SPACES
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* F16 GEDRÜCKT? SUCHE STARTEN
           IF       IN16 OF FORMAT1-I-INDIC = AN
                    IF   FNDSTR-WRK = SPACES
                         GO TO ANZ-DSP-10
                    END-IF
           END-IF.

     ‚* SOLL EINE SUCHE AUSGEFÜHRT WERDEN?
           IF       FNDSTR-WRK NOT = SPACES
                    MOVE 1 TO NEUANZEIGE-WRK
                    PERFORM FIND-TEXT
                    IF   FIND-GEFUNDEN-ZEILE NOT = ZEROES
                         MOVE FIND-GEFUNDEN-ZEILE TO GO-ZEILE-SEQ
                         MOVE FIND-GEFUNDEN-ZEILE TO FIND-STARTZEILE
                    END-IF
                    GO TO ANZ-DSP-90
           END-IF.

     ‚* WURDE EINE SOURCE IM AUSWAHLFELD AUSGEWÄHLT?
           IF       SOURCETAB OF FORMAT1 NOT = OPN-CHOICE
                    MOVE SOURCETAB OF FORMAT1 TO OPN-CHOICE
                    MOVE 1                    TO NEUANZEIGE-WRK
                    GO TO ANZ-DSP-90
           END-IF.

       ANZ-DSP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*--------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

     ‚* ÜBERSCHRIFT
           MOVE     "CRP0010"         TO MAINUEB OF FORMAT1.
           MOVE     "CRP0010*1"       TO SUBUEB  OF FORMAT1.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MEMBERINFORMATIONEN ANZEIGEN
     ‚*--------------------------------------------------------------
       SHOW-MEMBER-INFOS SECTION.
       SHW-INF-00.

           INITIALIZE F3, F5, F12.
           CALL     "CRP0027" USING SRCLIB-WRK
                                    SRCRCN-WRK
                                    SRCMBR-WRK
                                    F3
                                    F12
           END-CALL.

           INITIALIZE RTVMBRD-REC.
           MOVE     SRCLIB-WRK        TO OBJLIB   OF RTVMBRD-REC.
           MOVE     SRCRCN-WRK        TO FILENAME OF RTVMBRD-REC.
           MOVE     SRCMBR-WRK        TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       SHW-INF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINSTELLUNGEN ÄNDERN
     ‚*--------------------------------------------------------------
       CHANGE-SETTINGS SECTION.
       CHG-SET-00.

           INITIALIZE F3, F5, F12.
           CALL     "CRP0024" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    F3
                                    F12
           END-CALL.
           IF       F3 = "1"
                    GO TO CHG-SET-90
           END-IF.
           INITIALIZE F12.

     ‚* JETZT IN CFG-CPY ÜBERNEHMEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
     ‚*             SYNTAX-HIGHLIGHTING UND ANDERE DEFAULTS
                    MOVE  1           TO CFFK01 OF CFG-CPY
                    MOVE  1           TO CFFK03 OF CFG-CPY
                    MOVE  ZEROES      TO CFFK04 OF CFG-CPY
                    MOVE  1           TO CFFK13 OF CFG-CPY
                    MOVE  "QTMPSRC"   TO CFDAT2 OF CFG-CPY
                    MOVE  "APG"       TO CFDAT3 OF CFG-CPY
                    MOVE  "QTEMP"     TO CFDAT4 OF CFG-CPY
                    MOVE  "*SAME"     TO CFDAT5 OF CFG-CPY
                    MOVE 0,01         TO CFNK01 OF CFG-CPY
           END-IF.

     ‚* DA ETWAS GEÄNDERT WERDEN KONNTE, NUN NEU EINLESEN
           PERFORM  GET-SYNTAX-HGH-CMD.

     ‚* BEZEICHNUNG NEU EINLESEN
           INITIALIZE RTVMBRD-REC.
           MOVE     X-SRCLIB          TO OBJLIB   OF RTVMBRD-REC.
           MOVE     X-SRCRCN          TO FILENAME OF RTVMBRD-REC.
           MOVE     X-SRCMBR          TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       CHG-SET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DURCHSUCHEN STRINGTABELLE
     ‚*--------------------------------------------------------------
       SUCHEN SECTION.
       STR-SUC-00.

           INITIALIZE GEFUNDEN.
     ‚* ALLE SUCHERGEBNISSE LÖSCHEN
           MOVE     ZEROES TO IND.
           PERFORM  10 TIMES
                    ADD  1      TO IND
                    MOVE ZEROES TO S-POS(IND)
                    MOVE ZEROES TO E-POS(IND)
           END-PERFORM.

     ‚* SOURCE AUF GROßBUCHSTABEN UMSETZEN
           MOVE     1 TO IND.
           MOVE     FUNCTION UPPER-CASE(SOURCE-WRK) TO SOURCE-FND.

           MOVE     1 TO POS.
           MOVE     SUCH(1) TO STR-WRK.
           PERFORM  ERMITTELN-LAENGE.

           PERFORM  WITH TEST BEFORE UNTIL SOURCE-FND(IND:) = SPACES
                     OR IND >= LENGTH OF SOURCE-FND
                     OR STR-LEN + IND >  LENGTH OF SOURCE-FND
                    IF   SOURCE-FND(IND:STR-LEN) =
                         SUCH(1)(STR-ANF:STR-LEN)
     ‚*                  STARTPOSITION SICHERN
                         COMPUTE S-POS(POS) = IND
     ‚*                  ENDPOSITION SICHERN
                         COMPUTE E-POS(POS) = IND + STR-LEN
                         END-COMPUTE
                         MOVE 1 TO GEFUNDEN
                         ADD  1 TO POS
                    END-IF
                    ADD  1 TO IND
           END-PERFORM.

       STR-SUC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN LÄNGE EINES ÜBERGEBENEN STRINGS
     ‚*--------------------------------------------------------------
       ERMITTELN-LAENGE SECTION.
       ERM-LEN-00.

     ‚* WARSCHEINLICH WIRD DER NÄCHSTES SUCHSTRING IN DER TABELLE
     ‚* GESUCHT. DESHALB VORAB SCHONMAL PRÜFEN OB DIESER DAS IST.
     ‚* ANDERENFALLS WIRD NOCHMAL DIE KOMPLETTE TABELLE DURCHSUCHT.
           IF       STR-ANZ2 + 1 <= STR-ANZ
            AND     STR(STR-ANZ2 + 1) = STR-WRK
                    ADD 1 TO STR-ANZ2
                    MOVE S-LEN(STR-ANZ2) TO STR-ANF
                    MOVE E-LEN(STR-ANZ2) TO STR-LEN
                    GO TO ERM-LEN-90
           END-IF.

     ‚* IST FÜR DIESEN STRING BEREITS DIE SUCHE ERFOLGT, DANN
     ‚* GESICHERTE WERTE AUS DER TABELLE ÜBERNEHMEN (PERFORMANCE)
           INITIALIZE STR-ANZ2.
           PERFORM  UNTIL STR-ANZ2 >= STR-ANZ
                    ADD 1 TO STR-ANZ2
                    IF   STR(STR-ANZ2) = STR-WRK
                         MOVE S-LEN(STR-ANZ2) TO STR-ANF
                         MOVE E-LEN(STR-ANZ2) TO STR-LEN
                         GO TO ERM-LEN-90
                    END-IF
           END-PERFORM.

           INITIALIZE STR-ANF.
           INITIALIZE STR-LEN.
           PERFORM  WITH TEST BEFORE VARYING STR-POS FROM LENGTH OF
                    STR-WRK BY -1 UNTIL STR-POS = ZEROES
                    IF   STR-WRK(STR-POS:1) NOT = SPACE
                         IF   STR-LEN = ZEROES
     ‚*                       ABSCHLUSSZEICHEN
                              IF   STR-WRK(STR-POS:1) = "'"
                                   COMPUTE STR-LEN = STR-POS - 1
                              ELSE
                                   COMPUTE STR-LEN = STR-POS
                              END-IF
                         ELSE
                              IF   STR-WRK(STR-POS:1) = "'"
     ‚*                            ANFANGSZEICHEN
                                   COMPUTE STR-ANF = STR-POS + 1
                              END-IF
                         END-IF
                    END-IF
           END-PERFORM.

           IF       STR-ANF = ZEROES
                    MOVE 1 TO STR-ANF
           END-IF.
           COMPUTE  STR-LEN = STR-LEN - STR-ANF + 1.

     ‚* ERMITTELTE WERTE IN TABELLE ABLEGEN.
           ADD      1 TO STR-ANZ.
           IF       STR-ANZ > 100
                    MOVE 1 TO STR-ANZ
           END-IF.
           MOVE     STR-WRK TO STR-REC(STR-ANZ).
           MOVE     STR-ANF TO S-LEN(STR-ANZ).
           MOVE     STR-LEN TO E-LEN(STR-ANZ).

       ERM-LEN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SYNTAX-HIGHLIGHTING FÜR EINE TEILDATEI
     ‚*--------------------------------------------------------------
       SYNTAX-HIGHLIGHT-MEMBER SECTION.
       SYN-HGM-00.

           INITIALIZE SUCH-TAB.
           INITIALIZE STR-ANZ, STR-ANZ2.
     ‚* KOMMENTARE NICHT HIGHLIGHTEN
           IF       SOURCE-WRK(7:1)     = "*"
            AND     SOURCE-WRK(7:3) NOT = "*DB"
                    GO TO SYN-HGM-90
           END-IF.

     ‚* ERSTMAL ALLE BEFEHEL BLAU FÄRBEN
           IF       CFFK01 OF CFG-CPY NOT = ZEROES
                    INITIALIZE STR-ANZ, STR-ANZ2
                    PERFORM  SYNTAX-HIGHLIGHT-COMMAND
           END-IF.

           IF       CFFK05 OF CFG-CPY NOT = ZEROES
                    INITIALIZE STR-ANZ, STR-ANZ2
                    PERFORM  SYNTAX-HIGHLIGHT-OTHER
           END-IF.

       SYN-HGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHLE JETZT FÄRBEN
     ‚*--------------------------------------------------------------
       SYNTAX-HIGHLIGHT-COMMAND SECTION.
       SYN-HGC-00.

     ‚* SOURCETYP ERMITTELN
           CALL     "CRP0020" USING T-USER
                                    SRCTYPE OF RTVMBRD-REC
                                    SYTYP-WRK
           END-CALL.

     ‚* HIGHLIGHTING DURCHFÜHREN
           CALL     "CRP0023" USING HIGHLIGHT-PTR
                                    SYTYP-WRK
                                    SOURCE-TMP
           END-CALL.

       SYN-HGC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEFEHLE JETZT FÄRBEN
     ‚*--------------------------------------------------------------
       SYNTAX-HIGHLIGHT-OTHER SECTION.
       SYN-HGO-00.

           INITIALIZE SYTYP-WRK.
           MOVE     "OTHER" TO SYTYP-WRK.
     ‚* HIGHLIGHTING DURCHFÜHREN
           CALL     "CRP0023" USING HIGHLIGHT-PTR
                                    SYTYP-WRK
                                    SOURCE-TMP
           END-CALL.

       SYN-HGO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* E+P BEFEHLE FÄRBEN
     ‚*--------------------------------------------------------------
       SYNTAX-HIGHLIGHT-EUP SECTION.
       SYN-HGE-00.

           INITIALIZE SUCH-TAB.
           INITIALIZE STR-ANZ, STR-ANZ2.
     ‚* KOMMENTARE NICHT HIGHLIGHTEN
           IF       SOURCE-WRK(7:1)     = "*"
            AND     SOURCE-WRK(7:3) NOT = "*DB"
                    GO TO SYN-HGE-90
           END-IF.

     ‚* HIGHLIGHTING DURCHFÜHREN
           IF       CFFK03 OF CFG-CPY NOT = ZEROES
                    INITIALIZE SYTYP-WRK
                    MOVE     "E+P" TO SYTYP-WRK
                    CALL     "CRP0023" USING HIGHLIGHT-PTR
                                             SYTYP-WRK
                                             SOURCE-TMP
                    END-CALL
           END-IF.

       SYN-HGE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMENTARE AUTOMATISCH WEIß FÄRBEN
     ‚*--------------------------------------------------------------
       SYNTAX-COMMENT SECTION.
       SYN-COM-00.

     ‚* NUR KOMMENTARE FÄRBEN SOFERN DORT NICHT SCHON EINE ANDERE
     ‚* FARBE VORGESEHEN WAR
           IF       SOURCE-TMP(7:1) NOT = "*"
                    GO TO SYN-COM-90
           END-IF.

     ‚* JETZT WEIß FÄRBEN SOFERN KEINE ANDERE FARBE VORGESEHEN WAR.
     ‚* ES SEI DENN DIE WEIßE FARBE SOLL ERZWUNGEN WERDEN, DANN WIRD
     ‚* DER ALTE WERT ÜBERSCHRIEBEN
           IF       SOURCE-TMP(6:1)   NOT = SPACES
            AND     CFFK04 OF CFG-CPY NOT = 2
                    GO TO SYN-COM-90
           END-IF.

           MOVE     WHITE-HEX-WRK TO SOURCE-TMP(6:1).

       SYN-COM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATUMSFELD INVERS ANZEIGEN WENN ES PASST
     ‚*--------------------------------------------------------------
       DATUM-HIGHLIGHT SECTION.
       DAT-HGH-00.

     ‚* DATUM ZUM VERGLEICH AUFBEREITEN
           MOVE     SRCDAT OF SRC-WRK TO SRCDAT-WRK(3:6).
           IF       SRCDAT OF SRC-WRK(1:2) > 90
                    MOVE 19 TO SRCDAT-WRK(1:2)
           ELSE
                    MOVE 20 TO SRCDAT-WRK(1:2)
           END-IF.

     ‚* WERTE PRÜFEN
           EVALUATE CFFK07 OF CFG-CPY
                    WHEN ZEROES
                         IF   SRCDAT-WRK < DATUM-HGH-VGL
                              MOVE AN         TO IN02 OF INXX
                              MOVE COL-GRN-RI TO DATINXX
                         END-IF
                    WHEN 1
                         IF   SRCDAT-WRK = DATUM-HGH-VGL
                              MOVE AN         TO IN02 OF INXX
                              MOVE COL-GRN-RI TO DATINXX
                         END-IF
                    WHEN 2
                         IF   SRCDAT-WRK > DATUM-HGH-VGL
                              MOVE AN         TO IN02 OF INXX
                              MOVE COL-GRN-RI TO DATINXX
                         END-IF
           END-EVALUATE.

       DAT-HGH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* JETZT PRÜFEN OB NOTIZEN VORHANDEN SIND
     ‚*--------------------------------------------------------------
       NOTIZ-HIGHLIGHT SECTION.
       NOT-HGH-00.

     ‚* DATUM ZUM VERGLEICH AUFBEREITEN
           PERFORM  CHECK-NOTIZ-VORHANDEN.
           IF       RET-CODE = SPACES
     ‚*             WENN SCHON INVERS, DANN AUCH INVERS LASSEN
                    IF   DATINXX = COL-GRN-RI
                         MOVE AN         TO IN02 OF INXX
                         MOVE COL-PNK-RI TO DATINXX
                    ELSE
                         MOVE AN         TO IN02 OF INXX
                         MOVE COL-PNK    TO DATINXX
                    END-IF
           END-IF.

       NOT-HGH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE AUSBLENDEN
     ‚*--------------------------------------------------------------
       DATUM-AUSBLENDEN SECTION.
       DAT-AUS-00.

     ‚* DATUM ZUM VERGLEICH AUFBEREITEN
           MOVE     SRCDAT OF SRC-WRK TO SRCDAT-WRK(3:6).
           IF       SRCDAT OF SRC-WRK(1:2) > 90
                    MOVE 19 TO SRCDAT-WRK(1:2)
           ELSE
                    MOVE 20 TO SRCDAT-WRK(1:2)
           END-IF.

     ‚* WERTE PRÜFEN
           EVALUATE CFFK07 OF CFG-CPY
                    WHEN ZEROES
                         IF   SRCDAT-WRK >= DATUM-HGH-VGL
                              MOVE 1 TO IGNORE-ZEILE
                         END-IF
                    WHEN 1
                         IF   SRCDAT-WRK NOT = DATUM-HGH-VGL
                              MOVE 1 TO IGNORE-ZEILE
                         END-IF
                    WHEN 2
                         IF   SRCDAT-WRK <= DATUM-HGH-VGL
                              MOVE 1 TO IGNORE-ZEILE
                         END-IF
           END-EVALUATE.

       DAT-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DER SEU-COMMAND (Z.B. BOTTOM, TOP, ETC...)
     ‚*--------------------------------------------------------------
       SEU-COMMAND SECTION.
       SEU-CMD-00.

           INITIALIZE F3.
           INITIALIZE RET-CODE.
           INITIALIZE SEUCMD-NEUSTART.
     ‚* WILL DER USER NACH UNTEN SPRINGEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "B"       TO CMD-SUCHE(1).
           MOVE     2         TO CMD-LAENGE(1).
           MOVE     "BOT"     TO CMD-SUCHE(2).
           MOVE     4         TO CMD-LAENGE(2).
           MOVE     "BOTTOM"  TO CMD-SUCHE(3).
           MOVE     7         TO CMD-LAENGE(3).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    COMPUTE START-ZEILE = ANZREC-SAV - MAX-ZEILEN
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER NACH OBEN SPRINGEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "T"       TO CMD-SUCHE(1).
           MOVE     2         TO CMD-LAENGE(1).
           MOVE     "TOP"     TO CMD-SUCHE(2).
           MOVE     4         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    MOVE ZEROES TO START-ZEILE
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER IN EINE BESTIMMTE ZEILE SPRINGEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "G"       TO CMD-SUCHE(1).
           MOVE     2         TO CMD-LAENGE(1).
           MOVE     "GO"      TO CMD-SUCHE(2).
           MOVE     3         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-EXTRACT-ZEILE
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER ETWAS SUCHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "F"       TO CMD-SUCHE(1).
           MOVE     2         TO CMD-LAENGE(1).
           MOVE     "FIND"    TO CMD-SUCHE(2).
           MOVE     5         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    PERFORM SEU-COMMAND-EXTRACT-SUCHE
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER DIE DATEI SICHERN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SAVE"    TO CMD-SUCHE(1).
           MOVE     5         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SOURCE-SAVE
                    GO TO SEU-CMD-90
           END-IF.

     ‚* SOLL AUF DEN ÄNDERNMODUS GEWECHSELT WERDEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "EDIT"    TO CMD-SUCHE(1).
           MOVE     5         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO AENDERN-TMP
                    MOVE    1 TO SEUCMD-NEUSTART
                    MOVE    X-SRCLIB TO NEWSRCLIB-WRK
                    MOVE    X-SRCRCN TO NEWSRCRCN-WRK
                    MOVE    X-SRCMBR TO NEWSRCMBR-WRK
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WOLLEN WIR EINE ANDERE TEILDATEI ÖFFNEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "OPEN"    TO CMD-SUCHE(1).
           MOVE     5         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-OPEN
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WOLLEN WIR EINE ANDERE TEILDATEI ZUM ÄNDERN ÖFFNEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "OPENC"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO AENDERN-TMP
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-OPEN
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WOLLEN WIR UNS DIE SOURCE IM DSPPFM ANSEHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "DSPPFM"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-DSPPFM
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WOLLEN WIR UNS DIE SOURCE IM SEU ANSEHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "STRSEU"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM START-SEU
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WOLLEN WIR UNS DIE SOURCE IM SQL ANSEHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "RUNSQL"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM START-RUNSQL
                    GO TO SEU-CMD-90
           END-IF.

     ‚* SOLL EIN WINDOW MIT ALLEN SECTIONS ANGEZEIGT WERDEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SECTION" TO CMD-SUCHE(1).
           MOVE     8         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-WINDOW-SECTION
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER ZU EINER BESTIMMTEN ZEILE EINE NOTIZ ANLEGEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "NOTE"    TO CMD-SUCHE(1).
           MOVE     5         TO CMD-LAENGE(1).
           MOVE     "N"       TO CMD-SUCHE(2).
           MOVE     2         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-EXTRACT-ZEILE
                    IF   GO-ZEILE-SEQ NOT = ZEROES
                         MOVE GO-ZEILE-SEQ TO AUSW-SRCSEQ
                         PERFORM AUSW-N-WRITENOTE
                         MOVE ZEROES TO GO-ZEILE-SEQ
                    END-IF
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER ALLE NOTIZEN SEHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "NOTES"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-NOTES
                    GO TO SEU-CMD-90
           END-IF.

     ‚* LOCK PRÜFEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "WRKLCK"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-LOCK
                    GO TO SEU-CMD-90
           END-IF.

     ‚* OPTIONEN ANZEIGEN? WIE F13
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "OPTIONS"  TO CMD-SUCHE(1).
           MOVE     8          TO CMD-LAENGE(1).
           MOVE     "SETTINGS" TO CMD-SUCHE(2).
           MOVE     9          TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM CHANGE-SETTINGS
                    IF   F3 = "1"
                         GO TO SEU-CMD-90
                    END-IF
                    INITIALIZE F12
                    GO TO SEU-CMD-90
           END-IF.

     ‚* CR-MODUS AKTIVIEREN / DEAKTIVIEREN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SETCR"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM START-CRMODE
                    GO TO SEU-CMD-90
           END-IF.

     ‚* TEILDATEIEN-INFOS ANZEIGEN? WIE F13 -> F20
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "MEMBER"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SHOW-MEMBER-INFOS
                    IF   F3 = "1"
                         GO TO SEU-CMD-90
                    END-IF
                    INITIALIZE F12
                    GO TO SEU-CMD-90
           END-IF.

     ‚* DATEI NEU EINLESEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "RELOAD"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE AENDERN-WRK TO AENDERN-TMP
                    MOVE X-SRCLIB TO NEWSRCLIB-WRK
                    MOVE X-SRCRCN TO NEWSRCRCN-WRK
                    MOVE X-SRCMBR TO NEWSRCMBR-WRK
                    GO TO SEU-CMD-90
           END-IF.

     ‚* GESICHERTEN POINTER ZURÜCKSPIELEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "RESTORE" TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    CALL "CRP0016" USING RESTORE-PTR
                                         SOURCE-PTR
                    END-CALL
                    GO TO SEU-CMD-90
           END-IF.

     ‚* DATEI SCHLIEßEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "CLOSE"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           MOVE     "CLS"     TO CMD-SUCHE(2).
           MOVE     4         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE "1" TO F3
                    GO TO SEU-CMD-90
           END-IF.

     ‚* PROGRAMM IN HERMES TAGGEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "TAG"     TO CMD-SUCHE(1).
           MOVE     4         TO CMD-LAENGE(1).
           MOVE     "HERMES"  TO CMD-SUCHE(2).
           MOVE     7         TO CMD-LAENGE(2).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM START-HERMES
                    IF   F3 = "1"
                         GO TO SEU-CMD-90
                    END-IF
                    INITIALIZE F12
                    GO TO SEU-CMD-90
           END-IF.

     ‚* WILL DER USER DIE SPRUNGHISTORIE SEHEN?
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "GOHIST"  TO CMD-SUCHE(1).
           MOVE     7         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-GOHIST
                    GO TO SEU-CMD-90
           END-IF.

     ‚* SOURCE DRUCKEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "PRINT"   TO CMD-SUCHE(1).
           MOVE     6         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM START-PRINTING
                    GO TO SEU-CMD-90
           END-IF.

     ‚* POINTER ANZEIGEN (AUS POINTERVERWALTUNG)
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SHOWPTR" TO CMD-SUCHE(1).
           MOVE     8         TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-SHOWPTR
                    GO TO SEU-CMD-90
           END-IF.

     ‚* SOURCE SCRAMBLEN
           INITIALIZE CMD-SUCHE-TAB.
           MOVE     "SCRAMBLE" TO CMD-SUCHE(1).
           MOVE     9          TO CMD-LAENGE(1).
           PERFORM  SEU-COMMAND-SUCHE-BEFEHL.
           IF       SEU-CMD-GEFUNDEN NOT = ZEROES
                    MOVE    1 TO SEUCMD-NEUSTART
                    PERFORM SEU-COMMAND-EXTRACT-SCMOPT
                    IF   OPTIONS-OK = 1
                         PERFORM START-SCRAMBLEN
                    END-IF
                    GO TO SEU-CMD-90
           END-IF.

     ‚* KEIN BEFEHL GEFUNDEN? DANN NIMM DAS WORT ALS SUCHWORT
           MOVE     ZEROES TO SEU-CMD-GEFUNDEN-POS.
           INSPECT  SEUCMD-WRK TALLYING SEU-CMD-GEFUNDEN-POS FOR
                    LEADING SPACES.
           IF       SEU-CMD-GEFUNDEN-POS = ZEROES
                    MOVE 1 TO SEU-CMD-GEFUNDEN-POS
           ELSE
                    ADD  1 TO SEU-CMD-GEFUNDEN-POS
           END-IF.

     ‚* SUCHSTRING ÜBERNEHMEN
           MOVE     SEUCMD-WRK(SEU-CMD-GEFUNDEN-POS:) TO FNDSTR-WRK.

       SEU-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN DES SEU-COMMANDS
     ‚*--------------------------------------------------------------
       SEU-COMMAND-SUCHE-BEFEHL SECTION.
       SEU-SUC-00.

           INITIALIZE IND.
           INITIALIZE SEU-CMD-GEFUNDEN.
           INITIALIZE SEU-CMD-GEFUNDEN-POS.

       SEU-SUC-20.
           ADD      1 TO IND.
           IF       CMD-SUCHE OF CMD-SUCHE-TAB(IND) = SPACES
                    GO TO SEU-SUC-90
           END-IF.

     ‚* SUCHEN DES TEXTES
           MOVE     ZEROES TO LEN-TMP.
           INSPECT  SEUCMD-WRK TALLYING LEN-TMP FOR CHARACTERS BEFORE
                    INITIAL CMD-SUCHE OF CMD-SUCHE-TAB(IND)(1:
                    CMD-LAENGE OF CMD-SUCHE-TAB(IND)).
           IF       LEN-TMP = LENGTH OF SEUCMD-WRK
                    GO TO SEU-SUC-20
           END-IF.
           IF       LEN-TMP > 1
                    IF   SEUCMD-WRK(LEN-TMP - 1:1) NOT = SPACES
                         GO TO SEU-SUC-20
                    END-IF
                    IF   SEUCMD-WRK(LEN-TMP:1)     NOT = SPACES
                         GO TO SEU-SUC-20
                    END-IF
           END-IF.

           MOVE     1 TO SEU-CMD-GEFUNDEN.
           COMPUTE  SEU-CMD-GEFUNDEN-POS = LEN-TMP + 1.

       SEU-SUC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÖFFNEN EINER ANDEREN TEILDATEI
     ‚*--------------------------------------------------------------
       SEU-COMMAND-OPEN SECTION.
       SCM-OPN-00.

           INITIALIZE F12.
           MOVE     SRCLIB-WRK TO NEWSRCLIB-WRK.
           MOVE     SRCRCN-WRK TO NEWSRCRCN-WRK.
           MOVE     SPACES     TO NEWSRCMBR-WRK.
           MOVE     SPACES     TO NOCHECK-WRK.

     ‚* WINDOW ÖFFNEN
           CALL     "CRP0011" USING NEWSRCLIB-WRK
                                    NEWSRCRCN-WRK
                                    NEWSRCMBR-WRK
                                    NOCHECK-WRK
                                    F12
           END-CALL.
           IF       F12 = "1"
                    INITIALIZE F12
                    INITIALIZE NEWSRCLIB-WRK
                    INITIALIZE NEWSRCRCN-WRK
                    INITIALIZE NEWSRCMBR-WRK
           END-IF.

       SCM-OPN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER ZEILENNUMMER AUS DER EINGABE
     ‚*--------------------------------------------------------------
       SEU-COMMAND-EXTRACT-ZEILE SECTION.
       SCM-EZL-00.

           INITIALIZE GO-ZEILE-SEQ.
     ‚* IM REGELFALL WIRD MAN "GO 123" EINGEBEN, MAN KANN ABER AUCH
     ‚* "GO 123.10" BZW. "GO 123,10" EINGEBEN
           MOVE     ZEROES TO FIRST-SPACE-POS.
           INSPECT  SEUCMD-WRK(SEU-CMD-GEFUNDEN-POS:)
                    TALLYING FIRST-SPACE-POS FOR CHARACTERS
                    BEFORE INITIAL SPACES.
           ADD      SEU-CMD-GEFUNDEN-POS TO FIRST-SPACE-POS.
           IF       FIRST-SPACE-POS = ZEROES
            OR      FIRST-SPACE-POS = LENGTH OF SEUCMD-WRK
                    MOVE "1" TO RET-CODE
                    GO TO SCM-EZL-90
           END-IF.

     ‚* JETZT DEN TEXT "123" BZW. "123.10"/"123,10" EINGEBEN
           MOVE     ZEROES TO IND.
           COMPUTE  IND = FIRST-SPACE-POS + 1
           END-COMPUTE.

     ‚* IST DAS ERSTE ZEICHEN EIN PUNKT, SO WIRD EIN TAG-NAME
     ‚* EINGEBEN. DIESEN JETZT BEARBEITEN
           IF       SEUCMD-WRK(IND:1) = "."
                    PERFORM SEU-COMMAND-EXTRACT-TAGNAME
                    GO TO SCM-EZL-90
           END-IF.

           MOVE     ZEROES TO GO-ZEILE-LEN.
           MOVE     ZEROES TO GO-ZEILE-LEN-KOMMA.
           MOVE     ZEROES TO GO-ZEILE-POS-ANFANG.
           MOVE     ZEROES TO GO-ZEILE-POS-KOMMA.
           PERFORM  WITH TEST BEFORE UNTIL IND > LENGTH OF SEUCMD-WRK
                     OR   SEUCMD-WRK(IND:) = SPACES
                     OR  RET-CODE NOT = SPACES
                    IF   SEUCMD-WRK(IND:1) IS NUMERIC
     ‚*                  STARTPOSITION MERKEN
                         IF   GO-ZEILE-POS-ANFANG = ZEROES
                              MOVE IND TO GO-ZEILE-POS-ANFANG
                         END-IF
     ‚*                  VORKOMMA ODER NACHKOMMA-WERT?
                         IF   GO-ZEILE-POS-KOMMA = ZEROES
                              ADD 1 TO GO-ZEILE-LEN
                         ELSE
                              ADD 1 TO GO-ZEILE-LEN-KOMMA
                         END-IF
                    ELSE
     ‚*                  EVENTUELL EIN PUNKT?
                         IF   SEUCMD-WRK(IND:1) = "."
                          OR  SEUCMD-WRK(IND:1) = ","
                              MOVE IND TO GO-ZEILE-POS-KOMMA
                         ELSE
                              IF   SEUCMD-WRK(IND:1) NOT = SPACES
                                   MOVE "1" TO RET-CODE
                              END-IF
                         END-IF
                    END-IF
                    ADD 1 TO IND
           END-PERFORM.

     ‚* WIR HABEN JETZT DIE POSITION UND DIE LÄNGE
           IF       GO-ZEILE-LEN     = ZEROES
            OR      GO-ZEILE-LEN     > 4
            OR      RET-CODE     NOT = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO SCM-EZL-90
           END-IF.

     ‚* DIE ZIELVARIABLE IST "999999" AUFGEBAUT, WURDE JETZT 123
     ‚* EINGEGEBEN, DANN MUSS DIES AN STELLE 2:3 EINGEFÜGT WERDEN
           MOVE     GO-ZEILE-POS-ANFANG TO POS-TMP.
           MOVE     GO-ZEILE-LEN        TO LEN-TMP.
           IF       SEUCMD-WRK(POS-TMP:LEN-TMP) IS NUMERIC
                    MOVE SEUCMD-WRK(POS-TMP:LEN-TMP) TO
                         GO-ZEILE-SEQ(4 - LEN-TMP + 1:LEN-TMP)
           END-IF.

     ‚* NACHKOMMASTELLE ÜBERNEHMEN (+1, WEIL AUF DER POSITION JA DAS
     ‚* KOMMA STEHT UND ERST DANACH DER WERT KOMMT)
           IF       GO-ZEILE-POS-KOMMA > ZEROES
            AND     GO-ZEILE-LEN-KOMMA > ZEROES
                    COMPUTE  POS-TMP = GO-ZEILE-POS-KOMMA + 1
                    COMPUTE  LEN-TMP = GO-ZEILE-LEN-KOMMA
                    IF   SEUCMD-WRK(POS-TMP:LEN-TMP) IS NUMERIC
                     AND LEN-TMP <= 2
                         MOVE SEUCMD-WRK(POS-TMP:LEN-TMP) TO
                              GO-ZEILE-SEQ(6 - LEN-TMP + 1:LEN-TMP)
                    END-IF
           END-IF.

       SCM-EZL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÜBER DEN TAGNAME DIE ZEILE ERMITTELN
     ‚*--------------------------------------------------------------
       SEU-COMMAND-EXTRACT-TAGNAME SECTION.
       SCM-ETN-00.

     ‚* TAGPOINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     "LINETAG" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO SCM-ETN-90
           END-IF.

     ‚* JETZT IN MAPDTA DEN TAG LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     SEUCMD-WRK(IND:)   TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO SCM-ETN-90
           END-IF.

     ‚* IST DAS ERGEBNIS NUMERISCH?
           IF       MAP-REC OF MAPDTA-REC(1:6) IS NUMERIC
                    MOVE MAP-REC OF MAPDTA-REC(1:4) TO
                         GO-ZEILE-SEQ(1:4)
                    MOVE MAP-REC OF MAPDTA-REC(5:2) TO
                         GO-ZEILE-SEQ(5:2)
           END-IF.

       SCM-ETN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES SUCHBEGRIFFS
     ‚*--------------------------------------------------------------
       SEU-COMMAND-EXTRACT-SUCHE SECTION.
       SCM-ESC-00.

           INITIALIZE RET-CODE.
     ‚* IM REGELFALL WIRD MAN "F SUCHE" EINGEBEN
           MOVE     ZEROES TO FIRST-SPACE-POS.
           INSPECT  SEUCMD-WRK(SEU-CMD-GEFUNDEN-POS:)
                    TALLYING FIRST-SPACE-POS FOR CHARACTERS
                    BEFORE INITIAL SPACES.
           ADD      SEU-CMD-GEFUNDEN-POS TO FIRST-SPACE-POS.
           IF       FIRST-SPACE-POS = ZEROES
            OR      FIRST-SPACE-POS = LENGTH OF SEUCMD-WRK
                    MOVE "1" TO RET-CODE
                    GO TO SCM-ESC-90
           END-IF.

     ‚* JETZT DEN TEXT "SUCHE" ERMITTELN
           MOVE     ZEROES TO IND.
           COMPUTE  IND = FIRST-SPACE-POS + 1
           END-COMPUTE.

           MOVE     ZEROES TO FIND-ZEILE-LEN.
           MOVE     ZEROES TO FIND-ZEILE-POS-ANFANG.
           PERFORM  WITH TEST BEFORE UNTIL SEUCMD-WRK(IND:) = SPACES
                     OR  IND > LENGTH OF SEUCMD-WRK
                     OR  RET-CODE NOT = SPACES
                    IF   SEUCMD-WRK(IND:1) NOT = SPACES
     ‚*                  STARTPOSITION MERKEN
                         IF   FIND-ZEILE-POS-ANFANG = ZEROES
                              MOVE IND TO FIND-ZEILE-POS-ANFANG
                         END-IF
                         ADD 1 TO FIND-ZEILE-LEN
                    ELSE
                         MOVE "1" TO RET-CODE
                    END-IF
                    ADD 1 TO IND
           END-PERFORM.

     ‚* WIR HABEN JETZT DIE POSITION UND DIE LÄNGE
           IF       FIND-ZEILE-LEN     = ZEROES
            OR      RET-CODE       NOT = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO SCM-ESC-90
           END-IF.

     ‚* JETZT ÜBERNEHMEN
           MOVE     SEUCMD-WRK(FIND-ZEILE-POS-ANFANG:FIND-ZEILE-LEN) TO
                    FNDSTR-WRK.

       SCM-ESC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* OPTIONEN ERMITTELN, WIE: COPY-MODULE AUFLÖSEN, KEY ETC
     ‚*--------------------------------------------------------------
       SEU-COMMAND-EXTRACT-SCMOPT SECTION.
       SCM-ESO-00.

           MOVE     1 TO OPTIONS-OK.
     ‚* ERSTMAL FEST ZUM TESTEN:
           MOVE     "SCRAMBLE_MAIN" TO SCMKEY-WRK.

       SCM-ESO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DSPPFM AUSFÜHREN
     ‚*--------------------------------------------------------------
       SEU-COMMAND-DSPPFM SECTION.
       SCM-DFM-00.

           INITIALIZE RET-CODE.
     ‚* FÜLLEN STRUKTUR UND AUSFÜHREN
           INITIALIZE DSPPFM-REC.
           MOVE     SRCLIB-WRK      TO LIBNAME  OF DSPPFM-REC.
           MOVE     SRCRCN-WRK      TO FILENAME OF DSPPFM-REC.
           MOVE     SRCMBR-WRK      TO MBRNAME  OF DSPPFM-REC.
           CALL     "CLP0047" USING DSPPFM-REC
                                    RET-CODE
           END-CALL.

       SCM-DFM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÖFFNEN EINES WINDOWS MIT ALLEN GEFUNDENEN SECTIONS
     ‚*--------------------------------------------------------------
       SEU-COMMAND-WINDOW-SECTION SECTION.
       SCM-WSC-00.

           INITIALIZE F12, GO-ZEILE-SEQ.
           CALL     "CRP0005" USING SECTION-PTR
                                    GO-ZEILE-SEQ
                                    F12
           END-CALL.
           INITIALIZE F12.

       SCM-WSC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE NOTIZEN IM SUBFILE ANZEIGEN
     ‚*--------------------------------------------------------------
       SEU-COMMAND-NOTES SECTION.
       SCM-NOT-00.

           INITIALIZE F12, GO-ZEILE-SEQ.
           CALL     "CRP0005" USING NOTIZEN-PTR
                                    GO-ZEILE-SEQ
                                    F12
           END-CALL.
           INITIALIZE F12.

       SCM-NOT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE SPRÜNGE IM SUBFILE ANZEIGEN
     ‚*--------------------------------------------------------------
       SEU-COMMAND-GOHIST SECTION.
       SCM-GHS-00.

           INITIALIZE F12, GO-ZEILE-SEQ.
           CALL     "CRP0005" USING SPRUNG-PTR
                                    GO-ZEILE-SEQ
                                    F12
           END-CALL.
           INITIALIZE F12.

       SCM-GHS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN WER DIE TEILDATEI LOCKT
     ‚*--------------------------------------------------------------
       SEU-COMMAND-LOCK SECTION.
       SCM-WLK-00.

           INITIALIZE RET-CODE.
     ‚* WRKOBJLCK AUSFÜHREN
           INITIALIZE WRKOBJLCK-REC.
           MOVE     SRCLIB-WRK      TO LIBNAME  OF WRKOBJLCK-REC.
           MOVE     SRCRCN-WRK      TO FILENAME OF WRKOBJLCK-REC.
           MOVE     SRCMBR-WRK      TO MBRNAME  OF WRKOBJLCK-REC.
           MOVE     "*FILE"         TO TYPNAME  OF WRKOBJLCK-REC.
           CALL     "CLP0050" USING WRKOBJLCK-REC
                                    RET-CODE
           END-CALL.

       SCM-WLK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER ZEILENNUMMER AUS DER EINGABE
     ‚*--------------------------------------------------------------
       SEU-COMMAND-SHOWPTR SECTION.
       SCM-SPT-00.

     ‚* NACH SHOWPTR STEHT DER POINTERNAME
           MOVE     ZEROES TO FIRST-SPACE-POS.
           INSPECT  SEUCMD-WRK(SEU-CMD-GEFUNDEN-POS:)
                    TALLYING FIRST-SPACE-POS FOR CHARACTERS
                    BEFORE INITIAL SPACES.
           ADD      SEU-CMD-GEFUNDEN-POS TO FIRST-SPACE-POS.
           IF       FIRST-SPACE-POS = ZEROES
            OR      FIRST-SPACE-POS = LENGTH OF SEUCMD-WRK
                    MOVE "1" TO RET-CODE
                    GO TO SCM-SPT-90
           END-IF.

     ‚* JETZT DEN NAMEN ERMITTELN
           MOVE     ZEROES TO IND.
           COMPUTE  IND = FIRST-SPACE-POS + 1
           END-COMPUTE.

     ‚* POINTER ERMITTELN
           SET      PTR-READ TO TRUE.
           MOVE     SEUCMD-WRK(IND:) TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           IF       PTR-RETURN OF PTRMAP-REC = NULL
                    GO TO SCM-ETN-90
           END-IF.

     ‚* JETZT DEN POINTER ANZEIGEN
           CALL     "CFP0050" USING PTR-RETURN OF PTRMAP-REC
           END-CALL.

       SCM-SPT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DER AUSWAHL
     ‚*--------------------------------------------------------------
       AUSW-EXECUTE SECTION.
       ASW-EXC-00.

           INITIALIZE RET-CODE.
           MOVE     1 TO NEUANZEIGE-WRK.
           CALL     "CRP0026" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    X-OPTION
                                    AUSW-SRCSEQ
                                    AUSW-WRK
                                    COPY-TAB
                                    GO-ZEILE-SEQ
                                    SOURCE-PTR
                                    RESTORE-PTR
                                    NOTIZEN-PTR
                                    SPRUNG-PTR
                                    SECTION-PTR
           END-CALL.

       ASW-EXC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINER NOTIZ ZU EINER ZEILE
     ‚*--------------------------------------------------------------
       AUSW-N-WRITENOTE SECTION.
       ASN-WNT-00.

     ‚* ZU DER ZEILE WIRD NUN EIN WINDOW GEÖFFNET, IN DEM EINE NOTIZ
     ‚* EINGETRAGEN WERDEN KANN, DIESE KÖNNEN DANN WIEDER ÜBER DEN
     ‚* BEFEHL "NOTES" GEÖFFNET WERDEN
           CALL     "CRP0006" USING SRCLIB-WRK
                                    SRCRCN-WRK
                                    SRCMBR-WRK
                                    AUSW-SRCSEQ
                                    NOTIZEN-PTR
                                    F12
           END-CALL.

       ASN-WNT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE SUCHE LÄUFT IN 2 SCHRITTEN AB:
     ‚*  1. DIE AKTUELL ANGEZEIGTEN ZEILEN DURCHSUCHEN
     ‚*  2. MIT MAPDTA DIE NÄCHSTEN ZEILEN ABSUCHEN BIS ZUM ENDE
     ‚*     DER TEILDATEI
     ‚*--------------------------------------------------------------
       FIND-TEXT SECTION.
       FND-TXT-00.

           INITIALIZE FIND-GEFUNDEN.
           INITIALIZE FIND-GEFUNDEN-ZEILE.
     ‚* SUCHEN DES TEXTES MIT MAPDTA
           MOVE     ZEROES  TO START-ERFOLGT.
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.

       FND-TXT-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE     1             TO START-ERFOLGT
                    MOVE     START-ZEILE   TO MAP-LFDN OF MAPDTA-REC
                    MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FND-TXT-90
                    END-IF
           ELSE
                    MOVE     MAP-READ-NEXT TO MAP-ACT  OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
                    IF   RET-CODE NOT = SPACES
                         GO TO FND-TXT-90
                    END-IF
           END-IF.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO SRC-WRK.

     ‚* ZEILE ÜBERSPRINGEN?
           IF       SRCSEQ OF SRC-WRK <= FIND-STARTZEILE
                    GO TO FND-TXT-20
           END-IF.

     ‚* IST DER GESUCHTE TEXT VORHANDEN?
           IF       FNDSTR-WRK NOT = SPACES
                    MOVE SRCDTA OF SRC-WRK           TO RECORD-WRK
                    MOVE LENGTH OF SRCDTA OF SRC-WRK TO RECORD-LEN
                    MOVE FNDSTR-WRK                  TO SELEKTION-WRK
                    CALL PROCEDURE "HLP0001" USING RECORD-WRK
                                                   RECORD-LEN
                                                   SELEKTION-WRK
                                                   RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         GO TO FND-TXT-20
                    ELSE
                         MOVE SRCSEQ OF SRC-WRK TO FIND-GEFUNDEN-ZEILE
                         MOVE 1                 TO FIND-GEFUNDEN
                         PERFORM FIND-TEXT-POSITION
                         GO TO FND-TXT-90
                    END-IF
           END-IF.

     ‚* NÄCHSTE ZEILE LESEN
           GO TO    FND-TXT-20.

       FND-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* POSITION IM STRING FINDEN
     ‚*--------------------------------------------------------------
       FIND-TEXT-POSITION SECTION.
       FND-TXP-00.

           INITIALIZE SUCH-TAB.
           MOVE     SRCDTA OF SRC-WRK TO SOURCE-WRK.
           MOVE     FNDSTR-WRK        TO SUCH(01).
           PERFORM  SUCHEN.
           IF       S-POS(1) NOT = ZEROES
                    COMPUTE FIND-GEFUNDEN-POS = S-POS(1) + 11
                    END-COMPUTE
           END-IF.

       FND-TXP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SAMMELN DER PROGRAMMINFORMATIONEN
     ‚*--------------------------------------------------------------
       LIST-SOURCE-ELEMENTS SECTION.
       LST-SRE-00.

     ‚* SICHERN ALLE IM PGM VORKOMMENDER SECTIONS
           IF       SRCTYPE OF RTVMBRD-REC = "CBL"
            OR      SRCTYPE OF RTVMBRD-REC = "CBLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
                    PERFORM LIST-SOURCE-ELEMENTS-SECTION
           END-IF.

     ‚* SICHERN ALLE IM PGM VORKOMMENDER SECTIONS
           IF       SRCTYPE OF RTVMBRD-REC = "DSPF"
            OR      SRCTYPE OF RTVMBRD-REC = "PRTF"
            OR      SRCTYPE OF RTVMBRD-REC = "PF"
            OR      SRCTYPE OF RTVMBRD-REC = "LF"
                    PERFORM LIST-SOURCE-ELEMENTS-FORMATS
           END-IF.

       LST-SRE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN ALLER SECTIONS
     ‚*--------------------------------------------------------------
       LIST-SOURCE-ELEMENTS-SECTION SECTION.
       LST-SSC-00.

     ‚* IST DIES EIN SECTION-ANFANG? DANN DIESEN MERKEN
           INITIALIZE SUCH-TAB.
           MOVE     " SECTION."         TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
     ‚*             JETZT DIE SECTION SICHERN
                    INITIALIZE MAPDTA-REC
                    SET  MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    MOVE SRCLIB-WRK    TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE SRCRCN-WRK    TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE SRCMBR-WRK    TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE SRCSEQ OF QSETSRC-P TO
                         MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE "SECTION"     TO MAP-KEY OF MAPDTA-REC(38:7)
                    MOVE SOURCE-WRK(1:E-POS(1)) TO
                         MAP-REC OF MAPDTA-REC
                    MOVE MAP-WRITE     TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
                    SET SECTION-PTR TO MAP-PTR OF MAPDTA-REC
     ‚*             JETZT POINTER WIEDER ZURÜCKSETZEN
                    SET MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
           END-IF.

       LST-SSC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN ALLER FORMATE
     ‚*--------------------------------------------------------------
       LIST-SOURCE-ELEMENTS-FORMATS SECTION.
       LST-SFO-00.

     ‚* FORMAT MERKEN
           IF       SRCDTA OF QSETSRC-P(17:1)    = "R"
            AND     SRCDTA OF QSETSRC-P(7:1) NOT = "*"
     ‚*             JETZT DIE SECTION SICHERN
                    INITIALIZE MAPDTA-REC
                    SET  MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    MOVE SRCLIB-WRK    TO MAP-KEY OF MAPDTA-REC(1:10)
                    MOVE SRCRCN-WRK    TO MAP-KEY OF MAPDTA-REC(11:10)
                    MOVE SRCMBR-WRK    TO MAP-KEY OF MAPDTA-REC(21:10)
                    MOVE SRCSEQ OF QSETSRC-P TO
                         MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE "FORMAT"      TO MAP-KEY OF MAPDTA-REC(38:7)
                    MOVE SRCDTA OF QSETSRC-P(19:) TO
                         MAP-REC OF MAPDTA-REC
                    MOVE MAP-WRITE     TO MAP-ACT OF MAPDTA-REC
                    PERFORM COPY-CALL-MAPDTA
                    SET SECTION-PTR TO MAP-PTR OF MAPDTA-REC
     ‚*             JETZT POINTER WIEDER ZURÜCKSETZEN
                    SET MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
           END-IF.

       LST-SFO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE SOURCE AUS MAPDTA WIEDER IN DIE SOURCE ZURÜCKSCHREIBEN
     ‚*--------------------------------------------------------------
       SOURCE-SAVE SECTION.
       SRC-SAV-00.

           INITIALIZE RET-CODE.
     ‚* SOURCE SICHERN
           IF       AENDERN-WRK = ZEROES
                    GO TO SRC-SAV-90
           END-IF.

     ‚* PRÜFEN OB DIE TEILDATEI VORHANDEN IST UND OB SIE VOM RICHTIGEN
     ‚* TYP IST
           MOVE     X-SRCLIB TO CHECK-LIB.
           MOVE     X-SRCRCN TO CHECK-RCN.
           MOVE     X-SRCMBR TO CHECK-MBR.
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO RET-CODE
                    GO TO SRC-SAV-90
           END-IF.

     ‚* NUR SICHERN, WENN WAS GEÄNDERT WURDE
           IF       CHANGE-SRC-AKTIV  NOT = ZEROES
            AND     CFFK15 OF CFG-CPY NOT = ZEROES
                    CALL "CRP0014" USING X-SRCLIB
                                         X-SRCRCN
                                         X-SRCMBR
                    END-CALL
           END-IF.

     ‚* SETZEN OVRWRT FÜR QCBLSRC UND ÖFFNEN DER DATEI
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    MOVE "2" TO RET-CODE
                    GO TO SRC-SAV-90
           END-IF.

     ‚* SETZEN EINES LOCKS
     ‚*€   MOVE     X-SRCLIB TO LCK-LIB.
     ‚*€   MOVE     X-SRCRCN TO LCK-RCN.
     ‚*€   MOVE     X-SRCMBR TO LCK-MBR.
     ‚*€   PERFORM  SET-MBR-LOCK.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     OUTPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO SRC-SAV-80
           END-IF.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO START-ERFOLGT.

       SRC-SAV-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE   1                  TO START-ERFOLGT
                    MOVE   ZEROES             TO MAP-LFDN OF MAPDTA-REC
                    MOVE   MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    PERFORM  COPY-CALL-MAPDTA
           ELSE
                    MOVE   MAP-READ-NEXT      TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       RET-CODE NOT = SPACES
                    GO TO SRC-SAV-80
           END-IF.

     ‚* PASST DER KEY?
           IF       MAP-KEY OF MAPDTA-REC(1:10)  NOT = X-SRCLIB
            OR      MAP-KEY OF MAPDTA-REC(11:10) NOT = X-SRCRCN
            OR      MAP-KEY OF MAPDTA-REC(21:10) NOT = X-SRCMBR
            OR      MAP-KEY OF MAPDTA-REC(38:6)  NOT = X'FFFFFFFFFFFF'
            OR      MAP-KEY OF MAPDTA-REC(51:)   NOT = SPACES
                    GO TO SRC-SAV-20
           END-IF.

     ‚* JETZT BRAUCHEN WIR NUR NOCH NEU SCHREIBEN
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-P.
           WRITE    QSETSRC-P.

           GO TO    SRC-SAV-20.

       SRC-SAV-80.
           MOVE     SPACES TO RET-CODE.
           MOVE     ZEROES TO CHANGE-SRC-AKTIV.
           CLOSE    QSETSRC-DP.

     ‚* LOCK UND OVERWRITE ENTFERNEN
     ‚*€   IF       LCK-SET NOT = ZEROES
     ‚*€            PERFORM DLT-MBR-LOCK
     ‚*€   END-IF.
           PERFORM  DLT-OVR-MBR.

       SRC-SAV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE JETZT ÄNDERN
     ‚*--------------------------------------------------------------
       SOURCE-CHANGE SECTION.
       SRC-CHG-00.

     ‚* JETZT DIE 01. ZEILE AKTUALISIEREN
           IF       IN50 OF OUTXX = AN
                    MOVE    SRCDTA01  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ01H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN70 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 02. ZEILE AKTUALISIEREN
           IF       IN51 OF OUTXX = AN
                    MOVE    SRCDTA02  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ02H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN71 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 03. ZEILE AKTUALISIEREN
           IF       IN52 OF OUTXX = AN
                    MOVE    SRCDTA03  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ03H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN72 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 04. ZEILE AKTUALISIEREN
           IF       IN53 OF OUTXX = AN
                    MOVE    SRCDTA04  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ04H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN73 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 05. ZEILE AKTUALISIEREN
           IF       IN54 OF OUTXX = AN
                    MOVE    SRCDTA05  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ05H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN74 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 06. ZEILE AKTUALISIEREN
           IF       IN55 OF OUTXX = AN
                    MOVE    SRCDTA06  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ06H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN75 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 07. ZEILE AKTUALISIEREN
           IF       IN56 OF OUTXX = AN
                    MOVE    SRCDTA07  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ07H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN76 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 08. ZEILE AKTUALISIEREN
           IF       IN57 OF OUTXX = AN
                    MOVE    SRCDTA08  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ08H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN77 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 09. ZEILE AKTUALISIEREN
           IF       IN58 OF OUTXX = AN
                    MOVE    SRCDTA09  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ09H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN78 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 10. ZEILE AKTUALISIEREN
           IF       IN59 OF OUTXX = AN
                    MOVE    SRCDTA10  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ10H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN79 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 11. ZEILE AKTUALISIEREN
           IF       IN60 OF OUTXX = AN
                    MOVE    SRCDTA11  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ11H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN80 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 12. ZEILE AKTUALISIEREN
           IF       IN61 OF OUTXX = AN
                    MOVE    SRCDTA12  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ12H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN81 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 13. ZEILE AKTUALISIEREN
           IF       IN62 OF OUTXX = AN
                    MOVE    SRCDTA13  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ13H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN82 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 14. ZEILE AKTUALISIEREN
           IF       IN63 OF OUTXX = AN
                    MOVE    SRCDTA14  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ14H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN83 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 15. ZEILE AKTUALISIEREN
           IF       IN64 OF OUTXX = AN
                    MOVE    SRCDTA15  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ15H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN84 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 16. ZEILE AKTUALISIEREN
           IF       IN65 OF OUTXX = AN
                    MOVE    SRCDTA16  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ16H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN85 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 17. ZEILE AKTUALISIEREN
           IF       IN66 OF OUTXX = AN
                    MOVE    SRCDTA17  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ17H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN86 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 18. ZEILE AKTUALISIEREN
           IF       IN67 OF OUTXX = AN
                    MOVE    SRCDTA18  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ18H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN87 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 19. ZEILE AKTUALISIEREN
           IF       IN68 OF OUTXX = AN
                    MOVE    SRCDTA19  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ19H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN88 OF OUTXX
                    END-IF
           END-IF.
     ‚* JETZT DIE 20. ZEILE AKTUALISIEREN
           IF       IN69 OF OUTXX = AN
                    MOVE    SRCDTA20  OF FORMAT1 TO SOURCE-WRK
                    MOVE    SRCSEQ20H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM GET-ZEILE
                    PERFORM SET-ZEILE
                    IF   RET-COMMAND(1:1) = "I"
                         MOVE AN TO IN89 OF OUTXX
                    END-IF
           END-IF.

       SRC-CHG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN UND AUSFÜHREN DER ZEILENBEFEHLE
     ‚*--------------------------------------------------------------
       SOURCE-ZEILENBEFEHL SECTION.
       SRC-ZBF-00.

           INITIALIZE CHANGE-USRCMD-AKTIV.
     ‚* JETZT DIE 01. ZEILE AKTUALISIEREN
           IF       IN70 OF OUTXX = AN
                    MOVE    SRCSEQ01  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ01H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 02. ZEILE AKTUALISIEREN
           IF       IN71 OF OUTXX = AN
                    MOVE    SRCSEQ02  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ02H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 03. ZEILE AKTUALISIEREN
           IF       IN72 OF OUTXX = AN
                    MOVE    SRCSEQ03  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ03H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 04. ZEILE AKTUALISIEREN
           IF       IN73 OF OUTXX = AN
                    MOVE    SRCSEQ04  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ04H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 05. ZEILE AKTUALISIEREN
           IF       IN74 OF OUTXX = AN
                    MOVE    SRCSEQ05  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ05H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 06. ZEILE AKTUALISIEREN
           IF       IN75 OF OUTXX = AN
                    MOVE    SRCSEQ06  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ06H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 07. ZEILE AKTUALISIEREN
           IF       IN76 OF OUTXX = AN
                    MOVE    SRCSEQ07  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ07H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 08. ZEILE AKTUALISIEREN
           IF       IN77 OF OUTXX = AN
                    MOVE    SRCSEQ08  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ08H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 09. ZEILE AKTUALISIEREN
           IF       IN78 OF OUTXX = AN
                    MOVE    SRCSEQ09  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ09H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 10. ZEILE AKTUALISIEREN
           IF       IN79 OF OUTXX = AN
                    MOVE    SRCSEQ10  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ10H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 11. ZEILE AKTUALISIEREN
           IF       IN80 OF OUTXX = AN
                    MOVE    SRCSEQ11  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ11H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 12. ZEILE AKTUALISIEREN
           IF       IN81 OF OUTXX = AN
                    MOVE    SRCSEQ12  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ12H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 13. ZEILE AKTUALISIEREN
           IF       IN82 OF OUTXX = AN
                    MOVE    SRCSEQ13  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ13H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 14. ZEILE AKTUALISIEREN
           IF       IN83 OF OUTXX = AN
                    MOVE    SRCSEQ14  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ14H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 15. ZEILE AKTUALISIEREN
           IF       IN84 OF OUTXX = AN
                    MOVE    SRCSEQ15  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ15H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 16. ZEILE AKTUALISIEREN
           IF       IN85 OF OUTXX = AN
                    MOVE    SRCSEQ16  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ16H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 17. ZEILE AKTUALISIEREN
           IF       IN86 OF OUTXX = AN
                    MOVE    SRCSEQ17  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ17H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 18. ZEILE AKTUALISIEREN
           IF       IN87 OF OUTXX = AN
                    MOVE    SRCSEQ18  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ18H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 19. ZEILE AKTUALISIEREN
           IF       IN88 OF OUTXX = AN
                    MOVE    SRCSEQ19  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ19H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.
     ‚* JETZT DIE 20. ZEILE AKTUALISIEREN
           IF       IN89 OF OUTXX = AN
                    MOVE    SRCSEQ20  OF FORMAT1 TO SRCSEQ-WRK
                    MOVE    SRCSEQ20H OF FORMAT1 TO GET-ZEILE-WRK
                    PERFORM ADD-USRCMD-TAB
           END-IF.

       SRC-ZBF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN EINES ZEILENBEFEHLS (SOFERN KOMPLETT)
     ‚*--------------------------------------------------------------
       SOURCE-ZEILENBEFEHL-EXEC SECTION.
       SRC-ZBE-00.

           INITIALIZE ERRFLD-WRK.
           INITIALIZE RET-COMMAND.
           CALL     "CRP0013" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    PTR-MAIN OF PTRMAP-REC
                                    USRCMD-TAB
                                    TT-VALUE
                                    OPEN-COMMAND
                                    CHANGE-SRC-AKTIV
                                    RET-COMMAND
                                    ERRFLD-WRK
           END-CALL.

       SRC-ZBE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE SOURCE JETZT SICHERN
     ‚*--------------------------------------------------------------
       SOURCE-KOPIEREN SECTION.
       SRC-CPY-00.

           CALL     "CRP0016" USING TEMP-PTR
                                    RESTORE-PTR
           END-CALL.

     ‚* JETZT DIE SOURCE IM POINTER SPEICHERN
           SET      PTR-ADD TO TRUE.
           SET      PTR-INPUT OF PTRMAP-REC TO RESTORE-PTR.
           MOVE     "RESTORE-"  TO PTR-KEY OF PTRMAP-REC.
           MOVE     OPN-SRC-MAX OF OPN-SRC-TAB TO
                    PTR-KEY OF PTRMAP-REC(9:).
           PERFORM  COPY-PTR-MAPPING.

       SRC-CPY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* STARTEN EINES CODEREVIEWS FÜR DIESE SOURCE
     ‚*--------------------------------------------------------------
       START-CODEREVIEW SECTION.
       STR-CRV-00.

           INITIALIZE RET-CODE.
           INITIALIZE CR-LFDN, CR-VORG.
     ‚* PRÜFEN OB CR ERLAUBT IST, UM EINEN ENDRQS ODER SIGNOFF
     ‚* VORZUBEUGEN UND NUR DAS WINDOW ANZUZEIGEN
           MOVE     "CRP1051" TO CR-PGM.
           MOVE     "CHKSRC"  TO CR-TYP.
           CALL     "CFP9010" USING T-USER
                                    CR-PGM
                                    CR-TYP
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    CALL "CFP9101" USING CR-PGM
                                         CR-TYP
                    END-CALL
                    GO TO STR-CRV-90
           END-IF.

     ‚* STARTEN EINES CR-VORGANGS
           CALL     "CRP0002" USING SRCLIB-WRK
                                    SRCRCN-WRK
                                    SRCMBR-WRK
                                    CR-LFDN
                                    CR-VORG
           END-CALL.
           IF       CR-LFDN NOT = ZEROES
     ‚*             JETZT DEN CODE-REVIEW STARTEN
                    CALL CR-PGM    USING CR-LFDN
                                         SRCLIB-WRK
                                         SRCRCN-WRK
                                         SRCMBR-WRK
                    END-CALL
           END-IF.

     ‚* JETZT ALLE EINTRÄGE DES CODEREVIEW IN MAPDTA ÜBERNEHMEN
           INITIALIZE PCHKSRCF OF PCHKSRC-L.
           MOVE     CR-VORG             TO CRVORG OF PCHKSRC-L.
           MOVE     ZEROES              TO CRSECN OF PCHKSRC-L.
           MOVE     SPACES              TO CRFID  OF PCHKSRC-L.
           START    PCHKSRC-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO STR-CRV-90
           END-IF.

       STR-CRV-20.
           READ     PCHKSRC-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO STR-CRV-90
           END-IF.

           IF       CRVORG OF PCHKSRC-L NOT = CR-VORG
                    GO TO STR-CRV-90
           END-IF.

     ‚* EINTRÄGE OHNE FEHLER IGNORIEREN
           IF       CRFID  OF PCHKSRC-L     = SPACES
                    GO TO STR-CRV-20
           END-IF.

           INITIALIZE QSETSRC OF SRC-WRK.
           MOVE     CRSECN OF PCHKSRC-L TO SRCSEQ OF SRC-WRK.
           MOVE     ZEROES              TO SRCDAT OF SRC-WRK.
           MOVE     "##CR"              TO SRCDTA OF SRC-WRK(1:4).
           MOVE     RED-RI-HEX-WRK      TO SRCDTA OF SRC-WRK(6:1).
           MOVE     "*"                 TO SRCDTA OF SRC-WRK(7:1).
           MOVE     "CHKSRC:"           TO SRCDTA OF SRC-WRK(9:7).
           MOVE     CRFTXT OF PCHKSRC-L TO SRCDTA OF SRC-WRK(17:).

           INITIALIZE MAPDTA-REC.
           MOVE     CRLIB  OF PCHKSRC-L TO
                    MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     CRFILE OF PCHKSRC-L TO
                    MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     CRMBR  OF PCHKSRC-L TO
                    MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     CRSECN OF PCHKSRC-L TO
                    MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     "CHKSRC"            TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     SRC-WRK             TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

           GO TO    STR-CRV-20.

       STR-CRV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSCHIEDENE OPTIONEN AKTIVIEREN DIE FÜR DEN CR GEEIGNET SIND
     ‚*--------------------------------------------------------------
       START-CRMODE SECTION.
       STR-CRM-00.

           INITIALIZE CR-MODE-SCHALTER.
     ‚* PRÜFEN OB AKTIVIERT ODER DEAKTIVIERT WERDEN SOLL
           INITIALIZE POS-TMP.
           INSPECT  SEUCMD-WRK TALLYING POS-TMP FOR CHARACTERS
                    BEFORE " ON ".
           IF       POS-TMP = LENGTH OF SEUCMD-WRK
                    INITIALIZE POS-TMP
                    INSPECT  SEUCMD-WRK TALLYING POS-TMP FOR CHARACTERS
                             BEFORE " OFF "
                    IF   POS-TMP = LENGTH OF SEUCMD-WRK
                         MOVE 2      TO CR-MODE-SCHALTER
                    ELSE
                         MOVE ZEROES TO CR-MODE-SCHALTER
                    END-IF
           ELSE
                    MOVE 1 TO CR-MODE-SCHALTER
           END-IF.

     ‚* SOLL EIN- BZW. AUSGESCHALTET WERDEN OBWOHL ES DAS SCHON IST?
           IF       CR-MODE-SCHALTER = CR-MODE-AKTIV
                    GO TO STR-CRM-90
           END-IF.

           IF       CR-MODE-AKTIV = ZEROES
                    MOVE     1        TO CR-MODE-AKTIV
     ‚*             EINSTELLUNGEN SICHERN
                    INITIALIZE CFG-SAV
                    MOVE     PCONFIGF OF CFG-CPY TO CFG-SAV
     ‚*             DIE LINIEN BEI 12 (-> 20) UND 21 (-> 29) PLATZIEREN
                    MOVE     12       TO CFNM03 OF CFG-CPY
                    MOVE     21       TO CFNM04 OF CFG-CPY
     ‚*             DIE 24X80-LINIE UND NOTIZEN AKTIVIEREN
                    MOVE     1        TO CFFK10 OF CFG-CPY
                    MOVE     1        TO CFFK13 OF CFG-CPY
           ELSE
                    MOVE     CFG-SAV  TO CFG-CPY
                    MOVE     ZEROES   TO CR-MODE-AKTIV
           END-IF.

       STR-CRM-90.
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
     ‚*--------------------------------------------------------------
     ‚* DIE SOURCE IN RUNSQL ANSEHEN
     ‚*--------------------------------------------------------------
       START-RUNSQL SECTION.
       STR-RSQ-00.

           INITIALIZE RET-CODE.
     ‚* RECORD BEFÜLLEN
           INITIALIZE RUNSQL-REC.
           MOVE     "SELECT * FROM APG/QSETSRC" TO
                    SQLANF OF RUNSQL-REC.
           MOVE     "*"       TO OUTTYP OF RUNSQL-REC.

     ‚* CL JETZT AUSFÜHREN
           CALL     "CLP0048" USING RUNSQL-REC
                                    RET-CODE
           END-CALL.

       STR-RSQ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HERMES ZUM TAGGEN DES PROGRAMMS AUFRUFEN
     ‚*--------------------------------------------------------------
       START-HERMES SECTION.
       STR-HRM-00.

           INITIALIZE F3, F12.
           INITIALIZE HMP-LIB, HMP-PGM.
     ‚* DATEN ÜBERNEHMEN, HERMES LÄUFT AUF "PROGRAMMBASIS" NICHT AUF
     ‚* SOURCEN. DAHER MBR IN PGM SCHIEBEN
           MOVE     X-SRCLIB TO HMP-LIB.
           MOVE     X-SRCMBR TO HMP-PGM.
     ‚* IST DAS EINE LFS-LIB, DANN GGF. DAS "SRC" DURCH "PGM" ERSETZEN
           IF       X-SRCLIB(9:)  = SPACES
            AND     X-SRCLIB(8:1) IS NUMERIC
            AND     X-SRCLIB(4:3) = "SRC"
                    MOVE "PGM" TO HMP-LIB(4:3)
           END-IF.

     ‚* JETZT HERMES AUFRUFEN
           CALL     "HMP0003" USING HMP-LIB
                                    HMP-PGM
                                    PGM-WRK
                                    F3
                                    F12
           END-CALL.

       STR-HRM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DRUCKEN DER SOURCE
     ‚*--------------------------------------------------------------
       START-PRINTING SECTION.
       STR-PRT-00.

           INITIALIZE OUTQ-DUMMY.
           CALL     "DRP0010" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    OUTQ-DUMMY
           END-CALL.

       STR-PRT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BEGINN DES SCRAMBLEN DER SOURCE
     ‚*--------------------------------------------------------------
       START-SCRAMBLEN SECTION.
       STR-SCM-00.

           MOVE     "ONLYCOPY" TO SCM-AKTION.
           CALL     "CMP0007" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    SCM-AKTION
                                    SCMKEY-WRK
                                    SOURCE-PTR
                                    SCM-PTR
           END-CALL.

     ‚* POINTER INITIALISIEREN
           IF       SOURCE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SOURCE-PTR TO SCM-PTR.

       STR-SCM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* KOMMANDOZEILE AUFRUFEN
     ‚*--------------------------------------------------------------
       KOMMANDOZEILE SECTION.
       CMD-LIN-00.

           CALL     "XXP2001"
           END-CALL.

       CMD-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WINDOW ZUR PFLEGE DES SYNTAXHIGHLIGHTING
     ‚*--------------------------------------------------------------
       SHOW-SYNTAX-COMMAND SECTION.
       SHW-SCM-00.

           INITIALIZE F12.
     ‚* ALLE SYSNTAX-EINTRÄGE ANZEIGEN
           CALL     "CRP0004" USING SYTYP-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

     ‚* DA ETWAS GEÄNDERT WERDEN KONNTE, NUN NEU EINLESEN
           PERFORM  GET-SYNTAX-HGH-CMD.

       SHW-SCM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN VON INFORMATIONSTEXTEN
     ‚*--------------------------------------------------------------
       ANZEIGE-PGM-INFOS SECTION.
       ANZ-PGM-00.

           INITIALIZE F12.
     ‚* INFORMATIONEN ANZEIGEN
           CALL     "CFP0009" USING PGM-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

       ANZ-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

     ‚* LOCK ENTFERNEN SOFERN GESETZT
           IF       LCK-SET NOT = ZEROES
                    PERFORM DLT-MBR-LOCK
           END-IF.

     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     SRCLIB-WRK   TO OBJLIB   OF RTVMBRD-REC.
           MOVE     SRCRCN-WRK   TO FILENAME OF RTVMBRD-REC.
           MOVE     SRCMBR-WRK   TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

     ‚* POINTER INITIALISIEREN
           IF       SOURCE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SOURCE-PTR TO NULL.

           IF       RESTORE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO RESTORE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      RESTORE-PTR TO NULL.

           IF       SECTION-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SECTION-PTR TO NULL.

           IF       NOTIZEN-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO NOTIZEN-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      NOTIZEN-PTR TO NULL.

           IF       HIGHLIGHT-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO HIGHLIGHT-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      HIGHLIGHT-PTR TO NULL.

           IF       SPRUNG-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SPRUNG-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SPRUNG-PTR TO NULL.

           IF       LAST-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO LAST-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      LAST-PTR TO NULL.

     ‚* VARIABLEN INITIALISIEREN
           MOVE     AUS TO INXX.
           INITIALIZE START-ZEILE.
           INITIALIZE AKT-LFDN-MIN, AKT-LFDN-MAX.
           INITIALIZE AKT-ZEILE-MIN, AKT-ZEILE-MAX.
           INITIALIZE NEWSRCLIB-WRK.
           INITIALIZE NEWSRCRCN-WRK.
           INITIALIZE NEWSRCMBR-WRK.
           INITIALIZE SRCLEN-WRK.
           INITIALIZE FNDSTR-WRK.
           INITIALIZE FIND-STARTZEILE.
           INITIALIZE FIND-GEFUNDEN.
           INITIALIZE FIND-GEFUNDEN-POS.
           INITIALIZE DEBUG-WRK.
           INITIALIZE CR-MODE-AKTIV.
           INITIALIZE CHANGE-SRC-AKTIV.
           INITIALIZE TT-VALUE.
           INITIALIZE OPEN-COMMAND.
           INITIALIZE GET-ZEILE-HLP.
           INITIALIZE F10-MODUS.
     ‚* API-VARIABLEN INITIALISIEREN
           PERFORM  API-CLEAR-INPUT.
           PERFORM  API-CLEAR-OUTPUT.

     ‚* INT. TABELLEN INITIALISIEREN
           INITIALIZE IND.
           PERFORM  100 TIMES
                    ADD  1 TO IND
                    MOVE SPACES TO COPY-MOD-LIB OF COPY-REC(IND)
                    MOVE SPACES TO COPY-MOD-RCN OF COPY-REC(IND)
                    MOVE SPACES TO COPY-MOD-MBR OF COPY-REC(IND)
           END-PERFORM.
           INITIALIZE IND.
           PERFORM  500 TIMES
                    ADD  1 TO IND
                    MOVE ZEROES TO SRCSEQ       OF USRCMD-TAB(IND)
                    MOVE SPACES TO SRCCMD       OF USRCMD-TAB(IND)
                    MOVE ZEROES TO IS-DIRECT    OF USRCMD-TAB(IND)
                    MOVE ZEROES TO IS-BLOCK     OF USRCMD-TAB(IND)
                    MOVE ZEROES TO IS-OPEN      OF USRCMD-TAB(IND)
                    MOVE ZEROES TO IS-TARGET    OF USRCMD-TAB(IND)
                    MOVE ZEROES TO NEED-TARGET  OF USRCMD-TAB(IND)
                    MOVE ZEROES TO TARGET-SEQ   OF USRCMD-TAB(IND)
           END-PERFORM.

           MOVE     ZEROES TO AENDERN-WRK.
           MOVE     ZEROES TO AENDERN-TMP.
           IF       X-OPTION NOT = "5"
                    MOVE 1 TO AENDERN-WRK
           END-IF.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* KONFIGURATION ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H101"              TO CFID   OF CFG-CPY.
           MOVE     T-USER              TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
     ‚*             SYNTAX-HIGHLIGHTING UND ANDERE DEFAULTS
                    MOVE  1           TO CFFK01 OF CFG-CPY
                    MOVE  1           TO CFFK03 OF CFG-CPY
                    MOVE  ZEROES      TO CFFK04 OF CFG-CPY
                    MOVE  1           TO CFFK13 OF CFG-CPY
                    MOVE  "QTMPSRC"   TO CFDAT2 OF CFG-CPY
                    MOVE  "APG"       TO CFDAT3 OF CFG-CPY
                    MOVE  "QTEMP"     TO CFDAT4 OF CFG-CPY
                    MOVE  "*SAME"     TO CFDAT5 OF CFG-CPY
                    MOVE 0,01         TO CFNK01 OF CFG-CPY
           END-IF.

     ‚* FARBEN EINMALIG ERMITTELN
           PERFORM  GET-COLOUR.

     ‚* ERMITTELN DER SYNTAX-HIGHLIGHTING-WÖRTER
           INITIALIZE COBOL-CMD-TAB.
           INITIALIZE COBOL-SYS-TAB.
           INITIALIZE COBOL-EUP-TAB.
           INITIALIZE COBOL-OTH-TAB.
           PERFORM  GET-SYNTAX-HGH-CMD.

     ‚* DEN TT-WERT VORBELEGEN, ABER NICHT BEI LFS
     ‚*  -> LFSSRCV6 BZW. CLFSI10299
           IF       X-SRCLIB(1:3) NOT = "LFS"
            AND     X-SRCLIB(2:3) NOT = "LFS"
                    MOVE "##" TO TT-VALUE(1:2)
                    IF   X-SRCLIB(1:1) = "C"
                     AND X-SRCLIB(6:5) IS NUMERIC
                         MOVE X-SRCLIB(2:3) TO TT-VALUE(3:3)
                    ELSE
                         MOVE X-SRCLIB(1:3) TO TT-VALUE(3:3)
                    END-IF
           END-IF.

       INI-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE VARIABLEN FÜRS BEENDEN LÖSCHEN
     ‚*--------------------------------------------------------------
       CLEAR-VAR SECTION.
       CLR-VAR-00.

     ‚* POINTER JETZT WIEDER FREIGEBEN
           IF       SOURCE-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       RESTORE-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO RESTORE-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       SECTION-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       NOTIZEN-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO NOTIZEN-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       HIGHLIGHT-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO HIGHLIGHT-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       SPRUNG-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO SPRUNG-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
           IF       LAST-PTR NOT = NULL
                    INITIALIZE MAPDTA-REC
                    MOVE    MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    SET     MAP-PTR OF MAPDTA-REC TO LAST-PTR
                    PERFORM COPY-CALL-MAPDTA
           END-IF.
     ‚* ZENTRALE POINTERVERWATLUNG LEEREN
           SET      PTR-CLEAR TO TRUE.
           PERFORM  COPY-PTR-MAPPING.

           SET      SOURCE-PTR      TO NULL.
           SET      RESTORE-PTR     TO NULL.
           SET      SECTION-PTR     TO NULL.
           SET      NOTIZEN-PTR     TO NULL.
           SET      SPRUNG-PTR      TO NULL.
           SET      LAST-PTR        TO NULL.
           SET      HIGHLIGHT-PTR   TO NULL.

       CLR-VAR-90.
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
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER MAPDTA-INFOS
     ‚*--------------------------------------------------------------
       DLT-MAP-INFO SECTION.
       DLT-MAI-00.

     ‚* KEY FÜLLEN UND AUFSETZEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCZEI-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
     ‚* DAMIT DIESE ZEILEN IN DER SORTIERUNG GANZ HINTEN ERSCHEINEN
           MOVE     X'FFFFFFFFFFFF'     TO MAP-KEY OF MAPDTA-REC(38:6).
     ‚* CPY-MODUL?
           IF       SRCLIB-WRK NOT = X-SRCLIB
            OR      SRCRCN-WRK NOT = X-SRCRCN
            OR      SRCMBR-WRK NOT = X-SRCMBR
     ‚*             FÜR DIE SORTIERUNG ZEILENNUMMER DER ORIGINALZEILE
     ‚*             WIEDER ÜBERSCHREIBEN
                    MOVE SRCZEI-WRK TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE SRCLIB-WRK TO MAP-KEY OF MAPDTA-REC(51:10)
                    MOVE SRCRCN-WRK TO MAP-KEY OF MAPDTA-REC(61:10)
                    MOVE SRCMBR-WRK TO MAP-KEY OF MAPDTA-REC(71:10)
           END-IF.
           MOVE     MAP-READ-NEXT  TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO DLT-MAI-90
           END-IF.

       DLT-MAI-20.
     ‚* KEY PRÜFEN
           IF       MAP-KEY OF MAPDTA-REC(1:10)  NOT = X-SRCLIB
            OR      MAP-KEY OF MAPDTA-REC(11:10) NOT = X-SRCRCN
            OR      MAP-KEY OF MAPDTA-REC(21:10) NOT = X-SRCMBR
                    GO TO DLT-MAI-90
           END-IF.

     ‚* FALL COPY-MODUL, DIESES AUCH PRÜFEN
           IF       SRCLIB-WRK NOT = X-SRCLIB
            OR      SRCRCN-WRK NOT = X-SRCRCN
            OR      SRCMBR-WRK NOT = X-SRCMBR
                    IF   MAP-KEY OF MAPDTA-REC(51:10) NOT = SRCLIB-WRK
                     OR  MAP-KEY OF MAPDTA-REC(61:10) NOT = SRCRCN-WRK
                     OR  MAP-KEY OF MAPDTA-REC(71:10) NOT = SRCMBR-WRK
                         GO TO DLT-MAI-90
                    END-IF
           END-IF.

     ‚* HIER LÖSCHEN
           MOVE     MAP-DELETE    TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO DLT-MAI-90
           END-IF.

     ‚* JETZT DEN NÄCHSTEN LESEN
           MOVE     MAP-READ-NEXT TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO DLT-MAI-90
           END-IF.

           GO TO    DLT-MAI-20.

       DLT-MAI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ENTSPERREN DER SOURCE
     ‚*--------------------------------------------------------------
       DLT-MBR-LOCK SECTION.
       DLT-LCK-00.

           INITIALIZE RET-CODE.
     ‚* JETZT ENTSPERREN DER SOURCE
           MOVE     ZEROES TO LCK-AKTION.
           CALL     "CRP0008" USING LCK-LIB
                                    LCK-RCN
                                    LCK-MBR
                                    LCK-AKTION
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE ZEROES TO LCK-SET
           END-IF.

       DLT-LCK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FARBWERTE ANZEIGEN
     ‚*--------------------------------------------------------------
       GET-COLOUR SECTION.
       GET-COL-00.

     ‚* ALLE WERTE ZURÜCKSETZEN
           INITIALIZE GREEN-HEX-WRK.
           INITIALIZE WHITE-HEX-WRK.
           INITIALIZE RED-RI-HEX-WRK.
           INITIALIZE GREEN-RI-HEX-WRK.

     ‚* GRÜNEN FARBWERT ERMITTELN
           MOVE     "GREEN"         TO COLOR-WRK.
           MOVE     SPACES          TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    GREEN-HEX-WRK
           END-CALL.

     ‚* GRÜNEN FARBWERT ERMITTELN
           MOVE     "GREEN"         TO COLOR-WRK.
           MOVE     "1"             TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    GREEN-RI-HEX-WRK
           END-CALL.

     ‚* ROTEN INVERSEN FARBWERT ERMITTELN
           MOVE     "RED"           TO COLOR-WRK.
           MOVE     "1"             TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    RED-RI-HEX-WRK
           END-CALL.

     ‚* WEIßEN FARBWERT ERMITTELN
           MOVE     "WHITE"         TO COLOR-WRK.
           MOVE     SPACES          TO INVERS-WRK.
           MOVE     SPACES          TO BLINK-WRK.
           MOVE     SPACES          TO UNDERLINE-WRK.
           CALL     "HLP0011" USING COLOR-WRK
                                    INVERS-WRK
                                    BLINK-WRK
                                    UNDERLINE-WRK
                                    WHITE-HEX-WRK
           END-CALL.

       GET-COL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER SYNTAX-HIGHLIGHTING-WÖRTER
     ‚*--------------------------------------------------------------
       GET-SYNTAX-HGH-CMD SECTION.
       GET-SHC-00.

           INITIALIZE RELOAD-WRK.

       GET-SHC-10.
           INITIALIZE SYTYP-WRK.
     ‚* HIGHLIGHT-POINTER LEEREN
           IF       HIGHLIGHT-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO HIGHLIGHT-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      HIGHLIGHT-PTR TO NULL.

     ‚* ERMITTELN DES ZU VERWENDENDEN SOURCE-TYPEN
           CALL     "CRP0020" USING T-USER
                                    SRCTYPE OF RTVMBRD-REC
                                    SYTYP-WRK
           END-CALL.

           INITIALIZE HGH-DATEN.
     ‚* ERMITTELN DER SYNTAX-EINTRÄGE
           INITIALIZE PCHKHGHF OF PCHKHGH-L.
           MOVE     T-USER              TO SYUSER OF PCHKHGH-L.
           MOVE     SPACES              TO SYTYP  OF PCHKHGH-L.
           MOVE     ZEROES              TO SYLEN  OF PCHKHGH-L.
           MOVE     SPACES              TO SYCMD  OF PCHKHGH-L.
           START    PCHKHGH-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    IF   HGH-DATEN       = ZEROES
                     AND RELOAD-WRK      = ZEROES
                         MOVE 1 TO RELOAD-WRK
                         CALL "CRP0003"
                         END-CALL
                         GO TO GET-SHC-10
                    END-IF
                    GO TO GET-SHC-90
           END-IF.

       GET-SHC-20.
           READ     PCHKHGH-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    IF   HGH-DATEN       = ZEROES
                     AND RELOAD-WRK      = ZEROES
                         MOVE 1 TO RELOAD-WRK
                         CALL "CRP0003"
                         END-CALL
                         GO TO GET-SHC-10
                    END-IF
                    GO TO GET-SHC-90
           END-IF.

           IF       SYUSER OF PCHKHGH-L NOT = T-USER
                    IF   HGH-DATEN       = ZEROES
                     AND RELOAD-WRK      = ZEROES
                         MOVE 1 TO RELOAD-WRK
                         CALL "CRP0003"
                         END-CALL
                         GO TO GET-SHC-10
                    END-IF
                    GO TO GET-SHC-90
           END-IF.

     ‚* EINTRAG AKTIV?
           IF       SYAKT  OF PCHKHGH-L     = ZEROES
                    GO TO GET-SHC-20
           END-IF.

           MOVE     1 TO HGH-DATEN.
     ‚* MAPDTA-POINTER FÜLLEN
           INITIALIZE MAPDTA-REC.
           EVALUATE SYTYP  OF PCHKHGH-L
                    WHEN SYTYP-WRK
                         MOVE SYTYP-WRK  TO MAP-KEY OF MAPDTA-REC(1:20)
                    WHEN "E+P"
                         MOVE "E+P"      TO MAP-KEY OF MAPDTA-REC(1:20)
                    WHEN "OTHER"
                         MOVE "OTHER"    TO MAP-KEY OF MAPDTA-REC(1:20)
           END-EVALUATE.
           MOVE     SYCMD  OF PCHKHGH-L  TO MAP-KEY OF MAPDTA-REC(21:).
           MOVE     SYHEX  OF PCHKHGH-L  TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-WRITE            TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO HIGHLIGHT-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           SET      HIGHLIGHT-PTR       TO MAP-PTR OF MAPDTA-REC.

           GO TO    GET-SHC-20.

       GET-SHC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN EINER BESTIMMTEN ZEILE
     ‚*--------------------------------------------------------------
       GET-ZEILE SECTION.
       GET-ZEI-00.

           INITIALIZE QSETSRC OF SRC-WRK.
     ‚* LESEN MAPDTA UM DIE ZEILE ZU LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     SRCLIB-WRK         TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     SRCRCN-WRK         TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     SRCMBR-WRK         TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     GET-ZEILE-WRK      TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           IF       GET-ZEILE-HLP NOT = ZEROES
                    SET MAP-PTR OF MAPDTA-REC TO RESTORE-PTR
           ELSE
                    SET MAP-PTR OF MAPDTA-REC TO SOURCE-PTR
           END-IF.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE MAP-REC OF MAPDTA-REC(10:) TO SRC-WRK
           END-IF.
           MOVE     ZEROES      TO GET-ZEILE-HLP.

       GET-ZEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN EINES COPY-MODULNAMEN
     ‚*--------------------------------------------------------------
       GET-COPY-MODUL SECTION.
       GET-CMO-00.

           INITIALIZE COPY-DDS-LIB.
           INITIALIZE COPY-DDS-RCN.
           INITIALIZE COPY-DDS-MBR.
           INITIALIZE COPY-DDS-LEN.
           INITIALIZE COPY-DDS-POS.
     ‚* ALS ERSTES MÜSSEN WIR DIE ZEILE NOCHMAL LESEN
           PERFORM  GET-ZEILE.
           IF       RET-CODE NOT = SPACES
                    GO TO GET-CMO-90
           END-IF.

     ‚* ÜBERHAUPT EIN COPY VORHANDEN?
           INITIALIZE COPY-DDS-POS.
           INSPECT  SRCDTA OF SRC-WRK TALLYING COPY-DDS-POS
                    FOR CHARACTERS BEFORE " COPY ".
           IF       COPY-DDS-POS >= LENGTH OF SOURCE-WRK
                    GO TO GET-CMO-90
           END-IF.

     ‚* COPY MODULE STEHEN IN FOLGENDEN FORMEN:
     ‚*  1: COPY WRKPGMPARM OF QCPYSRC.
     ‚*  2: COPY WRKPGMPARM OF APG-QCPYSRC.
     ‚*
     ‚* BEI ERSTEREM IST DIE BIBLIOTHEK GLEICH DER SOURCE-BIBLIOTHEK.
     ‚* BEIM ZWEITEN WIRD DIE BIBLIOTHEK MITGEGEBEN.
     ‚* ALLERDINGS MUSS SICHERGESTELLT WERDEN, DASS FOLGENDER CPY-
     ‚* BEFEHL IGNORIERT WIRD:
     ‚*  3: COPY DDS-ALL-FORMATS OF PBESTTP.
     ‚*  4: COPY DDS-FMT01 OF LGDSP18.
     ‚*
     ‚* IMMER WENN EIN DDS- VORHANDEN IST, DANN ÖFFNEN WIR NICHT DAS
     ‚* COPY-MODUL.

     ‚* ALS ERSTES ERMITTELN WIR UNS WELCHEN DER 4 TYPEN WIR HABEN.
     ‚* WIR SCHLIEßEN 3 UND 4 ALS ERSTE AUS, DA DIESE RELATIV LEICHT
     ‚* ZU FINDEN SIND.
           INITIALIZE COPY-DDS-POS.
           INSPECT  SRCDTA OF SRC-WRK TALLYING COPY-DDS-POS
                    FOR CHARACTERS BEFORE "DDS-".
           IF       COPY-DDS-POS < LENGTH OF SOURCE-WRK
                    GO TO GET-CMO-90
           END-IF.

     ‚* DANACH PRÜFEN WIR OB WIR EIN "-" IM STRING FINDEN. DEN KANN ES
     ‚* JA NUR NOCH IN VERBINDUNG MIT EINER BIBLIOTHEK GEBEN, DA DDS-
     ‚* AUSGESCHLOSSEN WURDE.
           INITIALIZE COPY-DDS-POS.
           INSPECT  SRCDTA OF SRC-WRK TALLYING COPY-DDS-POS
                    FOR CHARACTERS BEFORE "-".
           IF       COPY-DDS-POS >= LENGTH OF SOURCE-WRK
     ‚*             DANN WURDE OHNE BIBLIOTHEK ANGEGEBEN
                    MOVE SRCLIB-WRK TO COPY-DDS-LIB
     ‚*             DATEI KÖNNEN WIR UNS AUCH GLEICH MERKEN
                    INITIALIZE COPY-DDS-LEN
                    INSPECT  SRCDTA OF SRC-WRK TALLYING COPY-DDS-LEN
                             FOR CHARACTERS BEFORE " OF "
                    MOVE SRCDTA OF SRC-WRK(COPY-DDS-LEN + 5:) TO
                         COPY-DDS-RCN
           ELSE
     ‚*             BIBLIOTHEKSNAMEN ERMITTELN, Z.B.:
     ‚*             COPY WRKPGMPARM OF APG-QCPYSRC
                    INITIALIZE COPY-DDS-LEN
                    INSPECT  SRCDTA OF SRC-WRK TALLYING COPY-DDS-LEN
                             FOR CHARACTERS BEFORE " OF "
                    IF   COPY-DDS-LEN NOT = ZEROES
                     AND COPY-DDS-POS NOT = ZEROES
                         COMPUTE POS-TMP = COPY-DDS-LEN + 5
                         COMPUTE LEN-TMP = COPY-DDS-POS + 1 - POS-TMP
                         MOVE SRCDTA OF SRC-WRK(POS-TMP:LEN-TMP) TO
                              COPY-DDS-LIB
     ‚*                  DATEI KÖNNEN WIR UNS AUCH GLEICH MERKEN
                         MOVE SRCDTA OF SRC-WRK(COPY-DDS-POS + 2:) TO
                              COPY-DDS-RCN
                    END-IF
           END-IF.
           INSPECT  COPY-DDS-RCN REPLACING ALL "." BY SPACES.

     ‚* JETZT NOCH DEN CPY-MODUL NAMEN AUSSCHNEIDEN UND WIR HABEN ALLE
     ‚* DATEN DIE WIR BRAUCHEN
           INITIALIZE COPY-DDS-POS.
           INSPECT  SRCDTA OF SRC-WRK TALLYING COPY-DDS-POS FOR
                    LEADING SPACES.
           IF       COPY-DDS-POS = ZEROES
                    MOVE 1 TO COPY-DDS-POS
           ELSE
                    ADD  1 TO COPY-DDS-POS
           END-IF.

     ‚* JETZT WISSEN WIR WO DAS COPY ANFÄNGT -> 5 ZEICHEN HINZUADDIEREN
           ADD      5 TO COPY-DDS-POS.
     ‚* MIT ABSICHT KEIN INITIALIZE, HIER NUTZEN WIR MAL DAS
     ‚* AUTOMATISCHE AUFADDIEREN
           INSPECT  SRCDTA OF SRC-WRK(COPY-DDS-POS:)
                    TALLYING COPY-DDS-POS FOR LEADING SPACES.
           COMPUTE  LEN-TMP = COPY-DDS-LEN - COPY-DDS-POS + 1.
           MOVE     SRCDTA OF SRC-WRK(COPY-DDS-POS:LEN-TMP) TO
                    COPY-DDS-MBR.

     ‚* JETZT SOLLTEN WIR ALLE DATEN HABEN
           IF       COPY-DDS-LIB NOT = SPACES
            OR      COPY-DDS-RCN NOT = SPACES
            OR      COPY-DDS-MBR NOT = SPACES
     ‚*             SOURCE VORHANDEN?
                    MOVE COPY-DDS-LIB TO CHECK-LIB
                    MOVE COPY-DDS-RCN TO CHECK-RCN
                    MOVE COPY-DDS-MBR TO CHECK-MBR
                    PERFORM CHECK-TEILDATEI
                    IF   RET-CODE NOT = SPACES
     ‚*                  WENN DIES EINE LFS-LIB IST, GGF. DIE SRC
     ‚*                  VERSUCHEN EINZUTRAGEN
                         IF   COPY-DDS-LIB(9:) = SPACES
                          AND COPY-DDS-LIB(8:1) IS NUMERIC
                          AND COPY-DDS-LIB(4:3) NOT = "SRC"
                              MOVE "SRC" TO COPY-DDS-LIB(4:3)
                              MOVE COPY-DDS-LIB TO CHECK-LIB
                              MOVE COPY-DDS-RCN TO CHECK-RCN
                              MOVE COPY-DDS-MBR TO CHECK-MBR
                              PERFORM CHECK-TEILDATEI
                         END-IF
                    END-IF
                    IF   RET-CODE NOT = SPACES
                         MOVE SPACES TO NOCHECK-WRK
                         CALL "CRP0011" USING COPY-DDS-LIB
                                              COPY-DDS-RCN
                                              COPY-DDS-MBR
                                              NOCHECK-WRK
                                              F12
                         END-CALL
                         IF   F12 = "1"
                              INITIALIZE F12
                              INITIALIZE COPY-DDS-LIB
                              INITIALIZE COPY-DDS-RCN
                              INITIALIZE COPY-DDS-MBR
                              GO TO GET-CMO-90
                         END-IF
                    END-IF
           END-IF.

       GET-CMO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES SECTION-NAMEN IN EINER ZEILE
     ‚*--------------------------------------------------------------
       GET-SECTION-NAME SECTION.
       GET-SNM-00.

           INITIALIZE RET-CODE.
           INITIALIZE SECTION-NAME.
     ‚* ALS ERSTES MÜSSEN WIR DIE ZEILE NOCHMAL LESEN
           PERFORM  GET-ZEILE.
           IF       RET-CODE NOT = SPACES
                    GO TO GET-SNM-90
           END-IF.

     ‚* IST DIES EIN KOMMENTAR?
           IF       SRCDTA OF SRC-WRK(7:1) = "*"
                    GO TO GET-SNM-90
           END-IF.

     ‚* EIN SECTION-AUFRUF KANN NUR MIT EINEM "PERFORM" GESCHEHEN
     ‚* Z.B. 1. PERFORM LESEN-DATEN.
     ‚*      2. PERFORM LESEN-DATEN UNTIL F3 = "1".
     ‚*      3. PERFORM
     ‚*                 LESEN-DATEN.
           INITIALIZE POS-TMP.
           INSPECT  SRCDTA OF SRC-WRK TALLYING POS-TMP FOR
                    CHARACTERS BEFORE " PERFORM ".
           IF       POS-TMP = LENGTH OF SRCDTA OF SRC-WRK
                    GO TO GET-SNM-90
           END-IF.

     ‚* JETZT PRÜFEN WIR OB ES EIN UNTIL GIBT ODER NICHT
           INITIALIZE LEN-TMP.
           INSPECT  SRCDTA OF SRC-WRK TALLYING LEN-TMP FOR
                    CHARACTERS BEFORE " UNTIL".
           IF       LEN-TMP = LENGTH OF SRCDTA OF SRC-WRK
                    IF   POS-TMP + 9 <= LENGTH OF SRCDTA OF SRC-WRK
                         MOVE SRCDTA OF SRC-WRK(POS-TMP + 9:) TO
                              SECTION-NAME
                    END-IF
     ‚*€   ELSE
           END-IF.
           INSPECT  SECTION-NAME REPLACING ALL "." BY SPACES.

       GET-SNM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER ZEILE EINER SECTION
     ‚*--------------------------------------------------------------
       GET-SECTION-ZEILE SECTION.
       GET-SCZ-00.

           INITIALIZE SECTION-SEQ.
           INITIALIZE MAP-KEY-WRK, MAP-LFDN-WRK.
     ‚* DAS PROGRAMM DURCHSUCHT DEN POINTER UND GIBT DEN KEY ZURÜCK
           MOVE     SECTION-NAME TO MAP-SUCHE-WRK.
           CALL     "CRP0009" USING SECTION-PTR
                                    MAP-SUCHE-WRK
                                    MAP-KEY-WRK
                                    MAP-LFDN-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE RET-CODE
                    GO TO GET-SCZ-90
           END-IF.

     ‚* ÜBERNEHMEN WENN MÖGLICH
           IF       MAP-KEY-WRK(31:6) IS NUMERIC
                    MOVE MAP-KEY-WRK(31:6) TO SECTION-SEQ(1:6)
           END-IF.

       GET-SCZ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LETZTEN KEY/LFDN IM POINTER ERMITTELN
     ‚*--------------------------------------------------------------
       GET-LAST-POINTER-KEY SECTION.
       GET-LPK-00.

           INITIALIZE LAST-KEY, LAST-LFDN, LAST-REC.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO MAP-LFDN-WRK.

       GET-LPK-20.
           MOVE     MAP-LFDN-WRK       TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO LAST-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO GET-LPK-90
           END-IF.

     ‚* LETZTEN KEY ÜBERNEHMEN
           MOVE     MAP-LFDN OF MAPDTA-REC TO LAST-LFDN.
           MOVE     MAP-KEY  OF MAPDTA-REC TO LAST-KEY.
           MOVE     MAP-REC  OF MAPDTA-REC TO LAST-REC.

           ADD      1 TO MAP-LFDN-WRK.
           GO TO    GET-LPK-20.

       GET-LPK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NÄCHSTES WORD AUS STR-WRK ERMITTELN
     ‚*--------------------------------------------------------------
       GET-NEXT-WORD SECTION.
       GET-NWD-00.

           INITIALIZE WORD.
     ‚* STRING VON BEGIN AN LESEN
           IF       GNW-NEW-READ = 1
                    INITIALIZE GNW-NEW-READ
                    MOVE 1 TO  GNW-POS
           END-IF.

     ‚* FÜHRENDE SPACES ÜBERLESEN
           PERFORM  WITH TEST BEFORE VARYING GNW-POS FROM GNW-POS BY 1
                    UNTIL GNW-POS > LENGTH OF SOURCE-WRK OR
                          SOURCE-WRK(GNW-POS:1) NOT = SPACES
                    CONTINUE
           END-PERFORM.

     ‚* ALLE ZEICHEN BIS ZUM NÄCHSTEN SPACE ÜBERNEHMEN
           INITIALIZE GNW-IDX.
           PERFORM  WITH TEST BEFORE VARYING GNW-POS FROM GNW-POS BY 1
                    UNTIL GNW-POS > LENGTH OF SOURCE-WRK OR
                          SOURCE-WRK(GNW-POS:1) = SPACES
                    ADD 1 TO GNW-IDX
                    MOVE SOURCE-WRK(GNW-POS:1) TO WORD(GNW-IDX:1)
           END-PERFORM.

       GET-NWD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER CURSER-POSITIONIERUNG IM BILDSCHIRM
     ‚*--------------------------------------------------------------
       GET-CURSER-POS SECTION.
       GET-CUR-00.

           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB           TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK         TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK             TO WHNAME OF PCFGFFD-L.
           MOVE     CURFLD OF FORMAT1-I TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CUR-90
           END-IF.

       GET-CUR-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-CUR-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L NOT = FMT-WRK
            OR      WHFLDE OF PCFGFFD-L NOT = CURFLD OF FORMAT1-I
                    GO TO GET-CUR-90
           END-IF.

     ‚* KOORDINATEN ERMITTELN
           MOVE     WHDROW OF PCFGFFD-L TO CURY OF FORMAT1.
           COMPUTE  CURX OF FORMAT1 =   WHDCOL OF PCFGFFD-L
                                      + CURPOS OF FORMAT1-I
                                      - 1
           END-COMPUTE.

           GO TO    GET-CUR-20.

       GET-CUR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER CURSER-POSITION FÜR INSERT
     ‚*--------------------------------------------------------------
       GET-CURSER-POS-X SECTION.
       GET-CUX-00.

           INITIALIZE RET-CODE.

       GET-CUX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER CURSER-POSITIONIERUNG IM BILDSCHIRM
     ‚*--------------------------------------------------------------
       GET-XMLFFD SECTION.
       GET-XML-00.

           MOVE     ZEROES TO XML-FOUND.
     ‚* ERMITTELN DER XMLFFD-DATEN ZU EINEM FELD
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB           TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK         TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK             TO WHNAME OF PCFGFFD-L.
           MOVE     XML-FELD            TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

       GET-XML-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L NOT = FMT-WRK
            OR      WHFLDE OF PCFGFFD-L NOT = XML-FELD
                    INITIALIZE PCFGFFDF OF PCFGFFD-L
                    GO TO GET-XML-90
           END-IF.

     ‚* FELD GEFUNDEN
           MOVE     1 TO XML-FOUND.

       GET-XML-90.
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
           MOVE     SRCLIB-WRK TO TOLIB    OF OVRDBF-REC.
           MOVE     SRCRCN-WRK TO TOFILE   OF OVRDBF-REC.
           MOVE     SRCMBR-WRK TO TOMBR    OF OVRDBF-REC.
           CALL     "CLP0011" USING OVRDBF-REC
                                    RET-CODE
           END-CALL.

       SET-OVR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EXTRA EINTRAG FÜR COPY-MODUL HINZUFÜGEN
     ‚*--------------------------------------------------------------
       SET-COPY-MARKE SECTION.
       SET-CMK-00.

           INITIALIZE RET-CODE.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCZEI-WRK         TO MAP-KEY OF MAPDTA-REC(31:6).
     ‚* DAMIT DIESE ZEILEN IN DER SORTIERUNG GANZ HINTEN ERSCHEINEN
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
     ‚* CPY-MODUL?
           IF       SRCLIB-WRK NOT = X-SRCLIB
            OR      SRCRCN-WRK NOT = X-SRCRCN
            OR      SRCMBR-WRK NOT = X-SRCMBR
     ‚*             FÜR DIE SORTIERUNG ZEILENNUMMER DER ORIGINALZEILE
     ‚*             WIEDER ÜBERSCHREIBEN
                    MOVE SRCZEI-WRK TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE SRCLIB-WRK TO MAP-KEY OF MAPDTA-REC(51:10)
                    MOVE SRCRCN-WRK TO MAP-KEY OF MAPDTA-REC(61:10)
                    MOVE SRCMBR-WRK TO MAP-KEY OF MAPDTA-REC(71:10)
                    EVALUATE COPY-START
                         WHEN 1
                              MOVE ZEROES     TO
                                   MAP-KEY OF MAPDTA-REC(81:6)
                              MOVE "COPY  "    TO
                                   MAP-KEY OF MAPDTA-REC(88:6)
                         WHEN 2
                              ADD  1 TO LAST-SEQNBR
                              MOVE LAST-SEQNBR TO
                                   MAP-KEY OF MAPDTA-REC(81:6)
     ‚*                       HIER 7 DAMIT DER DAHINTER STEHT
                              MOVE "FFFFFFFFFFFFFF" TO
                                   MAP-KEY OF MAPDTA-REC(88:7)
                    END-EVALUATE
           END-IF.

           INITIALIZE QSETSRC OF SRC-WRK.
           MOVE     ZEROES          TO SRCSEQ OF SRC-WRK.
           MOVE     ZEROES          TO SRCDAT OF SRC-WRK.
           MOVE     "##CPY"         TO SRCDTA OF SRC-WRK(1:5).
           MOVE     RED-RI-HEX-WRK  TO SRCDTA OF SRC-WRK(6:1).
           MOVE     "*"             TO SRCDTA OF SRC-WRK(7:1).
           EVALUATE COPY-START
                    WHEN 1
                         MOVE "BOF:"   TO SRCDTA OF SRC-WRK(9:4)
                    WHEN 2
                         MOVE "EOF:"   TO SRCDTA OF SRC-WRK(9:4)
           END-EVALUATE.

     ‚* TEILDATEIENNAME ZUSAMMENBAUEN
           INITIALIZE STRING-WRK.
           STRING   SRCLIB-WRK      DELIMITED BY SPACES
                    "/"             DELIMITED BY SIZE
                    SRCRCN-WRK      DELIMITED BY SPACES
                    "/"             DELIMITED BY SIZE
                    SRCMBR-WRK      DELIMITED BY SPACES
           INTO     STRING-WRK.
           MOVE     STRING-WRK      TO SRCDTA OF SRC-WRK(14:).
           MOVE     SRC-WRK         TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE       TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

       SET-CMK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SPERREN DER SOURCE
     ‚*--------------------------------------------------------------
       SET-MBR-LOCK SECTION.
       SET-LCK-00.

           INITIALIZE RET-CODE.
     ‚* NUR SPERREN WENN ÄNDERNMODUS AKTIV
           IF       AENDERN-WRK = ZEROES
                    GO TO SET-LCK-90
           END-IF.

     ‚* JETZT SPERREN DER SOURCE
           MOVE     1 TO LCK-AKTION.
           CALL     "CRP0008" USING LCK-LIB
                                    LCK-RCN
                                    LCK-MBR
                                    LCK-AKTION
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE 1 TO LCK-SET
           END-IF.

       SET-LCK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN EINER SOURCE-ZEILE
     ‚*--------------------------------------------------------------
       SET-ZEILE SECTION.
       SET-ZEI-00.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* ERSTMAL DIE ZEILE UM ALLE HEXZEICHEN ENTFERNEN, SOFERN DIESE
     ‚* KEINE KOMMENTARZEILE IST
           MOVE     SOURCE-WRK TO SRCDTA OF SRC-WRK.
           PERFORM  ENTFERNE-SYNTAX-HIGHLIGHT.

     ‚* DATUM SETZEN
           MOVE     DATE-8(3:6)        TO SRCDAT OF SRC-WRK.

     ‚* LESEN MAPDTA UM DIE ZEILE ZU SCHREIBEN/AKTUALISIEREN
           INITIALIZE MAPDTA-REC.
           MOVE     SRCLIB-WRK         TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     SRCRCN-WRK         TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     SRCMBR-WRK         TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     GET-ZEILE-WRK      TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
     ‚* JETZT DIE SOURCE-WRK IN DEN RECORD SCHIEBEN
           MOVE     SRC-WRK            TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

       SET-ZEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* COPY-MODUL IN INT. TABELLE EINTRAGEN
     ‚*--------------------------------------------------------------
       ADD-COPY-MOD-TAB SECTION.
       ADD-CMT-00.

           INITIALIZE IND.
     ‚* SOLANGE LAUFEN BIS WAS FREI IST
           PERFORM  100 TIMES
                    ADD  1 TO IND
                    IF   COPY-MOD-LIB OF COPY-REC(IND) = SPACES
                         MOVE SRCLIB-WRK TO
                              COPY-MOD-LIB OF COPY-REC(IND)
                         MOVE SRCRCN-WRK TO
                              COPY-MOD-RCN OF COPY-REC(IND)
                         MOVE SRCMBR-WRK TO
                              COPY-MOD-MBR OF COPY-REC(IND)
                         GO TO ADD-CMT-90
                    END-IF
           END-PERFORM.

       ADD-CMT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN ZEILENBEFEHL VORLIEGT UND EINTRAGEN
     ‚*--------------------------------------------------------------
       ADD-USRCMD-TAB SECTION.
       ADD-UCT-00.

     ‚* LIEGT EIN USERBEFEHL VOR?
           IF       SRCSEQ-WRK    = SPACES
            OR      GET-ZEILE-WRK = ZEROES
                    GO TO ADD-UCT-90
           END-IF.

     ‚* STEHT IN DER ZEILE GGF. WIEDER DIE ZEILENNUMMER?
           INITIALIZE SRCSEQ-TMP.
           MOVE     GET-ZEILE-WRK(1:4) TO SRCSEQ-TMP(1:4).
           MOVE     "."                TO SRCSEQ-TMP(5:1).
           MOVE     GET-ZEILE-WRK(5:2) TO SRCSEQ-TMP(6:2).
           IF       SRCSEQ-WRK = SRCSEQ-TMP
                    GO TO ADD-UCT-90
           END-IF.

           INITIALIZE USRCMD-WRK.
     ‚* BEFEHL EXTRAHIEREN
           INITIALIZE POS-TMP, LEN-TMP.
           PERFORM  WITH TEST BEFORE UNTIL POS-TMP >=
                    LENGTH OF SRCSEQ-WRK
                    ADD 1 TO POS-TMP
                    IF   SRCSEQ-WRK(POS-TMP:1) NOT =
                         SRCSEQ-TMP(POS-TMP:1)
                         ADD 1 TO LEN-TMP
                         MOVE SRCSEQ-WRK(POS-TMP:1) TO
                              USRCMD-WRK(LEN-TMP:1)
                    END-IF
           END-PERFORM.

           INITIALIZE IND.
     ‚* SOLANGE LAUFEN BIS WAS FREI IST
           PERFORM  500 TIMES
                    ADD  1 TO IND
                    IF   SRCSEQ OF TAB-REC OF USRCMD-TAB(IND) = ZEROES
                     OR  SRCSEQ OF TAB-REC OF USRCMD-TAB(IND) =
                         GET-ZEILE-WRK
                         MOVE 1 TO CHANGE-USRCMD-AKTIV
                         MOVE GET-ZEILE-WRK TO
                              SRCSEQ OF TAB-REC OF USRCMD-TAB(IND)
                         MOVE USRCMD-WRK    TO
                              SRCCMD OF TAB-REC OF USRCMD-TAB(IND)
                         GO TO ADD-UCT-90
                    END-IF
           END-PERFORM.

       ADD-UCT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* OFFENE DATEI HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-OPNSRC-TAB SECTION.
       ADD-OST-00.

           ADD      1 TO OPN-SRC-MAX OF OPN-SRC-TAB.
           IF       OPN-SRC-MAX OF OPN-SRC-TAB > MAX-OPN-SRC-TAB
                    GO TO ADD-OST-90
           END-IF.

           MOVE     SRCLIB-WRK  TO OPN-SLIB OF
                    OPN-SRC-TAB(OPN-SRC-MAX OF OPN-SRC-TAB).
           MOVE     SRCRCN-WRK  TO OPN-SRCN OF
                    OPN-SRC-TAB(OPN-SRC-MAX OF OPN-SRC-TAB).
           MOVE     SRCMBR-WRK  TO OPN-SMBR OF
                    OPN-SRC-TAB(OPN-SRC-MAX OF OPN-SRC-TAB).
           MOVE     AENDERN-WRK TO OPN-AUSW OF
                    OPN-SRC-TAB(OPN-SRC-MAX OF OPN-SRC-TAB).

     ‚* JETZT DIE SOURCE IM POINTER SPEICHERN
           SET      PTR-ADD TO TRUE.
           SET      PTR-INPUT OF PTRMAP-REC TO TEMP-PTR.
           MOVE     "SOURCE-"   TO PTR-KEY OF PTRMAP-REC.
           MOVE     OPN-SRC-MAX OF OPN-SRC-TAB TO
                    PTR-KEY OF PTRMAP-REC(8:).
           PERFORM  COPY-PTR-MAPPING.

     ‚* AUSWAHL ÜBERNEHMEN
           MOVE     OPN-SRC-MAX OF OPN-SRC-TAB TO OPN-CHOICE.

       ADD-OST-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* COPY-MODUL AUS INT. TABELLE ENTFERNEN
     ‚*--------------------------------------------------------------
       RMV-COPY-MOD-TAB SECTION.
       RMV-CMT-00.

           INITIALIZE IND.
     ‚* SOLANGE LAUFEN BIS WAS FREI IST
           PERFORM  100 TIMES
                    ADD  1 TO IND
                    IF   COPY-MOD-LIB OF COPY-REC(IND) = SRCLIB-WRK
                     AND COPY-MOD-RCN OF COPY-REC(IND) = SRCRCN-WRK
                     AND COPY-MOD-MBR OF COPY-REC(IND) = SRCMBR-WRK
                         MOVE SPACES TO COPY-MOD-LIB OF COPY-REC(IND)
                         MOVE SPACES TO COPY-MOD-RCN OF COPY-REC(IND)
                         MOVE SPACES TO COPY-MOD-MBR OF COPY-REC(IND)
                         GO TO RMV-CMT-90
                    END-IF
           END-PERFORM.

       RMV-CMT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE HEXZEICHEN ETC. ENTFERNEN
     ‚*--------------------------------------------------------------
       ENTFERNE-SYNTAX-HIGHLIGHT SECTION.
       ENT-SYH-00.

     ‚* NUR BEI ECHTEN SOURCEZEILEN DIE HEXWERTE ENTFERNEN
           IF       SRCDTA OF SRC-WRK(7:1) NOT = "*"
                    INITIALIZE POS-TMP
                    PERFORM  WITH TEST BEFORE UNTIL POS-TMP >=
                             LENGTH OF SRCDTA OF SRC-WRK
                             ADD  1 TO POS-TMP
     ‚*                      ALLE HEXZEICHEN ENTFERNEN
                             IF   SRCDTA OF SRC-WRK(POS-TMP:1)
                                   < SPACES
                                  MOVE SPACES TO
                                       SRCDTA OF SRC-WRK(POS-TMP:1)
                             END-IF
                    END-PERFORM
           END-IF.

     ‚* AUCH DIE LINIEN ENTFERNEN
           IF       CFFK10 OF CFG-CPY NOT = ZEROES
                    IF   SRCDTA OF SRC-WRK(72:1) = "|"
                         MOVE SPACES TO SRCDTA OF SRC-WRK(72:1)
                    END-IF
           END-IF.
           IF       CFNM03 OF CFG-CPY NOT = ZEROES
                    IF   SRCDTA OF SRC-WRK(CFNM03 OF CFG-CPY:1) = "|"
                         MOVE SPACES TO
                              SRCDTA OF SRC-WRK(CFNM03 OF CFG-CPY:1)
                    END-IF
           END-IF.
           IF       CFNM04 OF CFG-CPY NOT = ZEROES
                    IF   SRCDTA OF SRC-WRK(CFNM04 OF CFG-CPY:1) = "|"
                         MOVE SPACES TO
                              SRCDTA OF SRC-WRK(CFNM04 OF CFG-CPY:1)
                    END-IF
           END-IF.

       ENT-SYH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* RUNTERSCROLLEN
     ‚*--------------------------------------------------------------
       SCROLL-DOWN SECTION.
       SCR-DWN-00.

     ‚* API-LAUFENDE NUMMER?
           IF       API-LFDN-DSP OF SEU-API-OUTPUT NOT = ZEROES
                    MOVE API-LFDN-DSP OF SEU-API-OUTPUT TO START-ZEILE
                    GO TO SCR-DWN-90
           END-IF.
     ‚* API SRCZEILE?
           IF       API-SRCSEQ-DSP OF SEU-API-OUTPUT NOT = ZEROES
                    MOVE API-SRCSEQ-DSP OF SEU-API-OUTPUT TO
                         GO-ZEILE-SEQ
                    GO TO SCR-DWN-90
           END-IF.

     ‚* STANDARDSCROLLEN
           MOVE     1               TO NEUANZEIGE-WRK.
           MOVE     ZEROES          TO FIND-STARTZEILE.
           MOVE     START-ZEILE     TO LAST-STARTZEILE.
           MOVE     AKT-LFDN-MAX    TO START-ZEILE.
           ADD      1               TO START-ZEILE.

       SCR-DWN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HOCHSCROLLEN
     ‚*--------------------------------------------------------------
       SCROLL-UP SECTION.
       SCR-LUP-00.

     ‚* API-LAUFENDE NUMMER?
           IF       API-LFDN-DSP OF SEU-API-OUTPUT NOT = ZEROES
                    MOVE API-LFDN-DSP OF SEU-API-OUTPUT TO START-ZEILE
                    GO TO SCR-LUP-90
           END-IF.
     ‚* API SRCZEILE?
           IF       API-SRCSEQ-DSP OF SEU-API-OUTPUT NOT = ZEROES
                    MOVE API-SRCSEQ-DSP OF SEU-API-OUTPUT TO
                         GO-ZEILE-SEQ
                    GO TO SCR-LUP-90
           END-IF.

     ‚* STANDARDSCROLLEN
           MOVE     1               TO NEUANZEIGE-WRK.
           MOVE     ZEROES          TO FIND-STARTZEILE.
           SUBTRACT MAX-ZEILEN FROM START-ZEILE.
     ‚*€   MOVE     LAST-STARTZEILE TO START-ZEILE.

       SCR-LUP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* API VOR DER DISPLAY-AUSGABE AUFRUFEN
     ‚*--------------------------------------------------------------
       API-CALL-OUTPUT SECTION.
       API-COU-00.

     ‚* PROGRAMM VORHANDEN?
           IF       CFDAT6 OF CFG-CPY = SPACES
            OR      CFDAT7 OF CFG-CPY = SPACES
            OR      CFFK17 OF CFG-CPY = ZEROES
                    GO TO API-COU-90
           END-IF.

     ‚* GANZES DISPLAY ALS XMLFFD-REC ZUSAMMENBAUEN
           IF       API-XMLFFD = ZEROES
                    MOVE    1 TO API-XMLFFD
                    PERFORM API-FILL-XMLFFD
           END-IF.

           MOVE     1                 TO API-AKTION.
     ‚* PROGRAMM PRÜFEN UND AUFRUFEN
           MOVE     CFDAT7 OF CFG-CPY TO CHECK-LIB.
           MOVE     CFDAT6 OF CFG-CPY TO CHECK-RCN.
           MOVE     SPACES            TO CHECK-MBR.
           PERFORM  CHECK-PROGRAMM.
           IF       RET-CODE = SPACES
                    PERFORM API-FILL-INPUT
                    PERFORM API-CLEAR-OUTPUT
                    CALL CHECK-RCN IN LIBRARY CHECK-LIB
                                   USING SEU-API-INPUT
                                         SEU-API-OUTPUT
                    END-CALL
                    IF   API-ERRFLD OF SEU-API-OUTPUT = SPACES
                     AND API-ERRMSG OF SEU-API-OUTPUT = SPACES
                         PERFORM API-TAKE-OUTPUT
                    END-IF
           END-IF.

       API-COU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* API NACH DER DISPLAY-AUSGABE AUFRUFEN
     ‚*--------------------------------------------------------------
       API-CALL-INPUT SECTION.
       API-CIN-00.

     ‚* PROGRAMM VORHANDEN?
           IF       CFDAT6 OF CFG-CPY = SPACES
            OR      CFDAT7 OF CFG-CPY = SPACES
            OR      CFFK17 OF CFG-CPY = ZEROES
                    GO TO API-CIN-90
           END-IF.

     ‚* GANZES DISPLAY ALS XMLFFD-REC ZUSAMMENBAUEN
           IF       API-XMLFFD = ZEROES
                    MOVE    1 TO API-XMLFFD
                    PERFORM API-FILL-XMLFFD
           END-IF.

           MOVE     2                 TO API-AKTION.
     ‚* PROGRAMM PRÜFEN UND AUFRUFEN
           MOVE     CFDAT7 OF CFG-CPY TO CHECK-LIB.
           MOVE     CFDAT6 OF CFG-CPY TO CHECK-RCN.
           MOVE     SPACES            TO CHECK-MBR.
           PERFORM  CHECK-PROGRAMM.
           IF       RET-CODE = SPACES
                    PERFORM API-FILL-INPUT
                    PERFORM API-CLEAR-OUTPUT
                    CALL CHECK-RCN IN LIBRARY CHECK-LIB
                                   USING SEU-API-INPUT
                                         SEU-API-OUTPUT
                    END-CALL
                    IF   API-ERRFLD OF SEU-API-OUTPUT = SPACES
                     AND API-ERRMSG OF SEU-API-OUTPUT = SPACES
                         PERFORM API-TAKE-OUTPUT
                    END-IF
           END-IF.

       API-CIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER XMLFFD STRUKTUR
     ‚*--------------------------------------------------------------
       API-FILL-XMLFFD SECTION.
       API-FXM-00.

           INITIALIZE SEU-API-XMLFFD.
           MOVE     ZEROES TO FLD-RECORD-MAX OF SEU-API-XMLFFD.
     ‚* LESEN ALLER DATUMSFELDER
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     CONST-LIB       TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK     TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK         TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES          TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO API-FXM-90
           END-IF.

       API-FXM-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO API-FXM-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L      NOT = CONST-LIB
            OR      WHFILE OF PCFGFFD-L      NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L      NOT = FMT-WRK
                    GO TO API-FXM-90
           END-IF.

           ADD      1 TO FLD-RECORD-MAX OF SEU-API-XMLFFD.
     ‚* ALLE DATEN IN DIE INTERNE TABELLE SICHERN
           MOVE     CORR PCFGFFDF OF PCFGFFD-L TO
                         FLD-RECORD OF SEU-API-XMLFFD(FLD-RECORD-MAX OF
                                                      SEU-API-XMLFFD).

           GO TO    API-FXM-20.

       API-FXM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INPUT-PARAMETER FÜLLEN
     ‚*--------------------------------------------------------------
       API-FILL-INPUT SECTION.
       API-FIN-00.

     ‚* TEILDATEI-INFOS FÜLLEN
           MOVE     X-SRCLIB TO API-SRCLIB  OF SEU-API-INPUT.
           MOVE     X-SRCRCN TO API-SRCRCN  OF SEU-API-INPUT.
           MOVE     X-SRCMBR TO API-SRCMBR  OF SEU-API-INPUT.
           MOVE     X-OPTION TO API-OPNMODE OF SEU-API-INPUT.

     ‚* XMLFFD FÜLLEN
           MOVE     SEU-API-XMLFFD TO API-INPUT-EXPLAIN OF
                    SEU-API-INPUT.

     ‚* SOURCE-PTR FÜLLEN
           MOVE     "SOURCE" TO POINTER-TYPE OF SEU-API-INPUT.
           CALL     "CRP0016" USING SOURCE-PTR
                                    API-PTR OF SEU-API-INPUT
           END-CALL.

     ‚* INPUT-STRUKTUR FÜLLEN
           IF       API-AKTION = 2
                    MOVE "INPUT"  TO INP-TYPE OF SEU-API-INPUT
           ELSE
                    MOVE "OUTPUT" TO INP-TYPE OF SEU-API-INPUT
           END-IF.
           MOVE     DISPLAY-WRK   TO INP-DISPLAY OF SEU-API-INPUT.
           MOVE     FMT-WRK       TO INP-FMT     OF SEU-API-INPUT.
           MOVE     WSREC-WRK     TO INP-WSREC   OF SEU-API-INPUT.

       API-FIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* RÜCKGABEWERTE DES API-PROGRAMMS FÜR DEN CALL INITIALISIEREN
     ‚*--------------------------------------------------------------
       API-CLEAR-OUTPUT SECTION.
       API-CLO-00.

     ‚* ALLE FELDER INITIALISIEREN
           INITIALIZE API-SOURCE OF SEU-API-OUTPUT.
           INITIALIZE API-OUTPUT OF SEU-API-OUTPUT.
           INITIALIZE API-ERRFLD OF SEU-API-OUTPUT.
           INITIALIZE API-ERRMSG OF SEU-API-OUTPUT.

     ‚* RÜCKGABE-POINTER AUF NULL SETZEN
           SET      API-PTR OF SEU-API-OUTPUT TO NULL.
           MOVE     SPACES TO POINTER-TYPE OF SEU-API-OUTPUT.

       API-CLO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WERTE DES API-PROGRAMMS FÜR DEN CALL INITIALISIEREN
     ‚*--------------------------------------------------------------
       API-CLEAR-INPUT SECTION.
       API-CLI-00.

     ‚* ALLE FELDER INITIALISIEREN
           INITIALIZE API-INPUT         OF SEU-API-INPUT.
           INITIALIZE API-SOURCE        OF SEU-API-INPUT.
           INITIALIZE API-INPUT-EXPLAIN OF SEU-API-INPUT.

     ‚* RÜCKGABE-POINTER AUF NULL SETZEN
           SET      API-PTR OF SEU-API-INPUT TO NULL.
           MOVE     SPACES TO POINTER-TYPE OF SEU-API-INPUT.

       API-CLI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÜBERNEHMEN DES VOM API AUSGEFÜHRTEN RECORDS
     ‚*--------------------------------------------------------------
       API-TAKE-OUTPUT SECTION.
       API-TKO-00.

TODO ‚* ERSTMAL ÜBERNEHMEN WIR DEN RECORD OHNE PRÜFUNG
           IF       API-KEY-NEWDSP OF SEU-API-OUTPUT = "1"
                    MOVE OUT-WSREC OF SEU-API-OUTPUT TO WSREC-WRK
           END-IF.

       API-TKO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     CHECK-LIB    TO LIB     OF CHKOBJ-REC.
           MOVE     CHECK-RCN    TO OBJ     OF CHKOBJ-REC.
           MOVE     CHECK-MBR    TO MBR     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-PROGRAMM SECTION.
       CHK-PGM-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     CHECK-LIB    TO LIB     OF CHKOBJ-REC.
           MOVE     CHECK-RCN    TO OBJ     OF CHKOBJ-REC.
           MOVE     CHECK-MBR    TO MBR     OF CHKOBJ-REC.
           MOVE     "*PGM"       TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PGM-90
           END-IF.

       CHK-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIE ZEILE EIN KOMMENTAR IST
     ‚*--------------------------------------------------------------
       CHECK-IS-COMMENT SECTION.
       CHK-ICM-00.

           INITIALIZE IS-COMMENT.
     ‚* BEI EINIGEN SOURCEN IST DAS GANZ EINFACH
           IF       SRCTYPE OF RTVMBRD-REC = "CBL"
            OR      SRCTYPE OF RTVMBRD-REC = "CBLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "DSPF"
            OR      SRCTYPE OF RTVMBRD-REC = "PRTF"
            OR      SRCTYPE OF RTVMBRD-REC = "PF"
            OR      SRCTYPE OF RTVMBRD-REC = "LF"
                    IF   SOURCE-WRK(7:1)       = "*"
                     AND SOURCE-WRK(7:3)   NOT = "*DB"
                         MOVE 1 TO IS-COMMENT
                    END-IF
                    GO TO CHK-ICM-90
           END-IF.

     ‚* BEI CL-PROGRAMMEN IST DAS NOCH GANZ HUMAN ZU PRÜFEM
           IF       SRCTYPE OF RTVMBRD-REC = "CLLE"
            OR      SRCTYPE OF RTVMBRD-REC = "CLP"
                    INITIALIZE POS-TMP
                    INSPECT SOURCE-WRK TALLYING POS-TMP FOR CHARACTERS
                            BEFORE INITIAL "/*"
     ‚*             WURDE EIN EINLEITENDER KOMMENTAR GEFUNDEN?
                    IF   POS-TMP < LENGTH OF SOURCE-WRK
                         ADD 1 TO POS-TMP
     ‚*                  STEHT ETWAS VOR DEM KOMMENTAR? DANN IST DAS
     ‚*                  KEINE REINE KOMMENTARZEILE
                         IF   SOURCE-WRK(1:POS-TMP) NOT = SPACES
                              GO TO CHK-ICM-90
                         END-IF
     ‚*                  JETZT DAS ENDE DES KOMMENTARS SUCHEN, DA NOCH
     ‚*                  ETWAS DAHINTER STEHEN KÖNNTE
     ‚*                  DER KOMMENTAR KANN ENTWEDER MIT "+" ENDEN,
     ‚*                  DANN GEHT ER IN DIE NÄCHSTE ZEILE, ODER MIT
     ‚*                  "*/" BEENDET WERDEN
                    END-IF
           END-IF.

       CHK-ICM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB IN EINER ZEILE EINE AUSWAHL DURCHGEFÜHRT WERDEN SOLL
     ‚*--------------------------------------------------------------
       CHECK-AUSWAHL SECTION.
       CHK-AUS-00.

           INITIALIZE AUSW-WRK.
     ‚* JEDES FELD PRÜFEN UND AUSWAHL SICHERN
           IF       AUSW01 OF FORMAT1 NOT = SPACES
                    MOVE AUSW01    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ01H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW02 OF FORMAT1 NOT = SPACES
                    MOVE AUSW02    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ02H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW03 OF FORMAT1 NOT = SPACES
                    MOVE AUSW03    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ03H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW04 OF FORMAT1 NOT = SPACES
                    MOVE AUSW04    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ04H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW05 OF FORMAT1 NOT = SPACES
                    MOVE AUSW05    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ05H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW06 OF FORMAT1 NOT = SPACES
                    MOVE AUSW06    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ06H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW07 OF FORMAT1 NOT = SPACES
                    MOVE AUSW07    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ07H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW08 OF FORMAT1 NOT = SPACES
                    MOVE AUSW08    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ08H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW09 OF FORMAT1 NOT = SPACES
                    MOVE AUSW09    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ09H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW10 OF FORMAT1 NOT = SPACES
                    MOVE AUSW10    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ10H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW11 OF FORMAT1 NOT = SPACES
                    MOVE AUSW11    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ11H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW12 OF FORMAT1 NOT = SPACES
                    MOVE AUSW12    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ12H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW13 OF FORMAT1 NOT = SPACES
                    MOVE AUSW13    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ13H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW14 OF FORMAT1 NOT = SPACES
                    MOVE AUSW14    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ14H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW15 OF FORMAT1 NOT = SPACES
                    MOVE AUSW15    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ15H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW16 OF FORMAT1 NOT = SPACES
                    MOVE AUSW16    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ16H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW17 OF FORMAT1 NOT = SPACES
                    MOVE AUSW17    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ17H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW18 OF FORMAT1 NOT = SPACES
                    MOVE AUSW18    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ18H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW19 OF FORMAT1 NOT = SPACES
                    MOVE AUSW19    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ19H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

           IF       AUSW20 OF FORMAT1 NOT = SPACES
                    MOVE AUSW20    OF FORMAT1 TO AUSW-WRK
                    MOVE SRCSEQ20H OF FORMAT1 TO AUSW-SRCSEQ
                    PERFORM AUSW-EXECUTE
           END-IF.

       CHK-AUS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EINE NOTIZ VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-NOTIZ-VORHANDEN SECTION.
       CHK-NVO-00.

           INITIALIZE RET-CODE.
     ‚* JETZT KEY FÜLLEN
           INITIALIZE MAPKEY-WRK.
           MOVE     SRCLIB-WRK         TO MAPKEY-WRK(1:10).
           MOVE     SRCRCN-WRK         TO MAPKEY-WRK(11:10).
           MOVE     SRCMBR-WRK         TO MAPKEY-WRK(21:10).
           MOVE     SRCSEQ OF SRC-WRK  TO MAPKEY-WRK(31:6).
           CALL     "CRP0007" USING NOTIZEN-PTR
                                    MAPKEY-WRK
                                    RET-CODE
           END-CALL.

       CHK-NVO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN COPY-MODUL SCHON OFFEN IST
     ‚*--------------------------------------------------------------
       CHECK-COPY-MODUL-OFFEN SECTION.
       CHK-CMO-00.

           INITIALIZE IND.
           INITIALIZE COPY-MODUL-OFFEN.
     ‚* SOLANGE LAUFEN BIS WAS FREI IST
           PERFORM  100 TIMES
                    ADD  1 TO IND
                    IF   COPY-MOD-LIB OF COPY-REC(IND) = COPY-DDS-LIB
                     AND COPY-MOD-RCN OF COPY-REC(IND) = COPY-DDS-RCN
                     AND COPY-MOD-MBR OF COPY-REC(IND) = COPY-DDS-MBR
                         MOVE 1 TO COPY-MODUL-OFFEN
                         GO TO CHK-CMO-90
                    END-IF
           END-PERFORM.

       CHK-CMO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB AN DER SOURCE ETWAS GEÄNDERT WURDE
     ‚*--------------------------------------------------------------
       CHECK-CHANGE SECTION.
       CHK-CHG-00.

           INITIALIZE CHANGE-AKTIV.
     ‚* WURDE EINE EINGABE IN DEN ZEILEN GEMACHT?
           IF       IN50 OF OUTXX = AN
            OR      IN51 OF OUTXX = AN
            OR      IN52 OF OUTXX = AN
            OR      IN53 OF OUTXX = AN
            OR      IN54 OF OUTXX = AN
            OR      IN55 OF OUTXX = AN
            OR      IN56 OF OUTXX = AN
            OR      IN57 OF OUTXX = AN
            OR      IN58 OF OUTXX = AN
            OR      IN59 OF OUTXX = AN
            OR      IN60 OF OUTXX = AN
            OR      IN61 OF OUTXX = AN
            OR      IN62 OF OUTXX = AN
            OR      IN63 OF OUTXX = AN
            OR      IN64 OF OUTXX = AN
            OR      IN65 OF OUTXX = AN
            OR      IN66 OF OUTXX = AN
            OR      IN67 OF OUTXX = AN
            OR      IN68 OF OUTXX = AN
            OR      IN69 OF OUTXX = AN
                    MOVE 1 TO CHANGE-AKTIV
           END-IF.

       CHK-CHG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB IN DER ZEILENNNUMMER EIN BEFEHL EINGETRAGEN WURDE
     ‚*--------------------------------------------------------------
       CHECK-ZEILENBEFEHL SECTION.
       CHK-ZCH-00.

           INITIALIZE CHANGE-ZEILE-AKTIV.
     ‚* WURDE EINE EINGABE IN DEN ZEILEN GEMACHT?
           IF       IN70 OF OUTXX = AN
            OR      IN71 OF OUTXX = AN
            OR      IN72 OF OUTXX = AN
            OR      IN73 OF OUTXX = AN
            OR      IN74 OF OUTXX = AN
            OR      IN75 OF OUTXX = AN
            OR      IN76 OF OUTXX = AN
            OR      IN77 OF OUTXX = AN
            OR      IN78 OF OUTXX = AN
            OR      IN79 OF OUTXX = AN
            OR      IN80 OF OUTXX = AN
            OR      IN81 OF OUTXX = AN
            OR      IN82 OF OUTXX = AN
            OR      IN83 OF OUTXX = AN
            OR      IN84 OF OUTXX = AN
            OR      IN85 OF OUTXX = AN
            OR      IN86 OF OUTXX = AN
            OR      IN87 OF OUTXX = AN
            OR      IN88 OF OUTXX = AN
            OR      IN89 OF OUTXX = AN
                    MOVE 1 TO CHANGE-ZEILE-AKTIV
           END-IF.

       CHK-ZCH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES EINEN OFFENEN BEFEHL GIBT
     ‚*--------------------------------------------------------------
       CHECK-RET-COMMAND SECTION.
       CHK-RCM-00.

           INITIALIZE RET-CODE.

       CHK-RCM-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-FILL-CR-DATA: CODE-REVIEW-VARIABLEN FÜLLEN
     ‚*---------------------------------------------------------------

           COPY     CRCPY      OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CONVERT-DATE: DATUMSFELDER AUFBEREITEN/UMSETZEN
     ‚*---------------------------------------------------------------

           COPY     CNVDATE    OF APG-QCPYSRC.
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

TODO ˆ* 1. PROGRAMM SOWEIT LAUFFÄHIG KRIEGEN
     ˆ* 2. MITTELS GET-NEXT-WORD SOLANGE LESEN BIS EIN BEFEHL MIT
     ˆ*    PUNKT ENDET, DANN DEN GANZEN BEFEHL IN EINEN STRING PACKEN
     ˆ*    UND IN EIN EXTRA-PROGRAMM SYNTAX-PRÜFUNG REALISIEREN
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CRP1061.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN EINER COBOL-SOURCE                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.11.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERWEITERTER CHKSRC FÜR PROGRAMME             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI CODEREVIEW
     ‚*-------------------------------------------------------------
           SELECT PCHKSRC-DP
                  ASSIGN       TO  DATABASE-PCHKSRC
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- CODEREVIEW
       FD  PCHKSRC-DP
           LABEL RECORDS ARE STANDARD.
       01  PCHKSRC-P.
           COPY DDS-ALL-FORMATS OF PCHKSRC.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CODE-REVIEW VARIABLEN
           COPY WRKCRCPY   OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP1061".
       01  PGM-TYP                       PIC X(8)  VALUE "CHKSRC".

     ‚* VARIABLEN ZUM SCHREIBEN DES DATENSATZES
       01  CRFID-WRK                     LIKE CRFID  OF PCHKSRC-P.
       01  CRFTYP-WRK                    LIKE CRFTYP OF PCHKSRC-P.
       01  SOURCE-ZEILE                  LIKE CRLINE OF PCHKSRC-P.
       01  PCHKSRC-VORGANG.
            COPY DDS-ALL-FORMATS OF PCHKSRC.

     ‚* HILFSSTRUKTUR FÜR SOURCE
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* TABELLE ENTHÄLT ALLE LABELS/SPRUNGMARKEN
       01  TAB.
        05  TAB-POS                      PIC 9(6) COMP.
        05  TAB-POS2                     PIC 9(6) COMP.
        05  TAB-REC OCCURS 1000.
         06 SP-MARKE                     PIC X(30).
         06 ZEILE                        PIC 9(6)V9(2) COMP.
         06 SECNR                        PIC 9(5) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.
         06 VERWENDET                    PIC 9(1).

     ‚* TABELLE ENTHÄLT DIE GO TO NAMEN Z.B. GO TO "ANF010"
       01  TAB2.
        05  TAB2-POS                     PIC 9(6) COMP.
        05  TAB2-POS2                    PIC 9(6) COMP.
        05  TAB-REC OCCURS 1000.
         06  GOTO                        PIC X(30).
         06  ZEILE                       PIC 9(6)V9(2) COMP.
         06  SECNR                       PIC 9(5) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE ENTHÄLT ALLE SECTIONNAMEN
       01  TAB3.
        05  TAB3-POS                     PIC 9(6) COMP.
        05  TAB3-POS2                    PIC 9(6) COMP.
        05  TAB-REC OCCURS 10000.
         06 SECNAM                       PIC X(30).
         06 EXIT-KZ                      PIC 9(01) COMP.
         06 ZEILE                        PIC 9(6)V9(2) COMP.
         06 SP-NAME                      PIC X(30).
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE ENTHÄLT ALLE SECTIONAUFRUFE Z.B. PERFORM ERMITTELN-POS
       01  TAB4.
        05  TAB4-POS                     PIC 9(6) COMP.
        05  TAB4-POS2                    PIC 9(6) COMP.
        05  TAB-REC OCCURS 10000.
         06  SECNAM                      PIC X(30).
         06  ZEILE                       PIC 9(6)V9(2) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE ENTHÄLT ALLE VERWENDETE DATEIEN (NICHT DB-MAIN)
       01  TAB5.
        05  TAB5-POS                     PIC 9(6) COMP.
        05  TAB5-POS2                    PIC 9(6) COMP.
        05  TAB-REC OCCURS 1000.
         06  DATEI                       PIC X(10).
         06  ZEILE                       PIC 9(6)V9(2) COMP.
         06  WDKZ                        PIC 9(1) COMP.
         06  RKKZ                        PIC 9(1) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE ENTHÄLT ALLE VERWENDETE DATEIEN FÜR DB-MAIN
       01  TAB6.
        05  TAB6-POS                     PIC 9(06) COMP.
        05  TAB6-POS2                    PIC 9(06) COMP.
        05  TAB-REC OCCURS 1000.
         06  DATEI                       PIC X(20).
         06  ZEILE                       PIC 9(6)V9(2) COMP.
         06  VERWENDET                   PIC 9(01) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE FÜR SECTION "SUCHEN"
     ‚* ENTHÄLT ZEICHENKETTEN NACH DENEN GESUCHT WERDEN SOLL
       01  SUCH-TAB.
        05  SUCH-REC OCCURS 10.
         06  SUCH           LIKE SRCDTA OF QSETSRC-P.
         06  S-POS                       PIC 9(3) COMP.
         06  E-POS                       PIC 9(3) COMP.
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* TABELLE FÜR SECTION "ERMITTELN-LAENGE"
     ‚* ENTHÄLT DIE LENGE DER SUCHSTRINGS
       01  STR-TAB.
        05  STR-ANZ                      PIC 9(3) COMP.
        05  STR-ANZ2                     PIC 9(3) COMP.
        05  STR-REC OCCURS 100.
         06  STR            LIKE SRCDTA OF QSETSRC-P.
         06  S-LEN                       PIC 9(3) COMP.
         06  E-LEN                       PIC 9(3) COMP.

     ‚* PRÜFTABELLE
     ‚* ENTHÄLT KENNZEICHEN OB EIN BESTIMMTER FEHLERCODE GEPRÜFT
     ‚* WERDEN SOLL
       01  PRUEF-TAB.
        05  PRUEFUNG OCCURS 99           PIC 9(2) COMP VALUE 01.

       01  DATENSATZ                     LIKE SRCDTA OF QSETSRC-P.
       01  DATENSATZ-WRK                 LIKE SRCDTA OF QSETSRC-P.
       01  DATENSATZ-FULL                LIKE SRCDTA OF QSETSRC-P.
       01  SRCDTA-WRK                    LIKE SRCDTA OF QSETSRC-P.
       01  VAR-TMP                       LIKE SRCDTA OF QSETSRC-P.
       01  DISPLAY-WRK                   PIC X(10).

       01  DATENSATZ-SAV                 LIKE SRCDTA OF QSETSRC-P.

       01  KZ-WRK                        PIC 9(2) COMP.

     ‚* FELDER FÜR SECTION "SUCHEN" UND "ERMITTELN-LAENGE"
       01  POS                           PIC 9(3) COMP.
       01  POS2                          PIC 9(3) COMP.
       01  STR-POS                       PIC 9(3) COMP.
       01  STR-ANF                       PIC 9(3) COMP.
       01  STR-LEN                       PIC 9(3) COMP.
       01  STR-WRK                       LIKE SRCDTA OF QSETSRC-P.
       01  LEN                           PIC 9(3) COMP.
       01  LEN2                          PIC 9(3) COMP.

       01  POS-SAV                       PIC 9(3) COMP.
       01  ZEICHEN-WRK                   PIC X(1).
       01  SPLF-NAME                     PIC X(10).
       01  QSETSRC-WRK                   PIC X(10) VALUE "QSETSRC".
       01  ZEILEN                        PIC 9(3).

     ‚*    OPENMODE-OUT = 1 -> AKTUELLER DB-OPENMODE IST "OUTPUT"
     ‚*    OPENMODE-OUT = 0 -> AKTUELLER DB-OPENMODE IST NICHT "OUTPUT"
       01  OPENMODE-OUT                  PIC 9(1).
       01  AKT-DAT-POS                   PIC 9(3).

       01  START-WERT.
         05  PGM-ANFANG                  PIC 9(01) COMP.
         05  PGM-TEIL                    PIC 9(02) COMP.
         05  PGM-PUNKT-AKTUELL           PIC 9(02) COMP.
         05  KZ-WORKING-STORAGE          PIC 9(02) COMP.
         05  PROGRAM-ID-ERLEDIGT         PIC 9(01) COMP.
         05  PGM-WRK-VORHANDEN             PIC 9(01) COMP.
         05  ABBRUCH-KZ                    PIC 9(01) COMP.
         05  GEFUNDEN                      PIC 9(01) COMP.
         05  UEBERNAHME                    PIC 9(01) COMP.
         05  MELDUNG                       PIC X(30).
         05  OPEN-KZ                       PIC 9(01) COMP.
         05  BEFEHL                        PIC X(20).
         05  SP-MARKE-SAV                  PIC X(30).
         05  SECTION-NR                    PIC 9(4) COMP.
         05  SECNR-SAVE                    PIC 9(5) COMP.
         05  ZEILE-WRK                     PIC 9(6)V9(2) COMP.
         05  ZEILE-SAV                     PIC 9(6)V9(2) COMP.
         05  ZEILE-FIRST-CALL              PIC 9(6)V9(2) COMP.
         05  FIRST-CALL-GEFUNDEN           PIC 9(1).
         05  IF-ZAEHLER                    PIC 9(4).
         05  INXX-GEFUNDEN                 PIC 9(1).
         05  EBENE-WRK                     PIC 9(2).
         05  EBENE-SAVE                    PIC 9(2).
         05  LETZTES-ZEICHEN-WRK           PIC X(1).
         05  LETZTES-ZEICHEN-SAVE          PIC X(1).
         05  FELD-WRK                      PIC X(10).
         05  IS-SPRUNGMARKE                PIC 9(1).
         05  DATUM-WRK                     LIKE SRCDAT OF QSETSRC-P.
         05  DATUM-SAV                     LIKE SRCDAT OF QSETSRC-P.

     ‚* TABELLE WIRD FÜR ERMITTLUNG DER LVLCHECK ID BENÖTIGT
       01 FMTID-TAB.
        05 TAB-ANZ-FMT                   PIC 9(03).
        05 FMTID-REC OCCURS 1 TO 100 TIMES DEPENDING ON TAB-ANZ-FMT.
         10 FMT-NAME                     PIC X(10).
         10 FMT-ID                       PIC X(13).

     ‚* VARIABLEN-TABELLE
       01  VAR-TAB.
        05 VAR-POS               PIC 9(4).
        05 TAB-MAX               PIC 9(4).
        05 TAB-REC               OCCURS 1 TO 1000 DEPENDING ON
                                  TAB-MAX OF VAR-TAB.
         06 CRSECN                       LIKE CRSECN OF PCHKSRC-P.
         06 VARNAME                      PIC X(30).
         06 STRUKTUR                     PIC X(30).
         06 BENUTZT                      PIC 9(1).
         06 SRCDAT                       LIKE SRCDAT OF QSETSRC-P.
         06 SRCDTA                       LIKE SRCDTA OF QSETSRC-P.

     ‚* AKTUELLE SECTION
       01  AKTUELLE-SECTION              LIKE DATENSATZ-WRK.
       01  SECTION-SAV                   LIKE DATENSATZ-WRK.

       01  LAST-SP-NAME                  PIC X(30).

     ‚* LAUFVARIABLEN
       01  TAB-COUNTER                   PIC 9(4).

     ‚* SCHALTERVARIABLEN
       01  KEIN-PUNKT-VORHANDEN          PIC 9(1).
       01  COBOL-BEFEHL                  PIC 9(1).

     ‚* EBENE EINER ANWEISUNG, BEFINDEN WIR UNS IN EINEM IF, SO
     ‚* STEHT IFEBENE AUF 1, KOMMT DAS NÄCHSTE IF, WIRD DIES UM
     ‚* 1 ERHÖHT. BEIM END-IF UM EINS VERRINGERT.
       01  CMD-CHECK.
        05 IFEBENE                       PIC 9(4).
        05 EVALEBENE                     PIC 9(4).
        05 ADDEBENE                      PIC 9(4).
        05 SUBEBENE                      PIC 9(4).
        05 MULEBENE                      PIC 9(4).
        05 DIVEBENE                      PIC 9(4).
        05 CMPEBENE                      PIC 9(4).
        05 PEREBENE                      PIC 9(4).
        05 CALLEBENE                     PIC 9(4).

     ‚* BITLEISTE FÜR DIE "DEFAUT" DIVISIONS UND SECTIONS
       01  DIVISION-BIT.
        05 IDENTIFICATION-DIV            PIC 9(1).
         06 IDENTIFICATION-DA            PIC 9(1).
        05 ENVIRONMENT-DIV.
         06 ENVIRONMENT-DA               PIC 9(1).
         06 CONFIGURATION-SEC            PIC 9(1).
         06 INPUTOUTPUT-SEC              PIC 9(1).
        05 DATA-DIV.
         06 DATA-DA                      PIC 9(1).
         06 FILE-SEC                     PIC 9(1).
         06 WORKSTORE-SEC                PIC 9(1).
         06 LOCASTORE-SEC                PIC 9(1).
         06 LINKAGE-SEC                  PIC 9(1).
        05 PROCEDURE-DIV.
         06 PROCEDURE-DA                 PIC 9(1).
         06 STEUER-SEC                   PIC 9(1).

     ‚* KURZNAME FÜR DIE LETZTEN 3 ZIFFERN EINER SPRUNGMARKE,
     ‚* Z.B. -90 ODER -00
       01  SHORT-SP-NAME                 PIC X(3).

     ‚* VARIABLEN FÜR LÄNGENERMITTLUNG
       01  STRLEN-WRK                    PIC X(400).
       01  STRMAX-WRK                    PIC 9(4).
       01  GETLEN-WRK                    PIC 9(4).

     ‚* PARAMETER FÜR DAS CRP0025
       01  SRCLIB-WRK                    PIC X(10).
       01  SRCRCN-WRK                    PIC X(10).
       01  SRCMBR-WRK                    PIC X(10).
       01  SRCZEI-WRK                    LIKE SRCSEQ OF QSETSRC-WRK.
       01  ANZREC-SAV                    PIC 9(9).
       01  FIRST-SEQNBR                  LIKE SRCSEQ OF QSETSRC-WRK.
       01  LAST-SEQNBR                   LIKE SRCSEQ OF QSETSRC-WRK.
       01  TEMP-PTR                      USAGE POINTER.
       01  SECTION-PTR                   USAGE POINTER.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-CRLFDN                      LIKE CRLFDN OF PCHKSRC-P.
       01  X-SRCLIB                      LIKE CRLIB  OF PCHKSRC-P.
       01  X-SRCRCN                      LIKE CRFILE OF PCHKSRC-P.
       01  X-SRCMBR                      LIKE CRMBR  OF PCHKSRC-P.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-CRLFDN
                                X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP1061" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCHKSRC-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DES VORGANGS UM BESTIMMTE EINSTELLUNGEN ZU LESEN
           PERFORM  LESEN-VORGANG.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* FÜLLEN DER CR-TABELLEN
           PERFORM  COPY-FILL-CR-DATA.

     ‚* SOURCE EINLESEN
           PERFORM  READ-SOURCE-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE FELDER IM RECORD ZURÜCHSETZEN
           PERFORM  INIT-VAR.

     ‚* PRÜFEN OB ALLE RELEVANTEN DIVISIONS UND SECTIONS VORHANDEN
     ‚* SIND.
           PERFORM  CHECK-DIV-SECTIONS.

     ‚* JETZT SOURCE DURCHLESEN, DABEI DANN˜DIREKT‚FOLGENDES PRÜFEN:
     ‚*  1. SPRUNGMARKE NICHT STANDARDKONFORM                      ˜025
     ‚*  2. PGM-WRK UND BEI APG-PROGRAMMEN PGM-TYP VORHANDEN       ˜013
     ‚*  3. PROGRAM-ID PASSEND                                     ˜005
     ‚*  4. FEHLENDES SECTION                                      ˜011
     ‚*  5. KEINE DBMAIN-ANGABE (SOFERN DAS SEIN MUSS)             ˜017
     ‚*  6. FEHLERHAFTES FIRST-CALL                                ˜018
     ‚*  7. FEHLENDER PUNKT IN EINER ZEILE                         ˜027
     ‚*  8. BEFEHL BEGINNT VOR POS 20                              ˜022
     ‚*  9. BEFEHL HÖRT NACH 80 AUF                                ˜023
     ‚* 10. KEIN ANFANG/ENDE                                       ˜026
     ‚* 11. FALSCHE BEFÜLLUNG VON PGM-WRK                          ˜012
     ‚*
     ‚* BEI EINEM˜SECTIONWECHSEL‚WERDEN FOLGENDE SACHEN GEPRÜFT:
     ‚*  1. DOPPELTES ODER FALSCHES END-IF                         ˜028
     ‚*  2. FEHLENDES END-IF                                       ˜029
     ‚*  3. DOPPELTES ODER FALSCHES END-CALL                       ˜030
     ‚*  4. FEHLENDES END-CALL                                     ˜031
     ‚*  5. DOPPELTES ODER FALSCHES END-EVALUATE                   ˜032
     ‚*  6. FEHLENDES END-EVALUATE                                 ˜033
           PERFORM  READ-SOURCE-FIRST.

     ‚* IM ERSTEN DURCHLAUF WURDE FOLGENDES GESICHERT:
     ‚*  1. JEDE SECTION
     ‚*  2. JEDE VARIABLE
     ‚*  3. JEDE SPRUNGMARKE
     ‚*
     ‚* DADURCH KANN DANN FOLGENDES GEPRÜFT WERDEN:
     ‚*  1. DOPPELTE SPRUNGMARKE                                   ˜001
     ‚*  2. DOPPELTE SECTION                                       ˜004
     ‚*  3. SECTION WIRD NICHT VERWENDET                           ˜003
           PERFORM  CHECK-SOURCE-FIRST.

     ‚* PRÜFEN DER EINZELNEN FEHLER

       ENDE.
           CLOSE                 PCHKSRC-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES VORGANGS UND DEN EINSTELLUNGEN
     ‚*--------------------------------------------------------------
       LESEN-VORGANG SECTION.
       LES-VOR-00.

           INITIALIZE RET-CODE.
     ‚* LESEN DES VORGANGS
           INITIALIZE PCHKSRCF OF PCHKSRC-P.
           INITIALIZE PCHKSRCF OF PCHKSRC-VORGANG.
           MOVE     X-CRLFDN TO CRLFDN OF PCHKSRC-P.
           READ     PCHKSRC-DP WITH NO LOCK.
           IF       FILE-STATUS = ZEROES
                    MOVE CORR PCHKSRCF OF PCHKSRC-P TO
                              PCHKSRCF OF PCHKSRC-VORGANG
           ELSE
                    MOVE "1" TO RET-CODE
           END-IF.

       LES-VOR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* JETZT MITTELS MAPDTA DIE SOURCE DURCHLESEN
     ‚*--------------------------------------------------------------
       READ-SOURCE-FIRST SECTION.
       REA-SFI-00.

           INITIALIZE RET-CODE.
     ‚* LAUFENDE NUMMER INITIALISIEREN
           MOVE     ZEROES  TO LFDN-MAP.
           SET      MAP-PTR OF MAPDTA-REC TO SOURCE-PTR.

       REA-SFI-20.
           INITIALIZE MAPDTA-REC.
           MOVE     LFDN-MAP      TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO REA-SFI-90
           END-IF.

     ‚* SETTINGS NICHT LESEN
           IF       MAP-KEY OF MAPDTA-REC(1:1) = "*"
                    ADD   1       TO LFDN-MAP
                    GO TO REA-SFI-20
           END-IF.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(1:10) TO RECORD-FLAGS.
           MOVE     MAP-REC OF MAPDTA-REC(10:)  TO QSETSRC-WRK.

     ‚* METADATEN NICHT ANZEIGEN, EBENSO KOMMENTARE
           IF       SRCDTA OF QSETSRC-WRK(7:3) = "*M*"
     ‚*€    OR      SRCDTA OF QSETSRC-WRK(7:1) = "*"
                    ADD   1       TO LFDN-MAP
                    GO TO REA-SFI-20
           END-IF.

     ‚* JETZT DIE ZEILE PRÜFEN
           MOVE     SRCDAT OF QSETSRC-WRK TO DATUM-WRK.
           MOVE     SRCSEQ OF QSETSRC-WRK TO ZEILE-WRK.
           MOVE     FUNCTION UPPER-CASE(SRCDTA OF QSETSRC-WRK(8:66)) TO
                    DATENSATZ(8: ).
           MOVE     SRCDTA OF QSETSRC-WRK TO DATENSATZ-FULL.
           PERFORM  CHECK-SOURCE-ZEILE.

           ADD      1 TO LFDN-MAP.
           GO TO    REA-SFI-20.

       REA-SFI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER DATEIEN
     ‚*--------------------------------------------------------------
       PRUEFEN-DATEIEN SECTION.
       PRU-DAT-00.

     ‚* PRÜFEN OB ANGABE "WITH DUPLICATES" KORREKT
           INITIALIZE TAB5-POS2.
           PERFORM  TAB5-POS TIMES
                    ADD 1 TO TAB5-POS2
                    IF   RKKZ OF TAB5(TAB5-POS2) = 1
                         PERFORM LESEN-PDSPFD
                    END-IF
           END-PERFORM.

     ‚* PRÜFEN OB EINE DATEI EINGEBUNDEN IST UND OB DIESE IM PROGRAMM
     ‚* VERWENDET WIRD
           INITIALIZE TAB6-POS2.
           PERFORM  TAB6-POS TIMES
                    ADD 1 TO TAB6-POS2
                    IF   VERWENDET OF TAB6(TAB6-POS2) = ZERO
                         MOVE DATEI OF TAB6(TAB6-POS2) TO MELDUNG
                         MOVE ZEILE OF TAB6(TAB6-POS2) TO ZEILE-WRK
                         MOVE 15                       TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
           END-PERFORM.

       PRU-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN UM WELCHEN BEFEHL ES SICH HANDELT
     ‚*--------------------------------------------------------------
        PRUEFEN-DIVERSES SECTION.
        PRU-DIV-00.

           INITIALIZE SUCH-TAB.
     ‚* AUS PERFORMANCE GRÜNDEN WIRD HIER STR-ANZ2 INITIALISIERT, DAMIT
     ‚* IN DER SECTION "ERMITTELN-LAENGE" GLEICH AUF DEN ERSTEN
     ‚* TABELLENEINTRAG GEPRÜFT WIRD
           INITIALIZE STR-ANZ2.

     ‚* ERMITTELN DER EBENE, WIRD EIN NEUES IF ERÖFFNET? ODER ENDET
     ‚* EINS? ETC
     ***   PERFORM  CHECK-CMD-EBENE.

     ‚* PRÜFEN OB DIE ZEILE EIN BEFEHL IST, AN DEREN ENDE
     ‚* KEIN PUNKT GEHÖRT. UND WENN NICHT, FEHLER WENN KEIN
     ‚* PUNKT VORHANDEN.
           PERFORM  CHECK-ZEILE-PUNKT.

     ‚* PRÜFEN FUNKTION "FIRST-CALL"
           EVALUATE FIRST-CALL-GEFUNDEN
                    WHEN ZERO
                         MOVE "' FIRST-CALL '" TO SUCH(01)
                         MOVE "' IF '"         TO SUCH(02)
                         PERFORM SUCHEN
                         IF   GEFUNDEN = 1
                              MOVE 1 TO FIRST-CALL-GEFUNDEN
                              MOVE 1 TO IF-ZAEHLER
                              MOVE ZEILE-WRK TO ZEILE-SAV
                              MOVE ZEILE-WRK TO ZEILE-FIRST-CALL
                              GO TO PRU-DIV-90
                         END-IF
                    WHEN 1
                         MOVE "' FIRST-CALL '" TO SUCH(01)
                         MOVE "' MOVE '"       TO SUCH(02)
                         MOVE "1"              TO SUCH(03)
                         PERFORM SUCHEN
                         IF   GEFUNDEN = 1
                              MOVE 3 TO FIRST-CALL-GEFUNDEN
                              IF   IF-ZAEHLER NOT = 1
                                   MOVE "FIRST-CALL" TO MELDUNG
                                   MOVE 18           TO KZ-WRK
                                   MOVE ZEILE-SAV    TO ZEILE-WRK
                                   PERFORM ZEILE-SCHREIBEN
                              END-IF
                              GO TO PRU-DIV-90
                         END-IF
           END-EVALUATE.

           IF       FIRST-CALL-GEFUNDEN > ZERO
            AND     FIRST-CALL-GEFUNDEN < 2
                    INITIALIZE SUCH-TAB
                    MOVE "' IF '" TO SUCH(01)
                    PERFORM SUCHEN
                    IF   GEFUNDEN = 1
                         ADD 1 TO IF-ZAEHLER
                         GO TO PRU-DIV-90
                    END-IF
                    MOVE " END-IF" TO SUCH(01)
                    PERFORM SUCHEN
                    IF   GEFUNDEN = 1
                         SUBTRACT 1 FROM IF-ZAEHLER
                         IF   IF-ZAEHLER <= ZEROES
                              MOVE 2 TO FIRST-CALL-GEFUNDEN
                         END-IF
                         GO TO PRU-DIV-90
                    END-IF
           END-IF.

     ‚* PRÜFEN "DB-OPENMODE"
           INITIALIZE SUCH-TAB.
           MOVE     "DB-OPENMODE"  TO SUCH(01).
           MOVE     "' TO '"       TO SUCH(02).
           MOVE     " MOVE"        TO SUCH(03).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
            AND     S-POS(02) < S-POS(01)
                    INITIALIZE OPENMODE-OUT
                    MOVE E-POS(03) TO POS
                    PERFORM ERMITTELN-STRINGZUWEISUNG
     ‚*             WENN DB-OPENMODE = OUTPUT IST, WIRD DER MERKER
     ‚*             GESETZT
                    IF   DATENSATZ-WRK = "OUTPUT"
                         MOVE  1 TO    OPENMODE-OUT
                    END-IF
     ‚*             PRÜFUNG AUF VARIABLE POS DA DB-OPENMODE TEILWEISE
     ‚*             AUCH ALS STRING GEHANDELT WIRD UND NICHT ALS
     ‚*             VARIABLE: BEISPIEL AUS CRTDBMAIN:
     ‚*             MOVE "MOVE ""INPUT""  TO DB-OPENMODE"
                    IF   POS < E-POS(01)
                     AND DATENSATZ-WRK NOT = "OUTPUT"
                     AND DATENSATZ-WRK NOT = "INPUT"
                     AND DATENSATZ-WRK NOT = "EXTEND"
                     AND DATENSATZ-WRK NOT = "IO"
                         MOVE DATENSATZ-WRK TO MELDUNG
                         MOVE 16            TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN AUF "PGM-WRK"
           INITIALIZE SUCH-TAB.
           MOVE     " PGM-WRK" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
     ‚*             PRÜFEN OB PGM-WRK ALS VARIABLE VERWENDET WIRD.
     ‚*             HANDELT ES SICH UM EIN STRING Z.B. "01 PGM-WRK"
     ‚*             DANN HANDELT ES SICH NICHT UM DEN PGM-WRK BEFEHL
                    INITIALIZE LEN
                    INSPECT DATENSATZ(1:S-POS(01)) TALLYING LEN
                            FOR ALL '"'
                    INITIALIZE LEN2
                    INSPECT DATENSATZ(1:S-POS(01)) TALLYING LEN2
                            FOR ALL "'"
                    IF   LEN  = 1
                     OR  LEN2 = 1
                         CONTINUE
                    ELSE
                         PERFORM CHECK-PGM-WRK
                    END-IF
                    GO TO PRU-DIV-90
           END-IF.

     ‚* SUCHENUND SICHERN DISPLAY-WRK
           INITIALIZE SUCH-TAB.
           MOVE     "' TO '"          TO SUCH(01).
           MOVE     "' DISPLAY-WRK"   TO SUCH(02).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    PERFORM ERMITTELN-STRINGZUWEISUNG
                    IF   DATENSATZ-WRK NOT = SPACES
                         MOVE DATENSATZ-WRK(1:10) TO DISPLAY-WRK
                    END-IF
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN OB BEFEHL EIN "GOTO" IST
           MOVE     "' GO '" TO SUCH(01).
           MOVE     "' TO '" TO SUCH(02).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    MOVE E-POS(02) TO POS
                    PERFORM EINTRAGEN-GOTO
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN OB BEFEHL EIN "PERFROM" IST
           INITIALIZE SUCH-TAB.
           MOVE     "' PERFORM '" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    MOVE E-POS(01) TO POS
                    PERFORM CHECK-PERFORM
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN OB BEFEHL EIN SORT-BEFEHL IST
           MOVE     "' PROCEDURE '" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    MOVE E-POS(01) TO POS
                    PERFORM CHECK-PERFORM
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN OB EIN SECTION-WECHSEL VORLIEGT
           MOVE     " SECTION" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    MOVE DATENSATZ(E-POS(01): ) TO VAR-TMP
                    INSPECT VAR-TMP REPLACING ALL "." BY SPACE
                    IF   VAR-TMP = SPACES
                         PERFORM CHECK-SECTION
     ‚*                  WENN EINE NEUE SECTION BEGINNT, MÜSSEN
     ‚*                  ALLE IFS BEENDET SEIN
                         IF   AKTUELLE-SECTION NOT = SECTION-SAV
                              PERFORM CHECK-SECTION-WECHSEL
                              MOVE AKTUELLE-SECTION TO SECTION-SAV
                         END-IF
                         GO TO PRU-DIV-90
                    END-IF
           END-IF.

     ‚* PRÜFEN OB ANGABE DATEI ZUM AUFRUF FÜR DB-MAIN VORHANDEN
           MOVE     "' TO '"      TO SUCH(01).
           MOVE     "' DB-PTR '"  TO SUCH(02).
           MOVE     "' SET '"     TO SUCH(03).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
     ‚*             ERMITTELN STRINGZUWEISUNG AUS FELD "DATENSATZ"
                    MOVE E-POS(01) TO POS
                    PERFORM AUSRICHTEN
     ‚*             GGFLS '.' AM ENDE DER ZEILE ENTFERNEN
                    INSPECT DATENSATZ-WRK REPLACING ALL "." BY SPACES
     ‚*             IST DER AKTUELLE DB-OPENMODE = 'OUTPUT', SO WIRD
     ‚*             DIE DATEI ALS VERWENDET MARKIERT
                    IF   OPENMODE-OUT = 1
                         MOVE 1 TO VERWENDET OF TAB6(AKT-DAT-POS)
                    END-IF
                    IF   DATENSATZ-WRK = SPACES
                     OR  DATENSATZ-WRK(1:7) = "ADDRESS"
                     OR  DATENSATZ-WRK(1:4) = "NULL"
                         GO TO PRU-DIV-90
                    END-IF
     ‚*             PRÜFEN OB ANGEGEBENE DATEI AUCH DEKLARIERT IST
                    MOVE 1 TO TAB6-POS2
                    PERFORM UNTIL TAB6-POS2 > TAB6-POS
                         IF   DATENSATZ-WRK = DATEI OF TAB6(TAB6-POS2)
     ‚*                       KENNZEICHEN SETZEN DAS DATEI IN
     ‚*                       VERWENDUNG IST
                              MOVE 1 TO VERWENDET OF TAB6(TAB6-POS2)
     ‚*                       DATEI IST DAKLARIERT => ANGABE OK
                              INITIALIZE DATENSATZ-WRK
                              MOVE TAB6-POS TO TAB6-POS2
                         END-IF
                         ADD 1 TO TAB6-POS2
                    END-PERFORM
     ‚*             DATEI NICHT GEFUNDEN -> MELDUNG AUSGEBEN
                    IF   DATENSATZ-WRK NOT = SPACES
                         MOVE DATENSATZ-WRK TO MELDUNG
                         MOVE 14            TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
                    GO TO PRU-DIV-90
           END-IF.

           INITIALIZE SUCH-TAB.

     ‚* PRÜFEN OB BEFEHL EIN "EXIT-BEFEHL" IST
           MOVE     " EXIT" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    PERFORM CHECK-EXIT
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN OB ES SICH UM EINE SPRUNGMARKE HANDELT
           IF       DATENSATZ(8:4) NOT = SPACES
                    MOVE "SP-MARKE" TO BEFEHL
                    PERFORM CHECK-SP-MARKE
                    IF   IS-SPRUNGMARKE NOT = ZEROES
     ‚*                  WENN DAS EINE SPRUNGMARKE IST, DANN DEN NAMEN
     ‚*                  DER ERSTEN GEFUNDENEN SPRUNGMARKE BEI DER
     ‚*                  SECTION VERMERKEN. DAMIT KANN BEI EINEM GOTO
     ‚*                  GEPRÜFT WERDEN, OB VERSUCHT WIRD IN EINE
     ‚*                  ANDERE SECTION ZU SPRINGEN
                         PERFORM UPDATE-SECTION-TABELLE
                         GO TO PRU-DIV-90
                    END-IF
           END-IF.

     ‚* PRÜFEN OB BEFEHL VOR DER STELLE 20 BEGINNT, IM DATENSATZ
     ‚* WÄRE 20. CODESTELLE = 12. DATENSATZ-STELLE
           IF       DATENSATZ(8:4) NOT = SPACES
                    MOVE DATENSATZ     TO MELDUNG
                    MOVE DATENSATZ     TO SOURCE-ZEILE
                    MOVE 22            TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN
                    GO TO PRU-DIV-90
           END-IF.

     ‚* PRÜFEN OB DER BEFEHL ÜBER DIE 80. STELLE HINAUSGEHT
           IF       DATENSATZ-FULL(72:) NOT = SPACES
                    MOVE DATENSATZ     TO MELDUNG
                    MOVE DATENSATZ     TO SOURCE-ZEILE
                    MOVE 23            TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN
                    GO TO PRU-DIV-90
           END-IF.

        PRU-DIV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINTRAGEN GO TO
     ‚*--------------------------------------------------------------
       EINTRAGEN-GOTO SECTION.
       EIN-GTO-00.

     ‚* PRÜFEN WIE DIE SPRUNGMARKE HEIßT WOHIN GESPRUNGEN WERDEN SOLL
           INITIALIZE LEN.
           IF       DATENSATZ(POS: ) = SPACES
                    PERFORM READ-NEXT
                    IF   RET-CODE NOT = SPACES
                         GO TO EIN-GTO-90
                    END-IF
                    INSPECT DATENSATZ TALLYING LEN FOR LEADING SPACES
                    ADD 1 TO LEN
                    MOVE LEN TO POS
           ELSE
                    INSPECT DATENSATZ(POS: ) TALLYING LEN FOR
                    LEADING SPACES
                    ADD LEN TO POS
           END-IF.

           INITIALIZE TAB2-POS2.

           MOVE     DATENSATZ(POS: ) TO DATENSATZ-WRK.
           INSPECT  DATENSATZ-WRK REPLACING ALL "." BY SPACE.

       EIN-GTO-20.
     ‚* PRÜFEN OB GOTO SCHON EINGETRAGEN
           ADD      1 TO TAB2-POS2.
           IF       DATENSATZ-WRK =
                    GOTO OF TAB2(TAB2-POS2)
                    IF   SECTION-NR = SECNR OF TAB2(TAB2-POS2)
                         GO TO EIN-GTO-90
                    ELSE
                         MOVE 999999 TO TAB2-POS2
                    END-IF
           END-IF.
           IF       TAB2-POS > TAB2-POS2
                    GO TO EIN-GTO-20
           END-IF.

     ‚* GOTO IN INTERNE TABELLE SCHREIBEN
           ADD      1 TO TAB2-POS.
           MOVE     DATENSATZ-WRK          TO GOTO  OF TAB2(TAB2-POS).
           MOVE     SECTION-NR             TO SECNR OF TAB2(TAB2-POS).
           MOVE     SRCSEQ OF QSETSRC-P    TO ZEILE OF TAB2(TAB2-POS).
     ‚* WEITERE DATEN SICHERN
           MOVE     SRCDTA OF QSETSRC-P    TO SRCDTA OF TAB2(TAB2-POS).
           MOVE     SRCDAT OF QSETSRC-P    TO SRCDAT OF TAB2(TAB2-POS).

       EIN-GTO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANGEGEBENE DATEI IN TABELLE SICHERN
     ‚*--------------------------------------------------------------
       EINTRAGEN-DB-ANGABEN SECTION.
       EIN-DB-00.

           INITIALIZE STR-ANF.
           INITIALIZE STR-LEN.
           INITIALIZE ABBRUCH-KZ.
           PERFORM  WITH TEST BEFORE VARYING STR-POS FROM
                    LENGTH OF DATENSATZ BY -1 UNTIL STR-POS    = ZEROES
                                              OR    ABBRUCH-KZ = 1
                    IF   DATENSATZ(STR-POS:1) = '"'
                         IF   STR-LEN = ZEROES
     ‚*                       ABSCHLUSSZEICHEN
                              COMPUTE STR-LEN = STR-POS - 1
                         ELSE
     ‚*                       ANFANGSZEICHEN
                              COMPUTE STR-ANF = STR-POS + 1
                              MOVE 1 TO ABBRUCH-KZ
                         END-IF
                    END-IF
           END-PERFORM.

           IF       STR-ANF = ZEROES
                    MOVE 1 TO STR-ANF
           END-IF.
           COMPUTE  STR-LEN = STR-LEN - STR-ANF + 1.

           IF       STR-ANF > ZEROES
            AND     STR-LEN > ZEROES
                    ADD 1 TO TAB6-POS
                    MOVE DATENSATZ(STR-ANF:STR-LEN) TO
                         DATEI OF TAB6(TAB6-POS)
                    MOVE TAB6-POS  TO AKT-DAT-POS
                    MOVE ZEILE-WRK TO ZEILE OF TAB6(TAB6-POS)
                    MOVE     SRCDTA OF QSETSRC-P    TO
                             SRCDTA OF TAB6(TAB6-POS)
                    MOVE     SRCDAT OF QSETSRC-P    TO
                             SRCDAT OF TAB6(TAB6-POS)
           END-IF.

       EIN-DB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN PDSPFD UND ERMITTELN OB DATEI UNIQUE IST
     ‚*--------------------------------------------------------------
       LESEN-PDSPFD SECTION.
       LES-DFD-00.

     ‚* DIESES PROGRAMM ERSTELLT EINE DATEI PDSPFD IN DER QTEMP
           INITIALIZE DSPFD-REC.
           MOVE     DATEI OF TAB5(TAB5-POS2) TO FILENAME  OF DSPFD-REC.
           MOVE     "*LIBL"                  TO LIBNAME   OF DSPFD-REC.
           MOVE     "*ACCPTH"                TO TYPNAME   OF DSPFD-REC.
           MOVE     "*OUTFILE"               TO OUTPUTTYP OF DSPFD-REC.
           MOVE     "PDSPFD"                 TO OUTFILE   OF DSPFD-REC.
           MOVE     "QTEMP"                  TO OUTLIB    OF DSPFD-REC.
           CALL     "LGDSPFD" USING DSPFD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    GO TO LES-DFD-90
           END-IF.

     ‚*€   MOVE     "INPUT"  TO DB-OPENMODE
     ‚*€            SET DB-PTR TO ADDRESS OF PDSPFD-P-X.
     ‚*€            PERFORM  DB-OPEN.
     ‚*€
     ‚* DATEI PDSPFD LESEN UND PRÜFEN OB "WITH DUPLICATES" BENÖTIGT
     ‚* WIRD ODER NICHT
     ‚*€   INITIALIZE PDSPFD-P.
     ‚*€   SET      DB-PTR TO PDSPFD-DP.
     ‚*€   PERFORM  DB-READ-NEXT-NL.
     ‚*€   IF       FILE-STATUS NOT = ZEROES
     ‚*€            GO TO LES-DFD-90
     ‚*€   END-IF.
     ‚*€
     ‚* DATEI UNIQUE ABER "WITH DUPLICATES" VORHANDEN
     ‚*€   IF       (    APUNIQ OF PDSPFD-P      = "Y"
     ‚*€             AND WDKZ OF TAB5(TAB5-POS2) = 1     )
     ‚*                  NICHT UNIQUE UND "WITH DUPLICATES" FEHLT
     ‚*€    OR      (    APUNIQ OF PDSPFD-P      = "N"
     ‚*€             AND WDKZ OF TAB5(TAB5-POS2) = ZEROES )
     ‚*€                 MOVE DATEI OF TAB5(TAB5-POS2) TO MELDUNG
     ‚*€                 MOVE ZEILE OF TAB5(TAB5-POS2) TO ZEILE-WRK
     ‚*€                 MOVE 9 TO KZ-WRK
     ‚*€                 PERFORM ZEILE-SCHREIBEN
     ‚*€   END-IF.
     ‚*€
     ‚*€   SET      DB-PTR TO PDSPFD-DP.
     ‚*€   PERFORM  DB-CLOSE.

       LES-DFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER GOTO
     ‚*--------------------------------------------------------------
       PRUEFE-GOTO SECTION.
       PRU-GTO-00.

     ‚* GIBT ES SPRUNGMARKEN?
           IF       TAB2-POS = ZEROES
                    GO TO PRU-GTO-90
           END-IF.

           INITIALIZE SECNR-SAVE.
           INITIALIZE SP-MARKE-SAV.

           MOVE     1 TO TAB-POS2.
           MOVE     1 TO TAB2-POS2.
           MOVE     SP-MARKE OF TAB(TAB-POS2) TO SP-MARKE-SAV.

       PRU-GTO-20.
     ‚* GO TO-ANWEISUNG FÜR SPRUNGMARKE GEFUNDEN?
           IF       SP-MARKE OF TAB(TAB-POS2) = GOTO OF TAB2(TAB2-POS2)
                    MOVE 1 TO VERWENDET OF TAB(TAB-POS2)
                    IF   SECNR OF TAB(TAB-POS2) NOT =
                         SECNR OF TAB2(TAB2-POS2)
                         MOVE GOTO  OF TAB2(TAB2-POS2) TO MELDUNG
                         MOVE ZEILE OF TAB2(TAB2-POS2) TO ZEILE-WRK
                         MOVE 2 TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
           END-IF.
           IF       TAB2-POS2 >= TAB2-POS
                    IF   TAB-POS2 >= TAB-POS
                         GO TO PRU-GTO-90
                    END-IF
                    MOVE 1 TO TAB2-POS2
                    ADD  1 TO TAB-POS2
     ‚*             HAT DIE SPRUNGMARKE DEN AUFBAU 3-3-2 DANN SOLLTEN
     ‚*             ALLE IN IN DIESER SECTION SO BEGINNEN
                    IF   SECNR OF TAB(TAB-POS2) NOT = SECNR-SAVE
                         MOVE SECNR OF TAB(TAB-POS2)    TO SECNR-SAVE
                         MOVE SP-MARKE OF TAB(TAB-POS2) TO SP-MARKE-SAV
                    ELSE
     ‚*                  PRÜFEN AUFBAU 3-3-2 (XXX-XXX-XX)
                         IF   SP-MARKE-SAV(4:1) = "-"
                          AND SP-MARKE-SAV(8:1) = "-"
                          AND SP-MARKE-SAV(9:2) NUMERIC
                          AND SP-MARKE-SAV(11: ) = SPACES
                          AND SP-MARKE-SAV(1:8) NOT =
                              SP-MARKE OF TAB(TAB-POS2)(1:8)
                              MOVE SP-MARKE OF TAB(TAB-POS2) TO MELDUNG
                              MOVE ZEILE OF TAB(TAB-POS2)  TO ZEILE-WRK
                              MOVE 10 TO KZ-WRK
                              PERFORM ZEILE-SCHREIBEN
                         END-IF
                    END-IF
           ELSE
                    ADD  1 TO TAB2-POS2
           END-IF.

           GO TO    PRU-GTO-20.

       PRU-GTO-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER SECTIONS
     ‚*--------------------------------------------------------------
       CHECK-SECTION-VERWENDET SECTION.
       CHK-SCV-00.

           IF       TAB3-POS = ZEROES
                    GO TO CHK-SCV-90
           END-IF.

           MOVE     1 TO TAB3-POS2.
           MOVE     1 TO TAB4-POS2.

       CHK-SCV-20.
           IF       TAB3-POS2 > TAB3-POS
                    GO TO CHK-SCV-90
           END-IF.

     ‚* DIE ERSTE SECTION WIRD NIEMALS AUFGERUFEN, HIERBEI HANDELT ES
     ‚* SICH UM DIE STEUERSECTION
           IF       TAB3-POS2 = 1
                    ADD  1 TO TAB3-POS2
                    MOVE 1 TO TAB4-POS2
                    GO TO CHK-SCV-20
           END-IF
           IF       SECNAM OF TAB3(TAB3-POS2) =
                    SECNAM OF TAB4(TAB4-POS2)
                    ADD  1  TO TAB3-POS2
                    INITIALIZE TAB4-POS2
           END-IF.

           IF       TAB4-POS2 >= TAB4-POS
     ‚*             SECTION WURDE NICHT GEFUNDEN, FEHLER
     ‚*             IN DATEI SCHREIBEN
                    MOVE SECNAM OF TAB3(TAB3-POS2) TO MELDUNG
                    MOVE ZEILE  OF TAB3(TAB3-POS2) TO ZEILE-WRK
                    MOVE 3 TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN

                    ADD  1 TO TAB3-POS2
                    MOVE 1 TO TAB4-POS2
           ELSE
                    ADD  1 TO TAB4-POS2
           END-IF.

     ‚* PRÜFEN OB EINE SPRUNGMARKE MIT PERFORM AUFGERUFEN WIRD.
     ‚* MEIST IST DIES EINE SECTION WO VERGESSEN WURDE "SECTION"
     ‚* ANZUGEBEN
           IF       TAB3-POS2 = 2
                    MOVE 1 TO TAB-POS2
                    PERFORM TAB-POS TIMES
                          ADD 1 TO TAB-POS2
                          IF  SECNAM   OF TAB4(TAB4-POS2) =
                              SP-MARKE OF TAB(TAB-POS2)
     ‚*                       SPRUNGMARKE WIRD MIT PERFORM
     ‚*                       AUFGERUFEN (FEHLENDES "SECTION")
                              MOVE SP-MARKE OF TAB(TAB-POS2) TO MELDUNG
                              MOVE ZEILE  OF TAB(TAB-POS2) TO ZEILE-WRK
                              MOVE 11 TO KZ-WRK
                              PERFORM ZEILE-SCHREIBEN
                          END-IF
                    END-PERFORM
           END-IF.
           GO TO    CHK-SCV-20.

       CHK-SCV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER EXIT
     ‚*--------------------------------------------------------------
       CHECK-EXIT-VORHANDEN SECTION.
       CHK-EXT-00.

           INITIALIZE TAB3-POS2.
           PERFORM  TAB3-POS TIMES
                    ADD 1 TO TAB3-POS2
                    IF  TAB3-POS2 > 1
                        IF   EXIT-KZ OF TAB3(TAB3-POS2) = ZEROES
                             MOVE SECNAM OF TAB3(TAB3-POS2) TO MELDUNG
                             MOVE ZEILE OF TAB3(TAB3-POS2) TO ZEILE-WRK
                             MOVE 6 TO KZ-WRK
                             PERFORM ZEILE-SCHREIBEN
                        END-IF
                    END-IF
           END-PERFORM.

       CHK-EXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB PGM-WRK VORHANDEN IST
     ‚*--------------------------------------------------------------
       CHECK-SONSTIGES SECTION.
       CHK-SON-00.

     ‚* WENN PGM-WRK NICHT VORHANDEN IST, DANN FEHLER AUSGEBEN
           IF       PGM-WRK-VORHANDEN = ZEROES
                    MOVE "PGM-WRK" TO MELDUNG
                    MOVE ZEROES    TO ZEILE-WRK
                    MOVE 13 TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN
           END-IF.

     ‚* PRÜFEN OB FIRST-CALL ANGEGEBEN WURDE
           IF       FIRST-CALL-GEFUNDEN > ZERO AND < 3
                    MOVE "FIRST-CALL"     TO MELDUNG
                    MOVE 18               TO KZ-WRK
                    MOVE ZEILE-FIRST-CALL TO ZEILE-WRK
                    PERFORM ZEILE-SCHREIBEN
           END-IF.

       CHK-SON-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ALLE SPRUNGMARKEN VERWENDET WERDEN
     ‚*--------------------------------------------------------------
       PRUEFE-SPRUNGMARKEN-VERWENDET SECTION.
       PRU-SPV-00.

           MOVE     ZEROES TO TAB-POS2 OF TAB.
     ‚* GIBT ES SPRUNGMARKEN?
           IF       TAB-POS OF TAB = ZEROES
                    GO TO PRU-SPV-90
           END-IF.

       PRU-SPV-20.
           ADD      1 TO TAB-POS2 OF TAB.
           IF       TAB-POS2 OF TAB > TAB-POS OF TAB
                    GO TO PRU-SPV-90
           END-IF.

     ‚* NAME GEFÜLLT?
           MOVE     SP-MARKE OF TAB(TAB-POS2 OF TAB) TO SP-MARKE-SAV.
           IF       SP-MARKE-SAV = SPACES
                    GO TO PRU-SPV-20
           END-IF.

     ‚* ERMITTELN DER STRINGLÄNGE, DAMIT DIE LETZTEN 3 ZIFFERN
     ‚* GEPRÜFT WERDEN KÖNNEN
           MOVE     LENGTH OF SP-MARKE-SAV TO STRMAX-WRK.
           MOVE     SP-MARKE-SAV           TO STRLEN-WRK.
           PERFORM  GET-LENGTH.
           IF       GETLEN-WRK = ZEROES
                    GO TO PRU-SPV-20
           END-IF.

     ‚* JETZT DIE LETZTEN 3 ZIFFERN EXTRAHIEREN
           INITIALIZE SHORT-SP-NAME.
           IF       GETLEN-WRK > 3
                    MOVE SP-MARKE-SAV(GETLEN-WRK - 2:3) TO
                         SHORT-SP-NAME
           END-IF.

     ‚* BESTIMME SPRUNGMARKEN WERDEN I.D.R. AUCH NICHT
     ‚* VERWENDET, WIE ANFANG/ENDE UND DIE -00 UND -90
           IF       SP-MARKE-SAV  = "ANFANG"
            OR      SP-MARKE-SAV  = "ENDE"
            OR      SHORT-SP-NAME = "-00"
            OR      SHORT-SP-NAME = "-90"
                    GO TO PRU-SPV-20
           END-IF.

     ‚* GO TO-ANWEISUNG FÜR SPRUNGMARKE GEFUNDEN?
           IF       VERWENDET OF TAB(TAB-POS2) = ZEROES
                    MOVE SP-MARKE OF TAB(TAB-POS2) TO MELDUNG
                    MOVE ZEILE OF TAB(TAB-POS2)    TO ZEILE-WRK
                    MOVE 24 TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN
           END-IF.

           GO TO    PRU-SPV-20.

       PRU-SPV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ALLE SPRUNGMARKEN RICHTLINIENKONFORM SIND
     ‚*--------------------------------------------------------------
       PRUEFE-SPRUNGMARKEN-RICHTLINIE SECTION.
       PRU-SPR-00.

           MOVE     ZEROES TO TAB-POS2 OF TAB.
     ‚* GIBT ES SPRUNGMARKEN?
           IF       TAB-POS OF TAB = ZEROES
     ‚*             DANN FEHLT ANFANG UND ENDE!
                    MOVE     "ANFANG / ENDE"           TO MELDUNG
                    MOVE     ZEROES                    TO ZEILE-WRK
                    MOVE     26 TO KZ-WRK
                    PERFORM  ZEILE-SCHREIBEN
                    GO TO PRU-SPR-90
           END-IF.

       PRU-SPR-20.
           ADD      1 TO TAB-POS2 OF TAB.
           IF       TAB-POS2 OF TAB > TAB-POS OF TAB
                    GO TO PRU-SPR-90
           END-IF.

     ‚* NAME GEFÜLLT?
           MOVE     SP-MARKE OF TAB(TAB-POS2 OF TAB) TO SP-MARKE-SAV.
           IF       SP-MARKE-SAV = SPACES
                    GO TO PRU-SPR-20
           END-IF.

     ‚* BESTIMME SPRUNGMARKEN SIND ZWAR NICHT STANDARDKONFORM, ABER
     ‚* MÜSSEN LAUT RICHTLINIEN VORHANDEN SEIN
           IF       SP-MARKE-SAV  = "ANFANG"
            OR      SP-MARKE-SAV  = "ENDE"
                    GO TO PRU-SPR-20
           END-IF.

     ‚* ES GIBT 2 ARTEN VON SPRUNGMARKEN: ANFNNN ODER XXX-YYY-NN.
           IF       SP-MARKE-SAV(1:3) = "ANF"
            AND     SP-MARKE-SAV(4:3) IS NUMERIC
                    GO TO PRU-SPR-20
           END-IF.

     ‚* UND JETZT XXX-YYY-NN
           IF       SP-MARKE-SAV(1:3) NOT = SPACES
            AND     SP-MARKE-SAV(4:1)     = "-"
            AND     SP-MARKE-SAV(5:3) NOT = SPACES
            AND     SP-MARKE-SAV(8:1)     = "-"
            AND     SP-MARKE-SAV(9:2) IS NUMERIC
            AND     SP-MARKE-SAV(11:)     = SPACES
                    GO TO PRU-SPR-20
           END-IF.

     ‚* SPRUNGMARKE NICHT RICHTLINIENKONFORM
           MOVE     SP-MARKE OF TAB(TAB-POS2) TO MELDUNG.
           MOVE     ZEILE OF TAB(TAB-POS2)    TO ZEILE-WRK.
           MOVE     25 TO KZ-WRK.
           PERFORM  ZEILE-SCHREIBEN.

           GO TO    PRU-SPR-20.

       PRU-SPR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DURCHSUCHEN STRINGTABELLE
     ‚*--------------------------------------------------------------
       SUCHEN SECTION.
       STR-SUC-00.

           INITIALIZE GEFUNDEN.

           MOVE     1 TO POS.
           PERFORM  UNTIL SUCH(POS) = SPACES
     ‚*             SUCHSTRING LÄNGE ERMITTELN
                    MOVE SUCH(POS) TO STR-WRK
                    PERFORM ERMITTELN-LAENGE
                    INITIALIZE LEN
                    INSPECT DATENSATZ TALLYING LEN FOR
                            CHARACTERS BEFORE INITIAL
                            SUCH(POS)(STR-ANF:STR-LEN)
                    IF   LEN < LENGTH OF DATENSATZ
     ‚*                  STARTPOSITION SICHERN
                         COMPUTE S-POS(POS) = LEN + 1
     ‚*                  ENDPOSITION SICHERN
                         COMPUTE E-POS(POS) = LEN + STR-LEN + 1
                         END-COMPUTE
                         MOVE 1 TO GEFUNDEN
                         ADD  1 TO POS
                    ELSE
                         INITIALIZE GEFUNDEN
                         GO TO STR-SUC-90
                    END-IF
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
           MOVE     STR-WRK TO STR-REC(STR-ANZ).
           MOVE     STR-ANF TO S-LEN(STR-ANZ).
           MOVE     STR-LEN TO E-LEN(STR-ANZ).

       ERM-LEN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN EINER STRINGZUWEISUNG AUS FELD "DATENSATZ"
     ‚*--------------------------------------------------------------
       ERMITTELN-STRINGZUWEISUNG SECTION.
       ERM-SZW-00.

           INITIALIZE DATENSATZ-WRK.

           IF       POS <= ZEROES
            OR      POS > LENGTH OF DATENSATZ
                    MOVE 1 TO POS
           END-IF.

           MOVE     ZEROES TO POS2.
           MOVE     ZEROES TO ABBRUCH-KZ.
           MOVE     ZEROES TO UEBERNAHME.
           MOVE     SPACES TO ZEICHEN-WRK.

     ‚* ZUNÄCHST POSITION VOM STRING ANFANG ERMITTELN.
           PERFORM  UNTIL ABBRUCH-KZ = 1
                    IF   POS > LENGTH OF DATENSATZ
                         MOVE 1 TO ABBRUCH-KZ
                    ELSE
                         IF   ZEICHEN-WRK = SPACES
                          AND (    DATENSATZ(POS:1) = '"'
                               OR  DATENSATZ(POS:1) = "'" )
                               MOVE DATENSATZ(POS:1) TO ZEICHEN-WRK
                               MOVE 1 TO ABBRUCH-KZ
                         END-IF
                         ADD  1 TO POS
                    END-IF
           END-PERFORM.

       ERM-SZW-20.
           IF       POS > LENGTH OF DATENSATZ
            OR      ZEICHEN-WRK = SPACES
                    GO TO ERM-SZW-90
           END-IF.

           IF       POS + 1 <= LENGTH OF DATENSATZ
                    IF   DATENSATZ(POS:1)     = ZEICHEN-WRK
                     AND DATENSATZ(POS + 1:1) = ZEICHEN-WRK
                         ADD  1 TO POS
                         ADD  1 TO POS2
                         MOVE DATENSATZ(POS:1) TO DATENSATZ-WRK(POS2:1)
                         ADD  1 TO POS
                         GO TO ERM-SZW-20
                    END-IF
                    IF   DATENSATZ(POS:1) = ZEICHEN-WRK
                     AND DATENSATZ(POS + 1:1) NOT = ZEICHEN-WRK
                         GO TO ERM-SZW-90
                    END-IF
           END-IF.


           ADD      1 TO POS2.
           MOVE     DATENSATZ(POS:1) TO DATENSATZ-WRK(POS2:1)
           ADD      1 TO POS.

           GO TO    ERM-SZW-20.

       ERM-SZW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DEKLARATIONSEBENE
     ‚*--------------------------------------------------------------
       ERMITTELN-EBENE SECTION.
       ERM-EBE-00.

           INITIALIZE EBENE-WRK.

           INITIALIZE LEN.
           INSPECT  DATENSATZ TALLYING LEN FOR LEADING SPACES.
           IF       LEN >= LENGTH OF DATENSATZ
                    CONTINUE
           ELSE
                    ADD   1 TO LEN
                    IF    DATENSATZ(LEN:2) NUMERIC
                          MOVE DATENSATZ(LEN:2) TO EBENE-WRK
                    END-IF
           END-IF.

       ERM-EBE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATENSATZ AUSRICHTEN
     ‚*--------------------------------------------------------------
       AUSRICHTEN SECTION.
       AUS-DTA-00.

           INITIALIZE DATENSATZ-WRK.

     ‚* UNGÜLTIGE POSITION ? => DANN LIEBER NICHTS TUN
           IF      POS = ZEROES
            OR     POS > LENGTH OF DATENSATZ
                   GO TO AUS-DTA-90
           END-IF.

           MOVE    DATENSATZ(POS:) TO VAR-TMP.

           IF      VAR-TMP = SPACES
                   GO TO AUS-DTA-90
           END-IF.

     ‚* LINKSBÜNDIG DEN SECTIONNAHME IN DATENSATZ FÜLLEN
           INITIALIZE LEN.
           INSPECT  VAR-TMP TALLYING LEN FOR LEADING SPACES.
           MOVE     VAR-TMP(LEN + 1: ) TO DATENSATZ-WRK.

       AUS-DTA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WIEDERHERSTELLEN DER WORK-VARIABLEN AUS DER SOURCE
     ‚*--------------------------------------------------------------
       WIEDERHERSTELLEN SECTION.
       WIE-HER-00.

           MOVE     SRCDAT OF QSETSRC-P TO DATUM-WRK.
           MOVE     SRCSEQ OF QSETSRC-P TO ZEILE-WRK.
           MOVE     FUNCTION UPPER-CASE(SRCDTA OF QSETSRC-P(8:66)) TO
                    DATENSATZ(8: ).
           MOVE     SRCDTA OF QSETSRC-P TO DATENSATZ-FULL.

       WIE-HER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTE GEFUNDENE SPRUNGMARKE IN SECTION-TABELLE EINTRAGEN
     ‚*--------------------------------------------------------------
       UPDATE-SECTION-TABELLE SECTION.
       UPD-SCT-00.

           INITIALIZE TAB-COUNTER.

       UPD-SCT-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > TAB3-POS OF TAB3
                    GO TO UPD-SCT-90
           END-IF.

     ‚* WIR SUCHEN DIE AKTUELL VERWENDETE SECTION
           IF       SECNAM OF TAB-REC OF TAB3(TAB-COUNTER)     = SPACES
            OR      SECNAM OF TAB-REC OF TAB3(TAB-COUNTER) NOT =
                    AKTUELLE-SECTION
                    GO TO UPD-SCT-20
           END-IF.

     ‚* IST DER SPRUNGMARKENNAME SCHON GEFÜLLT?
           IF       SP-NAME OF TAB-REC OF TAB3(TAB-COUNTER) = SPACES
                    MOVE LAST-SP-NAME TO
                         SP-NAME OF TAB-REC OF TAB3(TAB-COUNTER)
     ‚*             DANN KÖNNEN WIR AUCH DIREKT PRÜFEN OB DIESE
     ‚*             SPRUNGMARKE ENTWEDER "ANFANG" HEIßT ODER MIT
     ‚*             "-00" AUFHÖRT. ALLES ANDERE WÄRE FALSCH
                    IF   LAST-SP-NAME      NOT = "ANFANG"
                     AND LAST-SP-NAME(8:3) NOT = "-00"
                         MOVE LAST-SP-NAME             TO MELDUNG
                         MOVE DATENSATZ                TO SOURCE-ZEILE
                         MOVE 35                       TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
           END-IF.

       UPD-SCT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE VIA MAPDTA EINLESEN
     ‚*--------------------------------------------------------------
       READ-SOURCE-MAPDTA SECTION.
       REA-SRC-00.

     ‚* ALLE RÜCKGABEWERTE ZURÜCKSETZEN
           INITIALIZE RET-CODE.
           INITIALIZE LAST-SEQNBR.
           INITIALIZE FIRST-SEQNBR.
           INITIALIZE STR-ANZ, STR-ANZ2.
     ‚* SONSTIGE WERTE INITIALISIEREN
           INITIALIZE SRCLIB-WRK, SRCRCN-WRK, SRCMBR-WRK.
           INITIALIZE SRCZEI-WRK, ANZREC-SAV.
     ‚* POINTER AUF NULL SETZEN
           SET      TEMP-PTR    TO NULL.
           SET      SECTION-PTR TO NULL.

     ‚* TEILDATEI EINLESEN
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
                                    SOURCE-PTR
                                    SECTION-PTR
                                    RET-CODE
           END-CALL.
     ‚* DIE POINTER JETZT DER ZENTRALEN POINTERVERWALTUNG ANHÄNGEN
           IF       SOURCE-PTR NOT = NULL
                    SET     PTR-ADD TO TRUE
                    SET     PTR-INPUT OF PTRMAP-REC TO SOURCE-PTR
                    MOVE    "SOURCE"   TO PTR-KEY OF PTRMAP-REC
                    PERFORM COPY-PTR-MAPPING
           END-IF.
           IF       SECTION-PTR NOT = NULL
                    SET     PTR-ADD TO TRUE
                    SET     PTR-INPUT OF PTRMAP-REC TO SECTION-PTR
                    MOVE    "SECTION"  TO PTR-KEY OF PTRMAP-REC
                    PERFORM COPY-PTR-MAPPING
           END-IF.

       REA-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIVISION-/SECTIONABHÄNGIG NACH FEHLERN SUCHEN
     ‚*--------------------------------------------------------------
       FIND-DIVISION-SPECIFIC-COMMAND SECTION.
       FND-DSC-00.

           EVALUATE PGM-TEIL-AKTUELL
     ‚*             WIR BEFINDEN UNS IN DER˜IDENTIFICATION DIVISION‚
                    WHEN 1
                         PERFORM CHECK-PROGRAM-ID
     ‚*             WIR BEFINDEN UNS IN DER˜INPUT-OUTPUT DIVISION‚
                    WHEN 4
                         PERFORM CHECK-DATEI-DEFINITION
     ‚*             WIR BEFINDEN UNS IN DER˜WORKING-STORAGE‚ODER IN DER
     ‚*            ˜LOCAL-STORAGE SECTION‚
                    WHEN 7
                    WHEN 8
                         PERFORM CHECK-FELD-DEKLARATIONEN
           END-EVALUATE.

       FND-DSC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN AN WELCHER STELLE IM PROGRAMM SICH DIE PRÜFROUTINE
     ‚* ZUR ZEIT BEFINDET
     ‚*--------------------------------------------------------------
       GET-PGM-PUNKT SECTION.
       GET-PGP-00.

     ‚* AUS PERFORMANCE GRÜNDEN WIRD HIER STR-ANZ2 INITIALISIERT, DAMIT
     ‚* IN DER SECTION "ERMITTELN-LAENGE" GLEICH AUF DEN ERSTEN
     ‚* TABELLENEINTRAG GEPRÜFT WIRD
           INITIALIZE STR-ANZ2.

       GET-PGP-20.
           INITIALIZE SUCH-TAB.
           EVALUATE PGM-TEIL
     ‚*             IDENTIFICATION
                    WHEN  1 MOVE "IDENTIFICATION"  TO SUCH(01)
                            MOVE " DIVISION"       TO SUCH(02)
     ‚*             ENVIRONMENT
                    WHEN  2 MOVE "ENVIRONMENT"     TO SUCH(01)
                            MOVE " DIVISION"       TO SUCH(02)
                    WHEN  3 MOVE "CONFIGURATION"   TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
                    WHEN  4 MOVE "INPUT-OUTPUT"    TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
     ‚*             DATA
                    WHEN  5 MOVE "DATA"            TO SUCH(01)
                            MOVE " DIVISION"       TO SUCH(02)
                    WHEN  6 MOVE "FILE"            TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
                    WHEN  7 MOVE "WORKING-STORAGE" TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
                    WHEN  8 MOVE "LOCAL-STORAGE"   TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
                    WHEN  9 MOVE "LINKAGE"         TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
     ‚*             PROCEDURE
                    WHEN 10 MOVE "PROCEDURE"       TO SUCH(01)
                            MOVE " DIVISION"       TO SUCH(02)
                    WHEN 11 MOVE "STEUER"          TO SUCH(01)
                            MOVE "SECTION"         TO SUCH(02)
                    WHEN OTHER
                         GO TO GET-PGP-90
           END-EVALUATE.

     ‚* JETZT SUCHEN
           PERFORM  SUCHEN.
           IF       GEFUNDEN NOT = 1
                    ADD 1 TO PGM-TEIL
                    GO TO GET-PGP-20
           END-IF.

     ‚* NEUER PROGRAMMPUNKT ERMITTELT
           MOVE     PGM-TEIL TO PGM-PUNKT-AKTUELL.
           EVALUATE PGM-TEIL
     ‚*             IDENTIFICATION
                    WHEN  1 MOVE 1 TO IDENTIFICATION-DA OF DIVISION-BIT
     ‚*             ENVIRONMENT
                    WHEN  2 MOVE 1 TO ENVIRONMENT-DA    OF DIVISION-BIT
                    WHEN  3 MOVE 1 TO CONFIGURATION-SEC OF DIVISION-BIT
                    WHEN  4 MOVE 1 TO INPUTOUTPUT-SEC   OF DIVISION-BIT
     ‚*             DATA
                    WHEN  5 MOVE 1 TO DATA-DA           OF DIVISION-BIT
                    WHEN  6 MOVE 1 TO FILE-SEC          OF DIVISION-BIT
                    WHEN  7 MOVE 1 TO WORKSTORE-SEC     OF DIVISION-BIT
                    WHEN  8 MOVE 1 TO LOCASTORE-SEC     OF DIVISION-BIT
                    WHEN  9 MOVE 1 TO LINKAGE-SEC       OF DIVISION-BIT
     ‚*             PROCEDURE
                    WHEN 10 MOVE 1 TO PROCEDURE-DA      OF DIVISION-BIT
                    WHEN 11 MOVE 1 TO STEUER-SEC        OF DIVISION-BIT
                    WHEN OTHER
                         GO TO GET-PGP-90
           END-EVALUATE.
     ‚* MIT JEDEM NEUEN PROGRAMMTEIL KANN DIE SICHERUNG DER
     ‚* SUCHTABELLE INITIALISIERT WERDEN, DA NICHT MEHR
     ‚* NACH DIESEN STRINGS GESUCHT WIRD.
           INITIALIZE STR-TAB.

       GET-PGP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER LÄNGE
     ‚*--------------------------------------------------------------
       GET-LENGTH SECTION.
       GET-LEN-00.

           INITIALIZE GETLEN-WRK.
           MOVE     STRMAX-WRK TO GETLEN-WRK.
     ‚* PRÜFEN BZW. SETZEN DER MAX. LÄNGE
           IF       GETLEN-WRK = ZEROES
            OR      GETLEN-WRK > LENGTH OF STRLEN-WRK
                    COMPUTE GETLEN-WRK = LENGTH OF STRLEN-WRK
           END-IF.

     ‚* LÄNGE DES STRINGS ERMITTELN
           PERFORM  UNTIL GETLEN-WRK                   = ZEROES
                     OR   STRLEN-WRK(GETLEN-WRK:1) NOT = SPACE
                    SUBTRACT 1 FROM GETLEN-WRK
           END-PERFORM.

       GET-LEN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE WICHTIGEN SECTIONS UND DEREN REIHENFOLGE ERMITTELN
     ‚*--------------------------------------------------------------
       CHECK-DIV-SECTIONS SECTION.
       CHK-DIV-00.

     ˜* IDENTIFICATION DIVISION
     ˜* ENVIRONMENT DIVISION
     ‚*  CONFIGURATION SECTION
     ‚*  INPUT-OUTPUT SECTION
     ˜* DATA DIVISION
     ‚*  FILE SECTION
     ‚*  WORKING-STORAGE SECTION
     ‚*  LOCAL-STORAGE SECTION (NUR BEI REKURSIVEN PROGRAMMEN)
     ‚*  LINKAGE SECTION
     ˜* PROCEDURE DIVISION
     ‚*  STEUER SECTION

       CHK-DIV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB SPRUNGMARKE BEREITS VORHANDEN
     ‚*--------------------------------------------------------------
       CHECK-SPRUNGMARKE SECTION.
       CHK-MAR-00.

     ‚* BISHER KEINE SPRUNGMARKE IN DER TABELLE VORHANDEN, DANN AUCH
     ‚* NICHT PRÜFEN
           IF         TAB-POS = ZEROES
                      GO TO CHK-MAR-90
           END-IF.

           INITIALIZE TAB-POS2.

       CHK-MAR-20.
           ADD      1 TO TAB-POS2.
           IF       SP-MARKE OF TAB(TAB-POS2) = MELDUNG
                    MOVE ZEILE OF TAB(TAB-POS2) TO ZEILE-WRK
                    MOVE "1"                    TO RET-CODE
           ELSE
     ‚*             ENDE DER EINTRÄGE ERREICHT ?
                    IF   TAB-POS2 NOT = TAB-POS
                         GO TO CHK-MAR-20
                    END-IF
           END-IF.

       CHK-MAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN AUF "PGM-WRK"
     ‚*--------------------------------------------------------------
       CHECK-PGM-WRK SECTION.
       CHK-PGW-00.

           IF       SUCH(01) NOT =  " PGM-WRK"
                    MOVE " PGM-WRK" TO SUCH(01)
                    PERFORM SUCHEN
                    IF   GEFUNDEN = ZERO
                         GO TO CHK-PGW-90
                    END-IF
           END-IF.
           MOVE     E-POS(01) TO POS-SAV.

     ‚* PRÜFEN OB ÜBERHAUPT EINE STRING-ZUWEISUNG DURCH DEN BEFEHL
     ‚* "MOVE" ERFOLGT
           MOVE     "MOVE" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = ZERO
                    GO TO CHK-PGW-90
           END-IF.

     ‚* BEFINDET SICH DAS MOVE HINTER PGM-WRK DANN WIRD PGM-WRK NICHT
     ‚* GEÄNDERT. BEISPIEL: DISPLAY PGM-WRK " MOVE  "PWAEPOS2""
           IF       POS-SAV < E-POS(01)
                    GO TO CHK-PGW-90
           END-IF.

     ‚* ERMITTELN STRINGZUWEISUNG AUS FELD "DATENSATZ"
           MOVE     E-POS(01) TO POS.
           PERFORM  ERMITTELN-STRINGZUWEISUNG.
           IF       DATENSATZ-WRK = SPACES
            OR      POS-SAV < POS
                    GO TO CHK-PGW-90
           END-IF.

           MOVE     1 TO PGM-WRK-VORHANDEN.

     ‚* PRÜFEN OB "PGM-WRK" MIT FLASCHEM PROGRAMMNAMEN GEFÜLLT WIRD
           IF       DATENSATZ-WRK NOT = X-CRMBR
                    MOVE 12 TO KZ-WRK
                    MOVE VAR-TMP TO MELDUNG
                    PERFORM ZEILE-SCHREIBEN
           END-IF.

       CHK-PGW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB BEFEHL EIN "PERFORM" IST
     ‚*--------------------------------------------------------------
        CHECK-PERFORM SECTION.
        CHK-PER-00.

           MOVE     DATENSATZ(POS: ) TO VAR-TMP.

        CHK-PER-10.
           IF       VAR-TMP = SPACES
                    PERFORM READ-NEXT
                    IF   RET-CODE NOT = SPACES
                         GO TO CHK-PER-90
                    END-IF
                    MOVE DATENSATZ TO VAR-TMP
                    GO TO CHK-PER-10
           END-IF.

     ‚* LINKSBÜNDIG DEN SECTIONNAHME IN DATENSATZ FÜLLEN
           INITIALIZE LEN.
           INSPECT  VAR-TMP TALLYING LEN FOR LEADING SPACES.
           MOVE     VAR-TMP(LEN + 1: ) TO DATENSATZ-WRK.

           INSPECT  DATENSATZ-WRK REPLACING ALL "." BY SPACE.

     ‚* ALLES HINTER DEM SECTIONNAMEN ABSCHNEIDEN Z.B.
     ‚* PERFORM "SECTION" UNTIL F3 = "1"
           INITIALIZE LEN.
           INSPECT  DATENSATZ-WRK TALLYING LEN FOR CHARACTERS BEFORE
                    INITIAL SPACE.
           IF       LEN < LENGTH OF DATENSATZ-WRK
                    INITIALIZE DATENSATZ-WRK(LEN + 1: )
           END-IF.

     ‚* HANDELT ES SICH UM DIE PARAMETER DEFINITION DANN HANDELT ES
     ‚* SICH NICHT UM EINEN SORT-BEFEHL
           IF       DATENSATZ-WRK = "DIVISION"
                    GO TO CHK-PER-90
           END-IF.

     ‚* BEFINDET SICH DER NAME DER AUFZURUFENDEN SECTION IN DER
     ‚* NÄCHSTEN ZEILE ? DANN NÄCHSTE ZEILE EINLESEN
           IF       DATENSATZ-WRK = SPACES
                    PERFORM READ-NEXT
                    IF   RET-CODE NOT = SPACES
                         GO TO CHK-PER-90
                    END-IF
                    GO TO CHK-PER-00
           END-IF.

     ‚* NUN PRÜFEN OB PERFORM BEREITS VORHANDEN
           INITIALIZE TAB4-POS2.

        CHK-PER-20.
           ADD     1 TO TAB4-POS2.
           IF      SECNAM OF TAB4(TAB4-POS2) = DATENSATZ-WRK
                   CONTINUE
           ELSE
                   IF  TAB4-POS > TAB4-POS2
                       GO TO CHK-PER-20
                   END-IF
     ‚*            PERFORMNAME IN TABELLE SCHREIBEN
                   ADD  1 TO TAB4-POS
                   MOVE DATENSATZ-WRK       TO SECNAM OF TAB4(TAB4-POS)
                   MOVE SRCSEQ OF QSETSRC-P TO ZEILE  OF TAB4(TAB4-POS)
           END-IF.

        CHK-PER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB BEFEHL EIN "SECTION" IST
     ‚*--------------------------------------------------------------
        CHECK-SECTION SECTION.
        CHK-SEC-00.

     ‚* FÜLLEN DER VAR-TMP OHNE FÜHRENDE SPACES
           MOVE     DATENSATZ(8:S-POS(01) - 7) TO VAR-TMP.

           IF       VAR-TMP = SPACES
                    GO TO CHK-SEC-90
           END-IF.

     ‚* LINKSBÜNDIG DEN SECTIONNAHME IN DATENSATZ FÜLLEN
           INITIALIZE LEN.
           INSPECT  VAR-TMP TALLYING LEN FOR LEADING SPACES.
           MOVE     VAR-TMP(LEN + 1: ) TO DATENSATZ-WRK.

     ‚* BEFINDET SICH DER NAME DER AUFZURUFENDEN SECTION IN DER
     ‚* NÄCHSTEN ZEILE ? DANN NÄCHSTE ZEILE EINLESEN
           IF       DATENSATZ-WRK = SPACES
                    PERFORM READ-NEXT
                    IF   RET-CODE NOT = SPACES
                         GO TO CHK-SEC-90
                    END-IF
                    GO TO CHK-SEC-00
           END-IF.

     ‚* HIER SECTIONZÄHLER UM EINS ERHÖHEN
           ADD      1 TO SECTION-NR.
           MOVE     DATENSATZ-WRK TO AKTUELLE-SECTION.

     ‚* NUN PRÜFEN OB SECTION BEREITS VORHANDEN (DOPPELTE SECTION)
           INITIALIZE TAB3-POS2.

        CHK-SEC-20.
           ADD     1 TO TAB3-POS2.
           IF      SECNAM OF TAB3(TAB3-POS2) = DATENSATZ-WRK
                   MOVE DATENSATZ-WRK          TO MELDUNG
                   MOVE SRCSEQ OF QSETSRC-P    TO ZEILE-WRK
                   MOVE 4 TO KZ-WRK
                   PERFORM ZEILE-SCHREIBEN
           ELSE
                   IF   TAB3-POS > TAB3-POS2
                        GO TO CHK-SEC-20
                   END-IF
     ‚*            PERFORMNAME IN TABELLE SCHREIBEN
                   ADD  1 TO TAB3-POS
                   MOVE DATENSATZ-WRK       TO SECNAM OF TAB3(TAB3-POS)
                   MOVE SRCSEQ OF QSETSRC-P TO ZEILE  OF TAB3(TAB3-POS)
           END-IF.

        CHK-SEC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB BEFEHL EIN "EXIT" IST
     ‚*--------------------------------------------------------------
        CHECK-EXIT SECTION.
        CHK-EXT-00.

           MOVE     DATENSATZ TO DATENSATZ-WRK.

           INSPECT  DATENSATZ-WRK REPLACING ALL "." BY SPACE.

           INITIALIZE POS.
           INSPECT  DATENSATZ-WRK(7: ) TALLYING POS FOR LEADING SPACES.

           ADD      7 TO POS.
           MOVE     DATENSATZ-WRK(POS: ) TO VAR-TMP.

     ‚* PRÜFEN OB BEFEHL EIN "EXIT" IST
           IF       VAR-TMP = "EXIT"
                    IF   TAB3-POS > ZEROES
                         IF   EXIT-KZ OF TAB3(TAB3-POS) = ZEROES
                              MOVE 1 TO EXIT-KZ OF TAB3(TAB3-POS)
                         ELSE
     ‚*                       "EXIT" WURDE DOPPELT DEKLARIERT
                              MOVE DATENSATZ-WRK TO MELDUNG
                              MOVE 7 TO KZ-WRK
                              PERFORM ZEILE-SCHREIBEN
                         END-IF
                    ELSE
     ‚*                  "EXIT" HAT KEIN BEZUG ZU EINER SECTION
                         MOVE DATENSATZ-WRK TO MELDUNG
                         MOVE 8 TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
           END-IF.

        CHK-EXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES SICH UM EINE SPRUNGMARKE HANDELT
     ‚*--------------------------------------------------------------
        CHECK-SP-MARKE SECTION.
        CHK-SPM-00.

           MOVE     1      TO IS-SPRUNGMARKE.
     ‚* PRÜFEN AN WELCHER POSITION DIE SPRUNGMARKE ZU FINDEN IST
           INITIALIZE POS.
           INSPECT  DATENSATZ TALLYING POS FOR LEADING SPACES.
           ADD      1 TO POS.

     ‚* LENGE DES STRINGS ERMITTELN
           INITIALIZE LEN.
           INSPECT  DATENSATZ(POS: ) TALLYING LEN FOR CHARACTERS
                    BEFORE INITIAL SPACES.

     ‚* HANDELT ES SICH UM EINE SPRUNGMARKE, DANN DÜRFEN KEINE
     ‚* WEITEREN INHALTE IN DER ZEILE VORHANDEN SEIN.
           IF       DATENSATZ(POS + LEN: ) NOT = SPACES
                    MOVE     ZEROES TO IS-SPRUNGMARKE
                    GO TO CHK-SPM-90
           END-IF.

           MOVE     DATENSATZ(POS:LEN) TO MELDUNG.
           INSPECT  MELDUNG REPLACING ALL "." BY SPACES.
           PERFORM  CHECK-SPRUNGMARKE.
           IF       RET-CODE NOT = SPACES
                    MOVE 1 TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN
                    GO TO CHK-SPM-90
           END-IF.

           ADD      1 TO TAB-POS.
           MOVE     MELDUNG                TO LAST-SP-NAME.
           MOVE     MELDUNG                TO SP-MARKE OF TAB(TAB-POS).
           MOVE     SECTION-NR             TO SECNR    OF TAB(TAB-POS).
           MOVE     SRCSEQ OF QSETSRC-P    TO ZEILE    OF TAB(TAB-POS).

     ‚* SPRUNGMARE WURDE IN TABELLE GESICHERT. NUN WIRD GEPRÜFT OB
     ‚* VORHERIGE ZEILE MIT EINEM PUNKT ABGESCHLOSSEN WURDE. SONST
     ‚* KOMMT ES ZU PROBLEMEN IN LINUX
           IF       LETZTES-ZEICHEN-SAVE NOT = "."
                    MOVE 20 TO KZ-WRK
                    INITIALIZE MELDUNG
                    PERFORM ZEILE-SCHREIBEN
           END-IF.

        CHK-SPM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB "WITH DUPLICATES" ANGEGEBEN WENN DATEI NICHT UNIQE
     ‚*--------------------------------------------------------------
       CHECK-DATEI-DEFINITION SECTION.
       CHK-DAT-00.

           INITIALIZE SUCH-TAB.

           MOVE     "DATABASE-" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    IF   DATENSATZ(E-POS(01):1) NOT = "Q"
     ‚*                  DAS DATENBANKHANDLING SOLL AUSSCHLIEßLICH ÜBER
     ‚*                  DIE DBMAIN ERFOLGEN, DAHER PRÜFEN OB DIE DATEI
     ‚*                  NOCH FIX EINGEBUNDEN IST. AUSNAHME STELLT Z.B.
     ‚*                  DIE DATEI "QCBLSRC"
                         MOVE DATENSATZ(E-POS(01): ) TO MELDUNG
                         MOVE 17                     TO KZ-WRK
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
     ‚*             DEFINIERTE DATEI IN TABELLE 5 ÜBERNEHMEN
                    ADD  1 TO TAB5-POS
                    MOVE DATENSATZ(E-POS(01): ) TO
                         DATEI OF TAB5(TAB5-POS)
                    MOVE ZEILE-WRK              TO
                         ZEILE OF TAB5(TAB5-POS)
                    GO TO CHK-DAT-90
           END-IF.

           MOVE     "DUPLICATES" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
     ‚*             KENNZEICHEN SETZEN DAS "WITH DUPLICATES" VORHANDEN
                    MOVE 1 TO WDKZ OF TAB5(TAB5-POS)
                    GO TO CHK-DAT-90
           END-IF.

           MOVE     "RECORD KEY" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
     ‚*             KENNZEICHEN SETZEN DAS "RECORD KEY" VORHANDEN
                    MOVE 1 TO RKKZ OF TAB5(TAB5-POS)
           END-IF.

       CHK-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB FELD-DEKLARATIONEN GÜLTIG SIND
     ‚*--------------------------------------------------------------
       CHECK-FELD-DEKLARATIONEN SECTION.
       CHK-FLD-00.

           INITIALIZE SUCH-TAB.

     ‚* ERMITTELN DEKLARIERTE DATEIEN FÜR DB-MAIN
           MOVE     "DB-INFO-DESC" TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    PERFORM EINTRAGEN-DB-ANGABEN
                    GO TO CHK-FLD-90
           END-IF.

     ‚* PRÜFEN VALUE ANGABE FÜR PGM-WRK
           MOVE     "' VALUE '"   TO SUCH(01).
           MOVE     "' PGM-WRK '" TO SUCH(02).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    MOVE 1 TO PGM-WRK-VORHANDEN
                    MOVE E-POS(01) TO POS
                    PERFORM ERMITTELN-STRINGZUWEISUNG
                    IF   DATENSATZ-WRK NOT = X-CRMBR
                         MOVE 12 TO KZ-WRK
                         MOVE DATENSATZ-WRK TO MELDUNG
                         PERFORM ZEILE-SCHREIBEN
                    END-IF
           END-IF.

     ‚* PRÜFEN VALUE ANGABE FÜR PGM-WRK
           EVALUATE INXX-GEFUNDEN
                    WHEN ZERO
                         MOVE "' 01 '"   TO SUCH(01)
                         MOVE "' INXX'" TO SUCH(02)
                         PERFORM SUCHEN
                         IF   GEFUNDEN = 1
                              MOVE 1 TO INXX-GEFUNDEN
                         END-IF
                    WHEN 1
                         PERFORM ERMITTELN-EBENE
     ‚*                  ENDE DER INXX-DEKLARATION ERREICHT ?
                         IF   EBENE-WRK < 02
                          OR  EBENE-WRK = 66
                          OR  EBENE-WRK = 77
                          OR  EBENE-WRK = 88
                              MOVE 2 TO INXX-GEFUNDEN
                         ELSE
                              IF   EBENE-SAVE = ZEROES
                                   MOVE EBENE-WRK TO EBENE-SAVE
                              ELSE
     ‚*                            UNTERSCHIEDLICHE EBENE BEI DER
     ‚*                            DEKLARATION VON INXX NICHT ZULÄSSIG
                                   IF   EBENE-WRK NOT = EBENE-SAVE
                                        MOVE 19              TO KZ-WRK
                                        MOVE DATENSATZ(LEN:) TO MELDUNG
                                        PERFORM ZEILE-SCHREIBEN
                                   END-IF
                              END-IF
                         END-IF
           END-EVALUATE.

       CHK-FLD-90.
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
           MOVE     X-CRFILE     TO OBJ     OF CHKOBJ-REC.
           MOVE     X-CRMBR      TO MBR     OF CHKOBJ-REC.
           MOVE     X-CRLIB      TO LIB     OF CHKOBJ-REC.
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
     ‚* PRÜFEN OB DIE ZEILE EINEN PUNKT AM ENDE BRAUCHT
     ‚*--------------------------------------------------------------
       CHECK-ZEILE-PUNKT SECTION.
       CHK-ZPK-00.

           INITIALIZE SUCH-TAB.
           INITIALIZE KEIN-PUNKT-VORHANDEN.
     ‚* WENN DIE ZEILE SCHON EINEN PUNKT AM ENDE HAT, GEHEN WIR MAL
     ‚* DAVON AUS, DAS DAS RICHTIG IST, DA DER COMPILER DAS EHER
     ‚* ANMECKERT
           MOVE     DATENSATZ           TO STRLEN-WRK.
           MOVE     LENGTH OF DATENSATZ TO STRMAX-WRK.
           PERFORM  GET-LENGTH.
           IF       GETLEN-WRK       = ZEROES
                    GO TO CHK-ZPK-90
           END-IF.

     ‚* JETZT PRÜFEN OB DAS LETZTE ZEICHEN EIN PUNKT IST, WENN JA, DANN
     ‚* PRÜFEN WIR ERSTMAL NICHTS WEITER
           IF       DATENSATZ(GETLEN-WRK:1) = "."
                    GO TO CHK-ZPK-90
           END-IF.

     ‚* WIR HABEN ALSO KEINEN PUNKT AM ENDE. BEFINDEN WIR UNS VLL.
     ‚* IN EINER ANWEISUNG, WO WIR KEINEN PUNKT BRAUCHEN?
           IF       IFEBENE   OF CMD-CHECK > ZEROES
            OR      EVALEBENE OF CMD-CHECK > ZEROES
            OR      CALLEBENE OF CMD-CHECK > ZEROES
            OR      CMPEBENE  OF CMD-CHECK > ZEROES
            OR      ADDEBENE  OF CMD-CHECK > ZEROES
            OR      SUBEBENE  OF CMD-CHECK > ZEROES
            OR      MULEBENE  OF CMD-CHECK > ZEROES
            OR      DIVEBENE  OF CMD-CHECK > ZEROES
            OR      PEREBENE  OF CMD-CHECK > ZEROES
                    GO TO CHK-ZPK-90
           END-IF.

     ‚* JETZT PRÜFEN OB DER COBOL-BEFEHL IN DER NÄCHSTEN ZEILE
     ‚* FORTGESETZT WIRD. WENN JA, KÖNNTE DER PUNKT DORT SEIN.
           MOVE     ZEILE-WRK                 TO ZEILE-SAV.
           MOVE     DATENSATZ                 TO DATENSATZ-SAV.
           PERFORM  CHECK-ZEILE-PUNKT-NEXT.
           IF       KEIN-PUNKT-VORHANDEN = ZEROES
                    GO TO CHK-ZPK-90
           END-IF.
           MOVE     ZEILE-SAV                 TO ZEILE-WRK.
           MOVE     DATENSATZ-SAV             TO DATENSATZ.

     ‚* WENN WIR HIERHIN KOMMEN, FEHLT EIN PUNKT
           MOVE     DATENSATZ                 TO MELDUNG.
           MOVE     DATENSATZ                 TO SOURCE-ZEILE.
           MOVE     27 TO KZ-WRK.
           PERFORM  ZEILE-SCHREIBEN.
           MOVE     1                         TO KEIN-PUNKT-VORHANDEN.

     ‚* NACHDEM DER FEHLER NOTIERT WURDE, DIE DATEN WIEDERHERSTELLEN
           PERFORM  WIEDERHERSTELLEN.

       CHK-ZPK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB IN DER NÄCHSTEN ZEILE EIN PUNKT IST
     ‚*--------------------------------------------------------------
       CHECK-ZEILE-PUNKT-NEXT SECTION.
       CHK-ZPX-00.

     ‚* RET-CODE AUF FEHLER VORBELEGEN
           MOVE     1      TO KEIN-PUNKT-VORHANDEN.

       CHK-ZPX-20.
     ‚* JETZT DIE NÄCHSTE ZEILE LESEN
           PERFORM  READ-NEXT.
           IF       RET-CODE NOT = SPACES
                    GO TO CHK-ZPX-90
           END-IF.

     ‚* PRÜFEN OB DER BEFEHL EIN NEUER COBOL-BEFEHL IST. WENN JA, DANN
     ‚* WURDE DER ALTE BEFEHL NICHT MIT EINEM PUNKT BEENDET.
           PERFORM  CHECK-COBOL-BEFEHL.
           IF       COBOL-BEFEHL = 1
                    GO TO CHK-ZPX-90
           END-IF.

     ‚* WENN DIE ZEILE SCHON EINEN PUNKT AM ENDE HAT, GEHEN WIR MAL
     ‚* DAVON AUS, DAS DAS RICHTIG IST, DA DER COMPILER DAS EHER
     ‚* ANMECKERT
           MOVE     DATENSATZ           TO STRLEN-WRK.
           MOVE     LENGTH OF DATENSATZ TO STRMAX-WRK.
           PERFORM  GET-LENGTH.
           IF       GETLEN-WRK       = ZEROES
                    GO TO CHK-ZPX-90
           END-IF.

     ‚* JETZT PRÜFEN OB DAS LETZTE ZEICHEN EIN PUNKT IST, WENN JA, DANN
     ‚* PRÜFEN WIR ERSTMAL NICHTS WEITER
           IF       DATENSATZ(GETLEN-WRK:1) = "."
                    MOVE ZEROES TO KEIN-PUNKT-VORHANDEN
                    GO TO CHK-ZPX-90
           END-IF.

     ‚* SOLANGE LESEN BIS ENTWEDER EIN NEUER BEFEHL GELESEN WIRD, ODER
     ‚* WIR DEN PUNKT FINDEN ...
           GO TO    CHK-ZPX-20.

       CHK-ZPX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIE ZEILE EINEN COBOL-BEFEHL ENTHÄLT
     ‚*--------------------------------------------------------------
       CHECK-COBOL-BEFEHL SECTION.
       CHK-CBB-00.

           INITIALIZE TAB-COUNTER.
           INITIALIZE COBOL-BEFEHL.

       CHK-CBB-20.
           ADD      1 TO TAB-COUNTER.
           IF       TAB-COUNTER > 1000
            OR      COMMAND OF COBOL-CMD-TAB(TAB-COUNTER) = SPACES
                    GO TO CHK-CBB-90
           END-IF.

           INITIALIZE SUCH-TAB.
     ‚* SUCH-STRING AUFBAUEN
           STRING   "' " DELIMITED BY SIZE
                    COMMAND OF COBOL-CMD-TAB(TAB-COUNTER)
                    DELIMITED BY SPACES
                    " '" DELIMITED BY SIZE
                    INTO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    MOVE 1 TO COBOL-BEFEHL
                    GO TO CHK-CBB-90
           END-IF.

           GO TO    CHK-CBB-20.

       CHK-CBB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WENN EINE SECTION WECHSELT, MUSS JEDES IF ETC DURCH EIN
     ‚* END-IF BEENDET SEIN
     ‚*--------------------------------------------------------------
       CHECK-SECTION-WECHSEL SECTION.
       CHK-SWE-00.

     ‚* MIND. EIN NICHT ABGESCHLOSSENES˜IF
           IF       IFEBENE   OF CMD-CHECK > ZEROES
                    MOVE     SECTION-SAV      TO MELDUNG
                    MOVE     SECTION-SAV      TO SOURCE-ZEILE
                    MOVE     29 TO KZ-WRK
                    PERFORM  ZEILE-SCHREIBEN
           END-IF.

     ‚* MIND. EIN NICHT ABGESCHLOSSENES˜CALL
           IF       CALLEBENE   OF CMD-CHECK > ZEROES
                    MOVE     SECTION-SAV      TO MELDUNG
                    MOVE     SECTION-SAV      TO SOURCE-ZEILE
                    MOVE     31 TO KZ-WRK
                    PERFORM  ZEILE-SCHREIBEN
           END-IF.

     ‚* MIND. EIN NICHT ABGESCHLOSSENES˜EVALUATE
           IF       EVALEBENE   OF CMD-CHECK > ZEROES
                    MOVE     SECTION-SAV      TO MELDUNG
                    MOVE     SECTION-SAV      TO SOURCE-ZEILE
                    MOVE     33 TO KZ-WRK
                    PERFORM  ZEILE-SCHREIBEN
           END-IF.

     ‚* JETZT INITIALISIEREN
           INITIALIZE CMD-CHECK.

       CHK-SWE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEIEN PRÜFEN
     ‚*--------------------------------------------------------------
       CHECK-DATEIEN SECTION.
       CHK-DAT-00.

           INITIALIZE RET-CODE.

       CHK-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER ZEILE NACH RICHTLINIEN
     ‚*--------------------------------------------------------------
       CHECK-SOURCE-ZEILE SECTION.
       CHK-SRZ-00.

     ‚* JETZT PRÜFEN OB WIR IN EINEN ANDEREN PROGRAMMTEIL GELANDET
     ‚* SIND. ALSO VON DER˜DATA DIVISION‚IN DIE˜PROCEDURE DIVISION‚
     ‚* GELANDET SIND
           PERFORM  GET-PGM-PUNKT.

     ‚* ALS ERSTES KÖNNEN WIR JETZT DIVISON BZW. SECTIONABHÄNGIGE
     ‚* BEFEHLE PRÜFEN, Z.B. SUCHEN WIR NUR IN DER˜IDENTIFICATION
     ‚*˜DIVISION‚AUCH NACH DER˜PROGRAM-ID‚
           PERFORM  FIND-DIVISION-SPECIFIC-COMMAND.

     ‚* DIE RESTLICHEN PRÜFUNGEN MACHEN ERST SINN, WENN WIR IN DER
     ‚*˜PROCEDURE DIVISION‚SIND
           IF       PGM-TEIL-AKTUEL < 10
                    GO TO CHK-SRC-90
           END-IF.

     ‚* ERMITTELN DER EBENE, WIRD EIN NEUES IF ERÖFFNET? ODER ENDET
     ‚* EINS? ETC
           PERFORM  CHECK-CMD-EBENE.

       CHK-SRZ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIE PROGRAM-ID GLEICH DEM SOURCENAMEN IST
     ‚*--------------------------------------------------------------
       CHECK-PROGRAM-ID SECTION.
       CHK-PID-00.

     ‚* WURDE SCHON NACH DER PROGRAMM-ID GESUCHT?
           IF       PROGRAM-ID-ERLEDIGT = 1
                    GO TO CHK-PID-90
           END-IF.

           INITIALIZE SUCH-TAB.
           MOVE     "PROGRAM-ID." TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = ZEROES
                    GO TO CHK-PID-90
           END-IF.

     ‚* ZUNÄCHST KENNZEICHEN SETZTEN, DAS PROGRAMM-ID GEFUNDEN WURDE.
           MOVE     1 TO PROGRAM-ID-ERLEDIGT.

     ‚* PROGRAMM-ID AUS ZEICHENKETTE ERMITTELN.
     ‚* BEISPIEL: "PROGRAM-ID.             LGPGM12."
           MOVE     E-POS(01) TO POS.
           MOVE     ZEROES    TO POS2.
           MOVE     ZEROES    TO ABBRUCH-KZ.
           MOVE     SPACES    TO DATENSATZ-WRK.
           PERFORM  UNTIL ABBRUCH-KZ = 1
     ‚*             TEXT SOLANGE ÜBERNEHMEN BIS ENDE DES STRINGS
     ‚*             ERREICHT IST
                    IF   DATENSATZ(POS:1) NOT = SPACES
                     AND DATENSATZ(POS:1) NOT = "."
                         ADD 1 TO POS2
                         MOVE DATENSATZ(POS:1) TO DATENSATZ-WRK(POS2:1)
                    ELSE
                         IF   POS2 > ZEROES
                              MOVE 1 TO ABBRUCH-KZ
                         END-IF
                    END-IF
                    ADD 1 TO POS
     ‚*             ENDE DER VARIABLE ERREICHT ?
                    IF   POS > LENGTH OF DATENSATZ
                         MOVE 1 TO ABBRUCH-KZ
                    END-IF
           END-PERFORM.

     ‚* STIMMT DER SOURCENAME MIT DER PRORGRAMM-ID ÜBEREIN ?
           IF       DATENSATZ-WRK NOT = X-CRMBR
                    STRING "PROGRAM-ID: " DELIMITED BY SIZE
                           DATENSATZ-WRK  DELIMITED BY SIZE
                      INTO MELDUNG
                    END-STRING
                    MOVE    5 TO KZ-WRK
                    PERFORM ZEILE-SCHREIBEN
           END-IF.

       CHK-PID-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN AUF WELCHER EBENE WIR UNS BEFINDEN
     ‚*--------------------------------------------------------------
       CHECK-CMD-EBENE SECTION.
       CHK-CME-00.

           INITIALIZE SUCH-TAB.
     ‚* PRÜFEN OB WIR EIN˜END-IF‚HABEN
           MOVE     "' END-IF'"           TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    IF   IFEBENE OF CMD-CHECK > ZEROES
                         SUBTRACT 1 FROM IFEBENE OF CMD-CHECK
                    ELSE
     ‚*                  DOPPELTES ODER FALSCHES END-IF?
                         MOVE     DATENSATZ        TO MELDUNG
                         MOVE     DATENSATZ        TO SOURCE-ZEILE
                         MOVE     28 TO KZ-WRK
                         PERFORM  ZEILE-SCHREIBEN
                    END-IF
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜END-CALL‚HABEN
           MOVE     "' END-CALL'"         TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    IF   CALLEBENE OF CMD-CHECK > ZEROES
                         SUBTRACT 1 FROM CALLEBENE OF CMD-CHECK
                    ELSE
     ‚*                  DOPPELTES ODER FALSCHES END-CALL?
                         MOVE     DATENSATZ        TO MELDUNG
                         MOVE     DATENSATZ        TO SOURCE-ZEILE
                         MOVE     30 TO KZ-WRK
                         PERFORM  ZEILE-SCHREIBEN
                    END-IF
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜END-EVALUATE‚HABEN
           MOVE     "' END-EVALUATE'"     TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    IF   EVALEBENE OF CMD-CHECK > ZEROES
                         SUBTRACT 1 FROM EVALEBENE OF CMD-CHECK
                    ELSE
     ‚*                  DOPPELTES ODER FALSCHES END-CALL?
                         MOVE     DATENSATZ        TO MELDUNG
                         MOVE     DATENSATZ        TO SOURCE-ZEILE
                         MOVE     32 TO KZ-WRK
                         PERFORM  ZEILE-SCHREIBEN
                    END-IF
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜END-ADD‚HABEN
           MOVE     "' END-ADD'"          TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    IF   ADDEBENE OF CMD-CHECK > ZEROES
                         SUBTRACT 1 FROM ADDEBENE OF CMD-CHECK
                    ELSE
     ‚*                  DOPPELTES ODER FALSCHES END-ADD?
                         MOVE     DATENSATZ        TO MELDUNG
                         MOVE     DATENSATZ        TO SOURCE-ZEILE
                         MOVE     34 TO KZ-WRK
                         PERFORM  ZEILE-SCHREIBEN
                    END-IF
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜END-SUBTRACT‚HABEN
           MOVE     "' END-SUBTRACT'"     TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    IF   SUBEBENE OF CMD-CHECK > ZEROES
                         SUBTRACT 1 FROM SUBEBENE OF CMD-CHECK
                    ELSE
     ‚*                  DOPPELTES ODER FALSCHES END-SUBTRACT?
                         MOVE     DATENSATZ        TO MELDUNG
                         MOVE     DATENSATZ        TO SOURCE-ZEILE
                         MOVE     36 TO KZ-WRK
                         PERFORM  ZEILE-SCHREIBEN
                    END-IF
                    GO TO CHK-CME-90
           END-IF.

     ‚* WENN DIE ZEILE SCHON EINEN PUNKT AM ENDE HAT, BRAUCHEN WIR
     ‚* NICHTS HOCHZÄHLEN, DA DER BEFEHL JA WIEDER BEENDET IST
           MOVE     DATENSATZ           TO STRLEN-WRK.
           MOVE     LENGTH OF DATENSATZ TO STRMAX-WRK.
           PERFORM  GET-LENGTH.
           IF       GETLEN-WRK       = ZEROES
                    GO TO CHK-CME-90
           END-IF.

     ‚* JETZT PRÜFEN OB DAS LETZTE ZEICHEN EIN PUNKT IST, WENN JA, DANN
     ‚* PRÜFEN WIR ERSTMAL NICHTS WEITER
           IF       DATENSATZ(GETLEN-WRK:1) = "."
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜IF‚HABEN
           MOVE     "' IF '"              TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    ADD 1 TO IFEBENE OF CMD-CHECK
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜CALL‚HABEN
           MOVE     "' CALL '"            TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    ADD 1 TO CALLEBENE OF CMD-CHECK
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜EVALUATE‚HABEN
           MOVE     "' EVALUATE '"        TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    ADD 1 TO EVALEBENE OF CMD-CHECK
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜ADD‚HABEN
           MOVE     "' ADD '"             TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    ADD 1 TO ADDEBENE OF CMD-CHECK
                    GO TO CHK-CME-90
           END-IF.

     ‚* PRÜFEN OB WIR EIN˜SUBTRACT‚HABEN
           MOVE     "' SUBTRACT '"        TO SUCH(01).
           PERFORM  SUCHEN.
           IF       GEFUNDEN = 1
                    ADD 1 TO SUBEBENE OF CMD-CHECK
                    GO TO CHK-CME-90
           END-IF.

       CHK-CME-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NACH DEM DURCHLESEN DER SOURCE KÖNNEN JETZT VERSCHIEDENE
     ‚* PRÜFUNGEN DURCHGEFÜHRT WERDEN, Z.B. OB SPRUNGMARKEN VERWENDET
     ‚* WERDEN ODER NICHT
     ‚*--------------------------------------------------------------
       CHECK-SOURCE-FIRST SECTION.
       CHK-SFI-00.

           PERFORM  PRUEFE-GOTO.
           PERFORM  CHECK-SECTION-VERWENDET.
           PERFORM  CHECK-EXIT-VORHANDEN.
           PERFORM  CHECK-DATEIEN.
           PERFORM  CHECK-SONSTIGES.
           PERFORM  PRUEFE-SPRUNGMARKEN-VERWENDET.
           PERFORM  PRUEFE-SPRUNGMARKEN-RICHTLINIE.

       CHK-SFI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

           INITIALIZE LAST-SP-NAME.
           INITIALIZE START-WERT.
           INITIALIZE AKTUELLE-SECTION, SECTION-SAV.
           INITIALIZE CMD-CHECK.
           INITIALIZE TAB, TAB2, TAB3, TAB4, TAB5, TAB6.
           INITIALIZE SUCH-TAB, STR-TAB.
           INITIALIZE OPENMODE-OUT.
           INITIALIZE ZEILE-WRK, ZEILE-SAV, ZEILE-FIRST-CALL.
           MOVE       99 TO ZEILEN.

       INI-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHELERHAFTE ANWEISUNG IN SPOOLDATEI SCHREIBEN
     ‚*--------------------------------------------------------------
       ZEILE-SCHREIBEN SECTION.
       ZEI-SCH-00.

     ‚* KZ-WRK =  1 => DOPPELTE SPRUNGMARKE
     ‚* KZ-WRK =  2 => FEHLERHAFTE GOTO-ANWEISUNG
     ‚* KZ-WRK =  3 => SECTION WIRD NICHT VERWENDET
     ‚* KZ-WRK =  4 => DOPPELT DEKLARIERTE SECTION
     ‚* KZ-WRK =  5 => FALSCHE PROGRAMM-ID
     ‚* KZ-WRK =  6 => FEHLENDES EXIT IN DER SECTION
     ‚* KZ-WRK =  7 => DOPPELTES EXIT IN DER SECTION
     ‚* KZ-WRK =  8 => EXIT OHNE BEZUG ZU EINER SECTION
     ‚* KZ-WRK =  9 => FEHLENDES ODER ÜBERFLÜSSIGES WITH DUPLICATES
     ‚* KZ-WRK = 10 => SPRUNGMARKENNAME INNERHALB SECTION ABWEICHEND
     ‚* KZ-WRK = 11 => FEHLENDE ANGABE VON SECTION
     ‚* KZ-WRK = 12 => VARIABLE PGM-WRK FALSCH GEFÜLLT
     ‚* KZ-WRK = 13 => DEKLARATION VON PGM-WRK FEHLT
     ‚* KZ-WRK = 14 => UNGÜLTIGE DATEIANGABE FÜR DB-MAIN
     ‚* KZ-WRK = 15 => ANGEGEBENE DATEI WIRD NICHT VERWENDET
     ‚* KZ-WRK = 16 => ANGABE DB-OPENMODE UNGÜLTIG
     ‚* KZ-WRK = 17 => DATEI WIRD NICHT MIT DBMAIN GEHANDELT
     ‚* KZ-WRK = 18 => ANGABE "FIRST-CALL" FEHLT ODER IST UNGÜLTIG
     ‚* KZ-WRK = 19 => STUFE DER INXX VARIABLE UNGÜLTIG
     ‚* KZ-WRK = 20 => FEHLENDER PUNKT VOR SPRUNGMARKE
     ‚* KZ-WRK = 21 => DSPATR-FELD FÜR ANWEISUNG EXISTIERT NICHT
     ‚* KZ-WRK = 22 => BEFEHL BEGINNT VOR POSITION 20
     ‚* KZ-WRK = 23 => BEFEHL ÜBERSCHREITET POSITION 80
     ‚* KZ-WRK = 24 => SPRUNGMARKE WIRD NICHT VERWENDET
     ‚* KZ-WRK = 25 => SPRUNGMARKE NICHT STANDARDKONFORM
     ‚* KZ-WRK = 26 => ANFANG UND ENDE SPRUNGMARKE NICHT VORHANDEN
     ‚* KZ-WRK = 27 => FEHLENDER PUNKT
     ‚* KZ-WRK = 28 => DOPPELTES ODER FALSCHES END-IF
     ‚* KZ-WRK = 29 => FEHLENDES END-IF IN SECTION
     ‚* KZ-WRK = 30 => DOPPELTES ODER FALSCHES END-CALL
     ‚* KZ-WRK = 31 => FEHLENDES END-CALL IN SECTION
     ‚* KZ-WRK = 32 => DOPPELTES ODER FALSCHES END-EVALUATE
     ‚* KZ-WRK = 33 => FEHLENDES END-EVALUATE IN SECTION
     ‚* KZ-WRK = 34 => DOPPELTES ODER FALSCHES END-ADD
     ‚* KZ-WRK = 35 => ERSTE SPRUNGMARKE BEGINNT NICHT MIT -00
           IF       PRUEFUNG(KZ-WRK) = ZEROES
                    GO TO ZEI-SCH-90
           END-IF.

     ‚* TEXT AUSGEBEN
           MOVE     "CHK00"                   TO CRFID-WRK(1:5).
           MOVE     KZ-WRK                    TO CRFID-WRK(6:2).

           INITIALIZE PCHKSRCF OF PCHKSRC-P.
     ‚* DIE MEISTEN DATEN (P3, VORGANG, ...) DURCH DEN HAUPTSATZ
     ‚* FÜLLEN
           MOVE     CORR PCHKSRCF OF PCHKSRC-VORGANG TO
                         PCHKSRCF OF PCHKSRC-P.
     ‚* JETZT FEHLERMELDUNG FÜLLEN
           MOVE     DATUM-WRK              TO CRSDTA OF PCHKSRC-P.
           MOVE     ZEILE-WRK              TO CRSECN OF PCHKSRC-P.
           MOVE     CRFID-WRK              TO CRFID  OF PCHKSRC-P.
           MOVE     CRFTYP-WRK             TO CRFTYP OF PCHKSRC-P.
     ‚* ÜBERSCHRIFT
           MOVE     CRFID-WRK              TO CPY-TXID.
           MOVE     SPRACHE-WRK            TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT             TO CRFTXT OF PCHKSRC-P.
           MOVE     MELDUNG                TO CRFTX2 OF PCHKSRC-P.
           MOVE     SOURCE-ZEILE           TO CRLINE OF PCHKSRC-P.
     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "PCHKSRC" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK               TO CRLFDN OF PCHKSRC-P.
     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                 TO CRAUSR OF PCHKSRC-P.
           MOVE     T-TERM                 TO CRABS  OF PCHKSRC-P.
           MOVE     PGM-WRK                TO CRAPGM OF PCHKSRC-P.
           MOVE     DATE-8                 TO CRADTA OF PCHKSRC-P.
           MOVE     TIME-6                 TO CRATIA OF PCHKSRC-P.
           WRITE    PCHKSRC-P.

       ZEI-SCH-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-FILL-CR-DATA: CODE-REVIEW-VARIABLEN FÜLLEN
     ‚*---------------------------------------------------------------

           COPY     CRCPY      OF APG-QCPYSRC.
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

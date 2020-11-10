       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0026.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE-AUSWAHLBEFEHLE AUSFÜHREN                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.05.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN DER AUSWAHLFUNKTION                *
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
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0026".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* SOURCE ZUM ARBEITEN
       01  QSETSRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

       01  SOURCE-WRK                    LIKE SRCDTA OF QSETSRC-WRK.
       01  SRCZEI-WRK                    LIKE SRCSEQ OF QSETSRC-WRK.
       01  RESTORE-KZ                    PIC 9(1).
       01  F12                           PIC X(1).
       01  STRING-WRK                    PIC X(100).

     ‚* INTERNE TABELLE FÜR DIE COPY-MODULE
       01  COPYMODUL-OFFEN               PIC 9(1).

     ‚* LAUFVARIABLEN
       01  IND                           PIC 9(4).

     ‚* HEX-WERTE
       01  RED-RI-HEX-WRK                PIC X(1).

     ‚* LÄNGEN-VARIABLEN
       01  LEN-TMP                       PIC 9(4).
       01  POS-TMP                       PIC 9(4).

     ‚* VARIABLEN FÜR TEILDATEIPRÜFUNG
       01  CHECK-LIB                     PIC X(10).
       01  CHECK-RCN                     PIC X(10).
       01  CHECK-MBR                     PIC X(10).

     ‚* VARIABLEN FÜR DIE ERMITTLUNG DES LETZTEN KEY
       01  LAST-KEY                      LIKE MAP-KEY  OF MAPDTA-REC.
       01  LAST-LFDN                     LIKE MAP-LFDN OF MAPDTA-REC.
       01  LAST-REC                      LIKE MAP-REC  OF MAPDTA-REC.

     ‚* SONSTIGE VARIABLEN
       01  FIRST-SEQNBR                  LIKE SRCSEQ OF QSETSRC-WRK.
       01  LAST-SEQNBR                   LIKE SRCSEQ OF QSETSRC-WRK.

     ‚* POINTER ZUR ANZEIGE DER SOURCE, ETC.
       01  LAST-PTR                      USAGE POINTER.

     ‚* VARIABLEN FÜR GET-SECTION
       01  SECTION-NAME                  PIC X(70).
       01  SECTION-SEQ                   LIKE SRCSEQ OF QSETSRC-WRK.

     ‚* VARIABLEN FÜR DIE ERMITTLUNG DES COPY-MODULS
       01  COPY-DDS-LIB                  PIC X(10).
       01  COPY-DDS-RCN                  PIC X(10).
       01  COPY-DDS-MBR                  PIC X(10).
       01  COPY-DDS-LEN                  PIC 9(4).
       01  COPY-DDS-POS                  PIC 9(4).
       01  COPY-MODUL-OFFEN              PIC 9(1).
       01  COPY-START                    PIC 9(1).

     ‚* PARAMETER FÜR DAS CRP0009
       01  MAP-SUCHE-WRK                 LIKE MAP-REC  OF MAPDTA-REC.
       01  MAP-KEY-WRK                   LIKE MAP-KEY  OF MAPDTA-REC.
       01  MAP-LFDN-WRK                  LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* PARAMETER FÜR DAS CRP0011
       01  NOCHECK-WRK                   PIC X(1).

     ‚* PARAMETER FÜR DAS CRP0025
       01  ANZREC-SAV                    PIC 9(9).

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚* SOURCEDATEI
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
     ‚* ÖFFNUNGSMODUS (2=ÄNDERN, 5=ANZEIGEN)
       01  X-OPENMODE                    PIC X(1).
     ‚* ZEILE BEI DER DIE AUSWAHL AUSGEFÜHRT WURDE
       01  X-SRCSEQ                      LIKE SRCSEQ OF QSETSRC-WRK.
     ‚* DIE EIGENTLICHE AUSWAHL
       01  X-AUSWAHL                     PIC X(1).
     ‚* COPY-MODULE
       01  X-COPY-TAB                    TYPE SEU-COPY-TAB-TYPE.
     ‚* RÜCKGABE EINER ZEILE Z.B. ZUM NACH OBEN/UNTEN SPRINGEN
       01  X-RET-SRCSEQ                  LIKE SRCSEQ OF QSETSRC-WRK.
     ‚* POINTER
       01  X-SOURCE-PTR                  USAGE POINTER.
       01  X-RESTORE-PTR                 USAGE POINTER.
       01  X-NOTIZEN-PTR                 USAGE POINTER.
       01  X-SPRUNG-PTR                  USAGE POINTER.
       01  X-SECTION-PTR                 USAGE POINTER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-OPENMODE
                                X-SRCSEQ
                                X-AUSWAHL
                                X-COPY-TAB
                                X-RET-SRCSEQ
                                X-SOURCE-PTR
                                X-RESTORE-PTR
                                X-NOTIZEN-PTR
                                X-SPRUNG-PTR
                                X-SECTION-PTR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0026"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE X-RET-SRCSEQ.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* VARIABLEN INITIALISIEREN
           PERFORM  INIT-VAR.

     ‚* DIE ÜBERGEBENE ZEILE AUSLESEN
           PERFORM  GET-ZEILE.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÜBERGABEPARAMETER ÜBERNEHMEN
           PERFORM  AUSW-EXECUTE.

       ENDE.

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
     ‚* EINLESEN DER SOURCE
           CALL     "CRP0025" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    COPY-DDS-LIB
                                    COPY-DDS-RCN
                                    COPY-DDS-MBR
                                    SRCZEI-WRK
                                    ANZREC-SAV
                                    FIRST-SEQNBR
                                    LAST-SEQNBR
                                    X-SOURCE-PTR
                                    X-SECTION-PTR
                                    RET-CODE
           END-CALL.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DER AUSWAHL
     ‚*--------------------------------------------------------------
       AUSW-EXECUTE SECTION.
       ASW-EXC-00.

           INITIALIZE RET-CODE.
     ‚* AUSWAHL "5": COPY-MODULE AUFKLAPPEN
           IF       X-AUSWAHL = "5"
     ‚*             COPY-MODULE GIBTS NUR BEI COBOL-SOURCEN
                    IF   SRCTYPE OF RTVMBRD-REC = "CBL"
                     OR  SRCTYPE OF RTVMBRD-REC = "CBLLE"
                     OR  SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
                         PERFORM  AUSW-5-OPENCOPY
                    END-IF
           END-IF.

     ‚* AUSWAHL "B": ZURÜCKSPRINGEN
           IF       X-AUSWAHL = "B"
                    PERFORM  AUSW-B-GOZEILE
           END-IF.

     ‚* AUSWAHL "D": DATUM ÄNDERN
           IF       X-AUSWAHL = "D"
                    PERFORM  AUSW-D-CHGDATE
           END-IF.

     ‚* AUSWAHL "T": DIE ZEILE NACH OBEN SETZEN
           IF       X-AUSWAHL = "T"
                    MOVE X-SRCSEQ TO X-RET-SRCSEQ
           END-IF.

     ‚* AUSWAHL "R": ZEILE MIT "RESTORE" ZURÜCKSETZEN
           IF       X-AUSWAHL = "R"
                    PERFORM  AUSW-R-RESTORE
           END-IF.

     ‚* AUSWAHL "G": ZUR SECTION SPRINGEN
           IF       X-AUSWAHL = "G"
     ‚*             PERFORMS GIBT ES NUR BEI COBOL-SOURCEN
                    IF   SRCTYPE OF RTVMBRD-REC = "CBL"
                     OR  SRCTYPE OF RTVMBRD-REC = "CBLLE"
                     OR  SRCTYPE OF RTVMBRD-REC = "SQLCBLLE"
                         PERFORM  AUSW-G-GOZEILE
                    END-IF
           END-IF.

     ‚* AUSWAHL "N": NOTIZEN ERSTELLEN
           IF       X-AUSWAHL = "N"
                    PERFORM  AUSW-N-WRITENOTE
           END-IF.

       ASW-EXC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN OB IN DER ZEILE AUCH WIRKLICH EIN COPY-MODUL STEHT
     ‚*--------------------------------------------------------------
       AUSW-5-OPENCOPY SECTION.
       AS5-OCP-00.

     ‚* AUSLESEN DES COPY-MODULANEN
           PERFORM  GET-COPY-MODUL.
           IF       COPY-DDS-LIB = SPACES
            OR      COPY-DDS-RCN = SPACES
            OR      COPY-DDS-MBR = SPACES
                    GO TO AS5-OCP-90
           END-IF.

     ‚* IST DAS COPY-MODUL EVTL. SCHON OFFEN?
           PERFORM  CHECK-COPY-MODUL-OFFEN.
           IF       COPY-MODUL-OFFEN = ZEROES
     ‚*             JETZT DAS COPY-MODUL IN MAPDTA SPEICHERN
                    MOVE     X-SRCSEQ     TO SRCZEI-WRK
     ‚*             JETZT INT. TABELLE AKTUALISIEREN
                    PERFORM  ADD-COPY-MOD-TAB
     ‚*             IN MAPDTA NOCH ANFANG/ENDE KENNZEICHNEN
                    MOVE     1            TO COPY-START
                    PERFORM  SET-COPY-MARKE
     ‚*             JETZT DIE SOURCE LESEN
                    PERFORM  LESE-DATEN
     ‚*             IN MAPDTA NOCH ANFANG/ENDE KENNZEICHNEN
                    MOVE     2            TO COPY-START
                    PERFORM  SET-COPY-MARKE
           ELSE
     ‚*             JETZT DAS COPY-MODUL IN MAPDTA LÖSCHEN
                    MOVE     X-SRCSEQ     TO SRCZEI-WRK
                    PERFORM  DLT-MAP-INFO
     ‚*             JETZT INT. TABELLE AKTUALISIEREN
                    PERFORM  RMV-COPY-MOD-TAB
           END-IF.

     ‚* DAMIT WIR WIEDER RICHTIG STARTEN
           MOVE     X-SRCSEQ TO X-RET-SRCSEQ.

       AS5-OCP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZUM LETZTEN GESPEICHERTEN PUNKT ZURÜCKSPRINGEN
     ‚*--------------------------------------------------------------
       AUSW-B-GOZEILE SECTION.
       ASB-GOZ-00.

           INITIALIZE RET-CODE.
     ‚* ERMITTELN DES LETZTEN EINTRAGS UND ANSCHLIEßEND ZU DIESEM
     ‚* ZURÜCKSPRINGEN. ZUM SCHLUSS DANN DEN EINTRAG LÖSCHEN
           SET      LAST-PTR TO X-SPRUNG-PTR.
           PERFORM  GET-LAST-POINTER-KEY.
           IF       LAST-KEY     = SPACES
                    INITIALIZE RET-CODE
                    GO TO ASB-GOZ-90
           END-IF.

     ‚* JETZT DIE SOURCEZEILE DARAUS ERMITTELN
           IF       LAST-REC(1:6) IS NUMERIC
                    MOVE LAST-REC(1:6) TO X-RET-SRCSEQ(1:6)
           END-IF.

     ‚* DEN EINTRAG IMMER LÖSCHEN
           INITIALIZE MAPDTA-REC.
           MOVE     LAST-KEY   TO MAP-KEY  OF MAPDTA-REC.
           MOVE     LAST-LFDN  TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-DELETE TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC  TO X-SPRUNG-PTR.
           PERFORM  COPY-CALL-MAPDTA.

       ASB-GOZ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATUM EINER ZEILE ÄNDERN
     ‚*--------------------------------------------------------------
       AUSW-D-CHGDATE SECTION.
       ASD-CHD-00.

           INITIALIZE RET-CODE.
     ‚* NUR IM ÄNDERN-MODUS SINNIG
           IF       X-OPENMODE NOT = "2"
                    GO TO ASD-CHD-90
           END-IF.

           INITIALIZE F12.
     ‚* DATUM EINLESEN
           CALL     "CRP0017" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    X-SRCSEQ
                                    SRCDAT OF QSETSRC-WRK
                                    F12
           END-CALL.
           IF       F12 = "1"
                    INITIALIZE F12
                    GO TO ASD-CHD-90
           END-IF.

     ‚* LESEN MAPDTA UM DIE ZEILE ZU SCHREIBEN/AKTUALISIEREN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     X-SRCSEQ           TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
     ‚* JETZT DIE SOURCE-WRK IN DEN RECORD SCHIEBEN
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     "C"                TO MAP-REC OF MAPDTA-REC(1:1).
           SET      MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

       ASD-CHD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SPRINGEN ZUR SECTION-DEKLARATION
     ‚*--------------------------------------------------------------
       AUSW-G-GOZEILE SECTION.
       ASG-GOZ-00.

     ‚* AUSLESEN DES SECTIONNAMEN
           PERFORM  GET-SECTION-NAME.
           IF       SECTION-NAME = SPACES
                    GO TO ASG-GOZ-90
           END-IF.

     ‚* JETZT SUCHEN DER ZEILE DIESER SECTION, DIESE HABEN WIR IM
     ‚* SECTION-PTR GESICHERT
           PERFORM  GET-SECTION-ZEILE.
           IF       SECTION-SEQ = ZEROES
                    GO TO ASG-GOZ-90
           END-IF.

     ‚* JETZT NOCH SICHERN DAS WIR Z.B. VON DER ZEILE 320 IN DIE
     ‚* ZEILE 640 GESPRUNGEN SIND, DAMIT WIR MIT "B" AUCH WIEDER
     ‚* ZURÜCKKOMMEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB      TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN      TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR      TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SECTION-SEQ   TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X-SRCSEQ      TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-WRITE     TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-SPRUNG-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-SPRUNG-PTR TO MAP-PTR OF MAPDTA-REC.

     ‚* DAMIT WIR WIEDER RICHTIG STARTEN
           MOVE     SECTION-SEQ   TO X-RET-SRCSEQ.

       ASG-GOZ-90.
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
           CALL     "CRP0006" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    X-SRCSEQ
                                    X-NOTIZEN-PTR
                                    F12
           END-CALL.

       ASN-WNT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE ZURÜCKSETZEN
     ‚*--------------------------------------------------------------
       AUSW-R-RESTORE SECTION.
       ASR-RST-00.

           INITIALIZE RET-CODE.
     ‚* NUR IM ÄNDERN-MODUS SINNIG
           IF       X-OPENMODE NOT = "2"
                    GO TO ASD-CHD-90
           END-IF.

     ‚* ZEILE AUSLESEN
           MOVE     1                  TO RESTORE-KZ.
           PERFORM  GET-ZEILE.
           IF       RET-CODE NOT = SPACES
                    GO TO ASR-RST-90
           END-IF.

     ‚* ZEILE ÜBERSCHREIBEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     X-SRCSEQ           TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     QSETSRC-WRK        TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE          TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR.
           PERFORM  COPY-CALL-MAPDTA.

       ASR-RST-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* INITIALISIEREN ALLER VARIABLEN
     ‚*--------------------------------------------------------------
       INIT-VAR SECTION.
       INI-VAR-00.

     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     X-SRCLIB     TO OBJLIB   OF RTVMBRD-REC.
           MOVE     X-SRCRCN     TO FILENAME OF RTVMBRD-REC.
           MOVE     X-SRCMBR     TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

           IF       LAST-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO LAST-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      LAST-PTR TO NULL.

     ‚* DATUM/ZEIT ERMITTELN
           PERFORM  COPY-GET-TIME.

     ‚* FARBEN EINMALIG ERMITTELN
           PERFORM  GET-COLOUR.

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

       INI-VAR-90.
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
           IF       COPY-DDS-LIB NOT = X-SRCLIB
            OR      COPY-DDS-RCN NOT = X-SRCRCN
            OR      COPY-DDS-MBR NOT = X-SRCMBR
     ‚*             FÜR DIE SORTIERUNG ZEILENNUMMER DER ORIGINALZEILE
     ‚*             WIEDER ÜBERSCHREIBEN
                    MOVE SRCZEI-WRK   TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE COPY-DDS-LIB TO MAP-KEY OF MAPDTA-REC(51:10)
                    MOVE COPY-DDS-RCN TO MAP-KEY OF MAPDTA-REC(61:10)
                    MOVE COPY-DDS-MBR TO MAP-KEY OF MAPDTA-REC(71:10)
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
           IF       COPY-DDS-LIB NOT = X-SRCLIB
            OR      COPY-DDS-RCN NOT = X-SRCRCN
            OR      COPY-DDS-MBR NOT = X-SRCMBR
                    IF   MAP-KEY OF MAPDTA-REC(51:10) NOT =
                         COPY-DDS-LIB
                     OR  MAP-KEY OF MAPDTA-REC(61:10) NOT =
                         COPY-DDS-RCN
                     OR  MAP-KEY OF MAPDTA-REC(71:10) NOT =
                         COPY-DDS-MBR
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
     ‚* LESEN EINER BESTIMMTEN ZEILE
     ‚*--------------------------------------------------------------
       GET-ZEILE SECTION.
       GET-ZEI-00.

           INITIALIZE QSETSRC OF QSETSRC-WRK.
     ‚* LESEN MAPDTA UM DIE ZEILE ZU LESEN
           INITIALIZE MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     X-SRCSEQ           TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'    TO MAP-KEY OF MAPDTA-REC(38:6).
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           IF       RESTORE-KZ NOT = ZEROES
                    SET MAP-PTR OF MAPDTA-REC TO X-RESTORE-PTR
           ELSE
                    SET MAP-PTR OF MAPDTA-REC TO X-SOURCE-PTR
           END-IF.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-WRK
                    MOVE SRCDTA OF QSETSRC-WRK      TO SOURCE-WRK
           END-IF.
           MOVE     ZEROES      TO RESTORE-KZ.

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

     ‚* ÜBERHAUPT EIN COPY VORHANDEN?
           INITIALIZE COPY-DDS-POS.
           INSPECT  SOURCE-WRK TALLYING COPY-DDS-POS
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
           INSPECT  SOURCE-WRK TALLYING COPY-DDS-POS
                    FOR CHARACTERS BEFORE "DDS-".
           IF       COPY-DDS-POS < LENGTH OF SOURCE-WRK
                    GO TO GET-CMO-90
           END-IF.

     ‚* DANACH PRÜFEN WIR OB WIR EIN "-" IM STRING FINDEN. DEN KANN ES
     ‚* JA NUR NOCH IN VERBINDUNG MIT EINER BIBLIOTHEK GEBEN, DA DDS-
     ‚* AUSGESCHLOSSEN WURDE.
           INITIALIZE COPY-DDS-POS.
           INSPECT  SOURCE-WRK TALLYING COPY-DDS-POS
                    FOR CHARACTERS BEFORE "-".
           IF       COPY-DDS-POS >= LENGTH OF SOURCE-WRK
     ‚*             DANN WURDE OHNE BIBLIOTHEK ANGEGEBEN
                    MOVE X-SRCLIB TO COPY-DDS-LIB
     ‚*             DATEI KÖNNEN WIR UNS AUCH GLEICH MERKEN
                    INITIALIZE COPY-DDS-LEN
                    INSPECT  SOURCE-WRK TALLYING COPY-DDS-LEN
                             FOR CHARACTERS BEFORE " OF "
                    MOVE SOURCE-WRK(COPY-DDS-LEN + 5:) TO COPY-DDS-RCN
           ELSE
     ‚*             BIBLIOTHEKSNAMEN ERMITTELN, Z.B.:
     ‚*             COPY WRKPGMPARM OF APG-QCPYSRC
                    INITIALIZE COPY-DDS-LEN
                    INSPECT  SOURCE-WRK TALLYING COPY-DDS-LEN
                             FOR CHARACTERS BEFORE " OF "
                    IF   COPY-DDS-LEN NOT = ZEROES
                     AND COPY-DDS-POS NOT = ZEROES
                         COMPUTE POS-TMP = COPY-DDS-LEN + 5
                         COMPUTE LEN-TMP = COPY-DDS-POS + 1 - POS-TMP
                         MOVE SOURCE-WRK(POS-TMP:LEN-TMP) TO
                              COPY-DDS-LIB
     ‚*                  DATEI KÖNNEN WIR UNS AUCH GLEICH MERKEN
                         MOVE SOURCE-WRK(COPY-DDS-POS + 2:) TO
                              COPY-DDS-RCN
                    END-IF
           END-IF.
           INSPECT  COPY-DDS-RCN REPLACING ALL "." BY SPACES.

     ‚* JETZT NOCH DEN CPY-MODUL NAMEN AUSSCHNEIDEN UND WIR HABEN ALLE
     ‚* DATEN DIE WIR BRAUCHEN
           INITIALIZE COPY-DDS-POS.
           INSPECT  SOURCE-WRK TALLYING COPY-DDS-POS FOR
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
           INSPECT  SOURCE-WRK(COPY-DDS-POS:)
                    TALLYING COPY-DDS-POS FOR LEADING SPACES.
           COMPUTE  LEN-TMP = COPY-DDS-LEN - COPY-DDS-POS + 1.
           MOVE     SOURCE-WRK(COPY-DDS-POS:LEN-TMP) TO
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
     ‚* IST DIES EIN KOMMENTAR?
           IF       SOURCE-WRK(7:1) = "*"
                    GO TO GET-SNM-90
           END-IF.

     ‚* EIN SECTION-AUFRUF KANN NUR MIT EINEM "PERFORM" GESCHEHEN
     ‚* Z.B. 1. PERFORM LESE-DATEN.
     ‚*      2. PERFORM LESE-DATEN UNTIL F3 = "1".
     ‚*      3. PERFORM
     ‚*                 LESE-DATEN.
           INITIALIZE POS-TMP.
           INSPECT  SOURCE-WRK TALLYING POS-TMP FOR
                    CHARACTERS BEFORE " PERFORM ".
           IF       POS-TMP = LENGTH OF SOURCE-WRK
                    GO TO GET-SNM-90
           END-IF.

     ‚* JETZT PRÜFEN WIR OB ES EIN UNTIL GIBT ODER NICHT
           INITIALIZE LEN-TMP.
           INSPECT  SOURCE-WRK TALLYING LEN-TMP FOR
                    CHARACTERS BEFORE " UNTIL".
           IF       LEN-TMP = LENGTH OF SOURCE-WRK
                    IF   POS-TMP + 9 <= LENGTH OF SOURCE-WRK
                         MOVE SOURCE-WRK(POS-TMP + 9:) TO
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
           CALL     "CRP0009" USING X-SECTION-PTR
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
           IF       COPY-DDS-LIB NOT = X-SRCLIB
            OR      COPY-DDS-RCN NOT = X-SRCRCN
            OR      COPY-DDS-MBR NOT = X-SRCMBR
     ‚*             FÜR DIE SORTIERUNG ZEILENNUMMER DER ORIGINALZEILE
     ‚*             WIEDER ÜBERSCHREIBEN
                    MOVE SRCZEI-WRK   TO MAP-KEY OF MAPDTA-REC(31:6)
                    MOVE COPY-DDS-LIB TO MAP-KEY OF MAPDTA-REC(51:10)
                    MOVE COPY-DDS-RCN TO MAP-KEY OF MAPDTA-REC(61:10)
                    MOVE COPY-DDS-MBR TO MAP-KEY OF MAPDTA-REC(71:10)
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

           INITIALIZE QSETSRC OF QSETSRC-WRK.
           MOVE     ZEROES          TO SRCSEQ OF QSETSRC-WRK.
           MOVE     ZEROES          TO SRCDAT OF QSETSRC-WRK.
           MOVE     "##CPY"         TO SRCDTA OF QSETSRC-WRK(1:5).
           MOVE     RED-RI-HEX-WRK  TO SRCDTA OF QSETSRC-WRK(6:1).
           MOVE     "*"             TO SRCDTA OF QSETSRC-WRK(7:1).
           EVALUATE COPY-START
                    WHEN 1
                         MOVE "BOF:"   TO SRCDTA OF QSETSRC-WRK(9:4)
                    WHEN 2
                         MOVE "EOF:"   TO SRCDTA OF QSETSRC-WRK(9:4)
           END-EVALUATE.

     ‚* TEILDATEIENNAME ZUSAMMENBAUEN
           INITIALIZE STRING-WRK.
           STRING   COPY-DDS-LIB    DELIMITED BY SPACES
                    "/"             DELIMITED BY SIZE
                    COPY-DDS-RCN    DELIMITED BY SPACES
                    "/"             DELIMITED BY SIZE
                    COPY-DDS-MBR    DELIMITED BY SPACES
           INTO     STRING-WRK.
           MOVE     STRING-WRK      TO SRCDTA OF QSETSRC-WRK(14:).
           MOVE     QSETSRC-WRK     TO MAP-REC OF MAPDTA-REC(10:).
           MOVE     MAP-WRITE       TO MAP-ACT OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

       SET-CMK-90.
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
                         MOVE COPY-DDS-LIB TO
                              COPY-MOD-LIB OF COPY-REC(IND)
                         MOVE COPY-DDS-RCN TO
                              COPY-MOD-RCN OF COPY-REC(IND)
                         MOVE COPY-DDS-MBR TO
                              COPY-MOD-MBR OF COPY-REC(IND)
                         GO TO ADD-CMT-90
                    END-IF
           END-PERFORM.

       ADD-CMT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* COPY-MODUL AUS INT. TABELLE ENTFERNEN
     ‚*--------------------------------------------------------------
       RMV-COPY-MOD-TAB SECTION.
       RMV-CMT-00.

           INITIALIZE IND.
     ‚* SOLANGE LAUFEN BIS WIR DEN EINTRAG GEFUNDEN HABEN
           PERFORM  100 TIMES
                    ADD  1 TO IND
                    IF   COPY-MOD-LIB OF COPY-REC(IND) = COPY-DDS-LIB
                     AND COPY-MOD-RCN OF COPY-REC(IND) = COPY-DDS-RCN
                     AND COPY-MOD-MBR OF COPY-REC(IND) = COPY-DDS-MBR
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
     ‚* FARBWERTE ANZEIGEN
     ‚*--------------------------------------------------------------
       GET-COLOUR SECTION.
       GET-COL-00.

     ‚* ALLE WERTE ZURÜCKSETZEN
           INITIALIZE RED-RI-HEX-WRK.

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

       GET-COL-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

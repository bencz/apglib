       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE SCRAMBLEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.06.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SOURCE SCRAMBLEN                             *
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
     ‚*    DATEI QSETSRC (NUR ZUM AUSLESEN)
     ‚*-------------------------------------------------------------
           SELECT QSETSRC-DP
                  ASSIGN       TO  DATABASE-QSETSRC
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCMPSCM: SMKEY
     ‚*-------------------------------------------------------------
           SELECT PCMPSCM-DP
                  ASSIGN       TO  DATABASE-PCMPSCM
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----DATEI QCBLSRC
       FD  QSETSRC-DP
           LABEL RECORD IS STANDARD.
       01  QSETSRC-P.
           COPY DDS-ALL-FORMATS OF QSETSRC.
      /
     ‚*--- PCMPSCM: SMKEY
       FD  PCMPSCM-DP
           LABEL RECORDS ARE STANDARD.
       01  PCMPSCM-P.
           COPY DDS-ALL-FORMATS OF PCMPSCM.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0007".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.
       01  CPYF-REC                      TYPE CPYF-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.
       01  CHGPFM-REC                    TYPE CHGPFM-TYPE.
       01  OVRDBF-REC                    TYPE OVRDBF-TYPE.
       01  DELOVR-REC                    TYPE DELOVR-TYPE.

     ‚* SOURCE-STRUKTUR
       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

     ‚* SCRAMBLER-TABELLE
       01  SCM-TAB.
        05 SCM-REC                       OCCURS 9999 TIMES.
         10 ZEILE                        LIKE SRCSEQ OF QSETSRC-P.
         10 VARTYP                       PIC X(10).
         10 VARNAME                      PIC X(100).
         10 VARSCMB                      PIC X(100).
         10 VARLEN                       PIC 9(4).
         10 SCMLEN                       PIC 9(4).

     ‚* VARIABLEN FÜR "GET-NEXT-WORD-UNTIL"
       01  SEARCH-WRK                    PIC X(100).

     ‚* VARIABLEN FÜR "GET-NEXT-WORD"
       01  WORD                          PIC X(40).
       01  GNW-IDX                       PIC 9(3).
       01  POS-STR                       PIC 9(3).
       01  NEW-READ                      PIC 9(1).
       01  STR-WRK                       LIKE SRCDTA OF QSETSRC-P.
       01  SEP-WORD-ALL                  PIC 9(1).

     ‚* VARIABLEN FÜR DIE FIND-ROUTINEN
       01  VAR-FOUND                     PIC 9(1).
       01  VAR-WRK                       LIKE SEARCH-WRK.

     ‚* SCRAMBLE-VARIABLEN
       01  SCRM-WERT                     PIC 9(3).
       01  SCRM-WRK                      PIC X(20).
       01  SCRM-UC                       LIKE SCRM-WRK.
       01  WERT-WRK                      PIC 9(2).
       01  SCMSRC-WRK                    LIKE SRCDTA OF QSETSRC-P.

     ‚* LÄNGENVARIABLEN
       01  POS                           PIC 9(4).
       01  W-POS                         PIC 9(4).
       01  FIRST-POS                     PIC 9(4).
       01  LEN                           PIC 9(4).
       01  CNT                           PIC 9(4).
       01  ANZAHL-WRK                    PIC 9(4).

     ‚* SCHALTER-VARIABLEN
       01  START-ERFOLGT                 PIC 9(1).
       01  FIRST-TIME                    PIC 9(1).
       01  LAST-PUNKT                    PIC 9(1).

     ‚* AKTIONSVARIABLEN
       01  FILL-SPACES                   PIC 9(1).

     ‚* SONSTIGE VARIABLEN
       01  SRCLIB-WRK                    PIC X(10).
       01  SRCRCN-WRK                    PIC X(10).
       01  SRCMBR-WRK                    PIC X(10).
       01  SRCSEQ-WRK                    LIKE SRCSEQ OF QSETSRC-P.
       01  VAL-WRK                       LIKE SEARCH-WRK.
       01  LFDN-MAP                      LIKE MAP-LFDN OF MAPDTA-REC.
       01  SOURCE-WRK                    LIKE SRCDTA OF QSETSRC-P.
       01  MAX-TAB                       PIC 9(4) VALUE 9999.
       01  SCM-VAL                       PIC X(1000).

     ‚* SOURCE-POINTER
       01  OLD-SOURCE-PTR                USAGE POINTER.
       01  NEW-SOURCE-PTR                USAGE POINTER.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-AKTION                      PIC X(20).
       01  X-SCMKEY                      LIKE SMKEY  OF PCMPSCM-P.
       01  X-INP-PTR                     USAGE POINTER.
       01  X-RET-PTR                     USAGE POINTER.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-AKTION
                                X-SCMKEY
                                X-INP-PTR
                                X-RET-PTR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0007" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT PCMPSCM-DP.

     ‚* SOURCE KOMPLETT EINLESEN, COPYMODULE SIND DANN SCHON AUFGELÖST
           SET      OLD-SOURCE-PTR TO NULL.
           CALL     "CRP0028" USING X-SRCLIB
                                    X-SRCRCN
                                    X-SRCMBR
                                    X-INP-PTR
                                    OLD-SOURCE-PTR
           END-CALL.

           INITIALIZE SCM-VAL.
     ‚* DATEI LESEN
           INITIALIZE PCMPSCMF OF PCMPSCM-P.
           MOVE     X-SCMKEY        TO SMKEY  OF PCMPSCM-P.
           READ     PCMPSCM-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE SMVAL  OF PCMPSCM-P TO SCM-VAL
           END-IF.

     ‚* IM ERSTEN SCHRITT WERDEN FÜR ALLE VARIABLEN UND SECTIONS
     ‚* NEUE VARIABLENNAMEN VERGEBEN. WEITERHIN WIRD JEDE KONSTANTE
     ‚* WERT DURCH EINE VARIABLE ERSETZT.
     ‚* SPACES UND ZEROES WERDEN EBENFALLS DURCH NEUE VARIABLEN ERSETZT
           PERFORM  FIND-ALL-VARS.

     ‚* IM NÄCHSTEN SCHRITT WIRD EIN POINTER AUFGEBAUT, DER DIE NEUE
     ‚* SOURCE BEINHALTET. DABEI WIRD WORT FÜR WORT ERSETZT.
           PERFORM  CREATE-SCRAMBLE-PTR.

     ‚* ZUM SCHLUSS MUSS DIE NEUE SOURCE ERSTELLT WERDEN. DAZU WIRD
     ‚* DIE ORIGINALSOURCE KOPIERT UND NEU BESCHRIEBEN
           IF       X-AKTION = "SCRAMBLE"
                    PERFORM  CREATE-SOURCE
     ‚*             DIE TEILDATEIENBEZEICHNUNG SCRAMBLEN WIR AUCH
                    PERFORM  CHANGE-TEILDATEIENTEXT
           END-IF.

     ‚* SOLL DIE SOURCE DANACH WIEDER ZURÜCKGESPIELT WERDEN?
           IF       X-AKTION = "ONLYCOPY"
            OR      X-AKTION = "SCRAMBLE&COPY"
                    CALL "CRP0016" USING NEW-SOURCE-PTR
                                         X-RET-PTR
                    END-CALL
           END-IF.

       ENDE.
           CLOSE                 PCMPSCM-DP.

           IF       OLD-SOURCE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO OLD-SOURCE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       NEW-SOURCE-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO NEW-SOURCE-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      OLD-SOURCE-PTR TO NULL.
           SET      NEW-SOURCE-PTR TO NULL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN NACH VARIABLEN
     ‚*--------------------------------------------------------------
       FIND-ALL-VARS SECTION.
       FND-VAR-00.

           INITIALIZE RET-CODE.
           INITIALIZE FIRST-TIME.
     ‚* LAUFENDE NUMMER INITIALISIEREN
           MOVE     ZEROES  TO LFDN-MAP.

       FND-VAR-20.
           SET      MAP-PTR OF MAPDTA-REC TO OLD-SOURCE-PTR.
           MOVE     LFDN-MAP      TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO FND-VAR-90
           END-IF.

     ‚* ZEILE HOCHZÄHLEN
           ADD      1 TO LFDN-MAP.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(10:)  TO SRC-WRK.
           MOVE     SRCDTA OF SRC-WRK           TO SOURCE-WRK.

     ‚* KOMMENTARE ÜBERLESEN
           IF       SOURCE-WRK(7:1)     = "*"
            AND     SOURCE-WRK(7:3) NOT = "*DB"
                    GO TO FND-VAR-20
           END-IF.

     ‚* PRÜFEN OB IN DER ZEILE EINE "SELECT"-ANWEISUNG FÜR DATEIEN
     ‚* VORHANDEN IST.
           PERFORM  FIND-SELECT.
           IF       VAR-FOUND NOT = ZEROES
                    PERFORM ADD-VARIABLE
           END-IF.

     ‚* PRÜFEN OB IN DER EINE STUFENANGABE IST
           PERFORM  FIND-STUFENVARIABLE.
           IF       VAR-FOUND NOT = ZEROES
                    PERFORM ADD-VARIABLE
           END-IF.

     ‚* PRÜFEN OB HIER EIN SECTIONNAME VORHANDEN IST
           PERFORM  FIND-SECTIONNAME.
           IF       VAR-FOUND NOT = ZEROES
                    PERFORM ADD-SECTION
           END-IF.

     ‚* PRÜFEN OB HIER EIN SPRUNGMARKE VORHANDEN IST
           PERFORM  FIND-SPRUNGMARKE.
           IF       VAR-FOUND NOT = ZEROES
                    PERFORM ADD-SECTION
           END-IF.

     ‚* PRÜFEN OB HIER EIN KONSTANTER WERT VORHANDEN IST
           PERFORM  FIND-CONSTANT.
           IF       VAR-FOUND NOT = ZEROES
                    PERFORM ADD-CONSTANT
           END-IF.

           GO TO    FND-VAR-20.

       FND-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SELECT-ANWEISUNG FINDEN
     ‚*--------------------------------------------------------------
       FIND-SELECT SECTION.
       FND-SEL-00.

           INITIALIZE VAR-FOUND.
     ‚* EINE SELECT-ANWEISUNG IST Z.B. SO AUFGEBAUT:
     ‚*   šSELECT PPGMLIB-DP‚
     ‚*           ASSIGN       TO  DATABASE-PPGMLIB
     ‚*           ORGANIZATION IS  INDEXED
     ‚*           RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
     ‚*           ACCESS       IS  DYNAMIC
     ‚*           FILE STATUS  IS  FILE-STATUS.

           MOVE     "SELECT" TO SEARCH-WRK.
           PERFORM  GET-NEXT-WORD-UNTIL.
           IF       WORD NOT = SEARCH-WRK
                    GO TO FND-SEL-90
           END-IF.

     ‚* DAS NÄCHSTE WORT SOLLTE DIE VARIABLE SEIN
           PERFORM  GET-NEXT-WORD.
           IF       WORD NOT = SPACES
                    INSPECT WORD REPLACING ALL "." BY SPACES
                    MOVE 1    TO VAR-FOUND
                    MOVE WORD TO VAR-WRK
           END-IF.

       FND-SEL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN NACH GEWÖHNLICHEN VARIABLEN
     ‚*--------------------------------------------------------------
       FIND-STUFENVARIABLE SECTION.
       FND-VAR-00.

           INITIALIZE VAR-FOUND.
           MOVE     SOURCE-WRK TO STR-WRK.
           MOVE     1          TO NEW-READ.
     ‚* EINE VARIABLE IST IMMER IN DIESER FORM:
     ‚*   š 01‚  OVR-LAENGE ‚PIC 9(10)V9(5) COMP-3.
           PERFORM  GET-NEXT-WORD.
           IF       WORD(1:2) IS NOT NUMERIC
                    GO TO FND-VAR-90
           END-IF.

     ‚* DAS NÄCHSTE WORT SOLLTE DIE VARIABLE SEIN
           PERFORM  GET-NEXT-WORD.
           IF       WORD NOT = SPACES
                    INSPECT WORD REPLACING ALL "." BY SPACES
                    MOVE 1    TO VAR-FOUND
                    MOVE WORD TO VAR-WRK
           END-IF.

       FND-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* <bitte Kommentar eingeben>
     ‚*--------------------------------------------------------------
       FIND-SECTIONNAME SECTION.
       FND-SEC-00.

           INITIALIZE RET-CODE.

       FND-SEC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* UMSETZEN VON SPRUNGMARKEN, JEDOCH CHKSRC-KOMPATIBEL
     ‚*--------------------------------------------------------------
       FIND-SPRUNGMARKE SECTION.
       FND-SPR-00.

           INITIALIZE RET-CODE.

       FND-SPR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* <bitte Kommentar eingeben>
     ‚*--------------------------------------------------------------
       FIND-CONSTANT SECTION.
       FND-CON-00.

           INITIALIZE RET-CODE.

       FND-CON-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER SOURCE
     ‚*--------------------------------------------------------------
       CREATE-SCRAMBLE-PTR SECTION.
       CRT-SPT-00.

           INITIALIZE RET-CODE.
     ‚* LAUFENDE NUMMER INITIALISIEREN
           MOVE     ZEROES  TO LFDN-MAP.
           MOVE     ZEROES  TO SRCSEQ-WRK.

       CRT-SPT-20.
           SET      MAP-PTR OF MAPDTA-REC TO OLD-SOURCE-PTR.
           MOVE     LFDN-MAP      TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO CRT-SPT-90
           END-IF.

     ‚* ZEILE HOCHZÄHLEN
           ADD      1 TO LFDN-MAP.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(10:)  TO SRC-WRK.
           MOVE     SRCDTA OF SRC-WRK           TO SOURCE-WRK.

     ‚* IST DAS EINE LEERZEILE?
           IF       (    SOURCE-WRK(7:1)     = "*"
                     AND SOURCE-WRK(7:3) NOT = "*DB")
            OR      SOURCE-WRK(8:)  = SPACES
                    GO TO CRT-SPT-20
           END-IF.

     ‚* JETZT WORT FÜR WORT DIE SOURCE ÄNDERN UND DABEI PRÜFEN
     ‚* OB WIR BESTIMMTE WÖRTER NICHT ERSETZEN KÖNNEN
           PERFORM  CHANGE-SOURCE.

     ‚* JETZT SIND DIE WÖRTER ERSETZT, JETZT ENTFERNEN WIR ALLE
     ‚* ÜBERFLÜSSIGE LEERZEICHEN
           PERFORM  TRUNCATE-SPACES.

     ‚* NEUE SOURCEDATEN ÜBERNEHMEN
           ADD      0,01            TO SRCSEQ-WRK.
           MOVE     SPACES          TO SRCDTA OF SRC-WRK.
           MOVE     SCMSRC-WRK      TO SRCDTA OF SRC-WRK.
           MOVE     ZEROES          TO SRCDAT OF SRC-WRK.
           MOVE     SRCSEQ-WRK      TO SRCSEQ OF SRC-WRK.

     ‚* NEUEN POINTER FÜLLEN
           SET      MAP-PTR OF MAPDTA-REC       TO NEW-SOURCE-PTR.

     ‚* NEUEN KEY AUFBAUEN
           MOVE     SPACES             TO MAP-KEY OF MAPDTA-REC.
           MOVE     X-SRCLIB           TO MAP-KEY OF MAPDTA-REC(1:10).
           MOVE     X-SRCRCN           TO MAP-KEY OF MAPDTA-REC(11:10).
           MOVE     X-SRCMBR           TO MAP-KEY OF MAPDTA-REC(21:10).
           MOVE     SRCSEQ-WRK          TO MAP-KEY OF MAPDTA-REC(31:6).
           MOVE     X'FFFFFFFFFFFF'     TO MAP-KEY OF MAPDTA-REC(38:6).

     ‚* NEUE SOURCE AUFBAUEN
           MOVE     SPACES        TO MAP-REC  OF MAPDTA-REC.
           MOVE     SRC-WRK       TO MAP-REC  OF MAPDTA-REC(10:).

           MOVE     MAP-WRITE     TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           SET      NEW-SOURCE-PTR TO MAP-PTR OF MAPDTA-REC.

           GO TO    CRT-SPT-20.

       CRT-SPT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN DER SOURCE IN DER QTEMP
     ‚*--------------------------------------------------------------
       CREATE-SOURCE SECTION.
       CRT-SRC-00.

     ‚* CPYF ZUSAMMENBAUEN
           INITIALIZE CPYF-REC.
           MOVE     X-SRCLIB               TO FROMLIB  OF CPYF-REC.
           MOVE     X-SRCRCN               TO FROMFILE OF CPYF-REC.
           MOVE     X-SRCMBR               TO FROMMBR  OF CPYF-REC.
           MOVE     "QTEMP"                TO TOLIB    OF CPYF-REC.
           MOVE     X-SRCRCN               TO TOFILE   OF CPYF-REC.
           MOVE     X-SRCMBR               TO TOMBR    OF CPYF-REC.
           MOVE     "*REPLACE"             TO SRCOPT   OF CPYF-REC.
           MOVE     "*NONE"                TO FMTOPT   OF CPYF-REC.
           MOVE     "*YES"                 TO CRTFIL   OF CPYF-REC.
           CALL     "CLP0053" USING CPYF-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE = SPACES
                    MOVE "QTEMP" TO X-SRCLIB
           END-IF.

     ‚* SETZEN OVRWRT FÜR QCBLSRC UND ÖFFNEN DER DATEI
           MOVE     "QTEMP"  TO SRCLIB-WRK.
           MOVE     X-SRCRCN TO SRCRCN-WRK.
           MOVE     X-SRCMBR TO SRCMBR-WRK.
           PERFORM  SET-OVR-MBR.
           IF       RET-CODE NOT = SPACES
                    GO TO CRT-SRC-90
           END-IF.

     ‚* ÖFFNEN DER TEILDATEI
           OPEN     OUTPUT QSETSRC-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CRT-SRC-80
           END-IF.

     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     ZEROES TO START-ERFOLGT.

       CRT-SRC-20.
           IF       START-ERFOLGT = ZEROES
                    MOVE   1                  TO START-ERFOLGT
                    MOVE   ZEROES             TO MAP-LFDN OF MAPDTA-REC
                    MOVE   MAP-READ-LFDN      TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO NEW-SOURCE-PTR
                    PERFORM  COPY-CALL-MAPDTA
           ELSE
                    MOVE   MAP-READ-NEXT      TO MAP-ACT  OF MAPDTA-REC
                    SET    MAP-PTR OF MAPDTA-REC TO NEW-SOURCE-PTR
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           IF       RET-CODE NOT = SPACES
                    GO TO CRT-SRC-80
           END-IF.

     ‚* JETZT BRAUCHEN WIR NUR NOCH NEU SCHREIBEN
           MOVE     MAP-REC OF MAPDTA-REC(10:) TO QSETSRC-P.
           WRITE    QSETSRC-P.

           GO TO    CRT-SRC-20.

       CRT-SRC-80.
           CLOSE    QSETSRC-DP.

           PERFORM  DLT-OVR-MBR.

       CRT-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEILE ÄNDERN
     ‚*--------------------------------------------------------------
       CHANGE-SOURCE SECTION.
       CHG-SRC-00.

           INITIALIZE SCMSRC-WRK.
           MOVE     SOURCE-WRK TO STR-WRK.
           MOVE     1 TO NEW-READ.

       CHG-SRC-20.
           MOVE     1 TO SEP-WORD-ALL.
           PERFORM  GET-NEXT-WORD.
           IF       WORD = SPACES
                    GO TO CHG-SRC-90
           END-IF.

     ‚* WORT ÜBERNEHMEN
     ‚*
     ‚* NICHT UMSETZBARE VARIABLEN
           IF       WORD NOT = "PGM-WRK"
            AND     WORD NOT = "PGM-TYP"
     ‚*     NICHT UMSETZBARE SECTIONS
            AND     WORD NOT = "LINKAGE"
            AND     WORD NOT = "PROCEDURE"
            AND     WORD NOT = "INPUT-OUTPUT"
            AND     WORD NOT = "FILE"
            AND     WORD NOT = "WORKING-STORAGE"
            AND     WORD NOT = "LOCAL-STORAGE"
     ‚*             INDIKATOREN FÜR EIN MOVE CORR SOLLTE MAN
     ‚*             ERST DANN UMSETZEN, WENN DAS AUCH IM FORMAT
     ‚*             GEÄNDERT WIRD. DIES IST ABER BISHER NICHT
     ‚*             DER FALL
            AND     WORD NOT = "IN01"
            AND     WORD NOT = "IN02"
            AND     WORD NOT = "IN03"
            AND     WORD NOT = "IN04"
            AND     WORD NOT = "IN05"
            AND     WORD NOT = "IN06"
            AND     WORD NOT = "IN07"
            AND     WORD NOT = "IN08"
            AND     WORD NOT = "IN09"
            AND     WORD NOT = "IN10"
            AND     WORD NOT = "IN11"
            AND     WORD NOT = "IN12"
            AND     WORD NOT = "IN13"
            AND     WORD NOT = "IN14"
            AND     WORD NOT = "IN15"
            AND     WORD NOT = "IN16"
            AND     WORD NOT = "IN17"
            AND     WORD NOT = "IN18"
            AND     WORD NOT = "IN19"
            AND     WORD NOT = "IN20"
            AND     WORD NOT = "IN21"
            AND     WORD NOT = "IN22"
            AND     WORD NOT = "IN23"
            AND     WORD NOT = "IN24"
            AND     WORD NOT = "IN25"
            AND     WORD NOT = "IN26"
            AND     WORD NOT = "IN27"
            AND     WORD NOT = "IN28"
            AND     WORD NOT = "IN29"
            AND     WORD NOT = "IN30"
            AND     WORD NOT = "IN31"
            AND     WORD NOT = "IN32"
            AND     WORD NOT = "IN33"
            AND     WORD NOT = "IN34"
            AND     WORD NOT = "IN35"
            AND     WORD NOT = "IN36"
            AND     WORD NOT = "IN37"
            AND     WORD NOT = "IN38"
            AND     WORD NOT = "IN39"
            AND     WORD NOT = "IN40"
            AND     WORD NOT = "IN41"
            AND     WORD NOT = "IN42"
            AND     WORD NOT = "IN43"
            AND     WORD NOT = "IN44"
            AND     WORD NOT = "IN45"
            AND     WORD NOT = "IN46"
            AND     WORD NOT = "IN47"
            AND     WORD NOT = "IN48"
            AND     WORD NOT = "IN49"
            AND     WORD NOT = "IN50"
            AND     WORD NOT = "IN51"
            AND     WORD NOT = "IN52"
            AND     WORD NOT = "IN53"
            AND     WORD NOT = "IN54"
            AND     WORD NOT = "IN55"
            AND     WORD NOT = "IN56"
            AND     WORD NOT = "IN57"
            AND     WORD NOT = "IN58"
            AND     WORD NOT = "IN59"
            AND     WORD NOT = "IN60"
            AND     WORD NOT = "IN61"
            AND     WORD NOT = "IN62"
            AND     WORD NOT = "IN63"
            AND     WORD NOT = "IN64"
            AND     WORD NOT = "IN65"
            AND     WORD NOT = "IN66"
            AND     WORD NOT = "IN67"
            AND     WORD NOT = "IN68"
            AND     WORD NOT = "IN69"
            AND     WORD NOT = "IN70"
            AND     WORD NOT = "IN71"
            AND     WORD NOT = "IN72"
            AND     WORD NOT = "IN73"
            AND     WORD NOT = "IN74"
            AND     WORD NOT = "IN75"
            AND     WORD NOT = "IN76"
            AND     WORD NOT = "IN77"
            AND     WORD NOT = "IN78"
            AND     WORD NOT = "IN79"
            AND     WORD NOT = "IN80"
            AND     WORD NOT = "IN81"
            AND     WORD NOT = "IN82"
            AND     WORD NOT = "IN83"
            AND     WORD NOT = "IN84"
            AND     WORD NOT = "IN85"
            AND     WORD NOT = "IN86"
            AND     WORD NOT = "IN87"
            AND     WORD NOT = "IN88"
            AND     WORD NOT = "IN89"
            AND     WORD NOT = "IN90"
            AND     WORD NOT = "IN91"
            AND     WORD NOT = "IN92"
            AND     WORD NOT = "IN93"
            AND     WORD NOT = "IN94"
            AND     WORD NOT = "IN95"
            AND     WORD NOT = "IN96"
            AND     WORD NOT = "IN97"
            AND     WORD NOT = "IN98"
            AND     WORD NOT = "IN99"
            AND     WORD NOT = "PIC"
                    PERFORM CHANGE-WORD
           END-IF.

           MOVE     WORD TO SCMSRC-WRK(W-POS:).

           GO TO    CHG-SRC-20.

       CHG-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WORT IN SCRAMBLER-WORT ÄNDERN
     ‚*--------------------------------------------------------------
       CHANGE-WORD SECTION.
       CHG-WRD-00.

           INITIALIZE CNT.
     ‚* JETZT DAS NEUE WORT SUCHEN
           PERFORM  MAX-TAB TIMES
                    ADD 1 TO CNT
                    IF   WORD = VARNAME OF SCM-TAB(CNT)
                         MOVE VARSCMB OF SCM-TAB(CNT) TO WORD
                         GO TO CHG-WRD-90
                    END-IF
           END-PERFORM.

       CHG-WRD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LEERZEICHEN IN DER SOURCE ENTFERNEN
     ‚*--------------------------------------------------------------
       TRUNCATE-SPACES SECTION.
       TRU-SPC-00.

           INITIALIZE RET-CODE.

       TRU-SPC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VARIABLE UMBENENNEN
     ‚*--------------------------------------------------------------
       SCRAMBLE-VAR SECTION.
       SCR-VAR-00.

           INITIALIZE SCRM-WRK.
           MOVE     ZEROES TO POS.
           MOVE     LENGTH OF SCRM-WRK TO ANZAHL-WRK.

     ‚* WERT ÄNDERN
           PERFORM  ANZAHL-WRK TIMES
                    ADD 1 TO POS
                    IF   VAR-WRK(POS:1) NOT = SPACES
                         MOVE    VAR-WRK(POS:1) TO VAL-WRK
                         PERFORM SCRAMBLE-VAL
                         MOVE    VAL-WRK        TO SCRM-WRK(POS:1)
                    END-IF
           END-PERFORM.

     ‚* HIER EINE BLACKLIST AN WÖRTERN DEI NICHT ERSTELLT WERDEN DÜRFEN
           MOVE     FUNCTION UPPER-CASE(SCRM-WRK) TO SCRM-UC.
           IF       SCRM-UC = "ID"
                    ADD 1 TO SRCDAT OF SRC-WRK
                    GO TO SCR-VAR-00
           END-IF.

       SCR-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINEN BUCHSTABEN ÄNDERN
     ‚*--------------------------------------------------------------
       SCRAMBLE-VAL SECTION.
       SCR-VAL-00.

     ‚* JEDEM ZEICHEN EIN WERT ZUWEISEN
           EVALUATE VAL-WRK
                    WHEN "A" MOVE 1  TO WERT-WRK
                    WHEN "B" MOVE 2  TO WERT-WRK
                    WHEN "C" MOVE 3  TO WERT-WRK
                    WHEN "D" MOVE 4  TO WERT-WRK
                    WHEN "E" MOVE 5  TO WERT-WRK
                    WHEN "F" MOVE 6  TO WERT-WRK
                    WHEN "G" MOVE 7  TO WERT-WRK
                    WHEN "H" MOVE 8  TO WERT-WRK
                    WHEN "I" MOVE 9  TO WERT-WRK
                    WHEN "J" MOVE 10 TO WERT-WRK
                    WHEN "K" MOVE 11 TO WERT-WRK
                    WHEN "L" MOVE 12 TO WERT-WRK
                    WHEN "M" MOVE 13 TO WERT-WRK
                    WHEN "N" MOVE 14 TO WERT-WRK
                    WHEN "O" MOVE 15 TO WERT-WRK
                    WHEN "P" MOVE 16 TO WERT-WRK
                    WHEN "Q" MOVE 17 TO WERT-WRK
                    WHEN "R" MOVE 18 TO WERT-WRK
                    WHEN "S" MOVE 19 TO WERT-WRK
                    WHEN "T" MOVE 20 TO WERT-WRK
                    WHEN "U" MOVE 21 TO WERT-WRK
                    WHEN "V" MOVE 22 TO WERT-WRK
                    WHEN "W" MOVE 23 TO WERT-WRK
                    WHEN "X" MOVE 24 TO WERT-WRK
                    WHEN "Y" MOVE 25 TO WERT-WRK
                    WHEN "Z" MOVE 26 TO WERT-WRK
                    WHEN "a" MOVE 27 TO WERT-WRK
                    WHEN "b" MOVE 28 TO WERT-WRK
                    WHEN "c" MOVE 29 TO WERT-WRK
                    WHEN "d" MOVE 30 TO WERT-WRK
                    WHEN "e" MOVE 31 TO WERT-WRK
                    WHEN "f" MOVE 32 TO WERT-WRK
                    WHEN "g" MOVE 33 TO WERT-WRK
                    WHEN "h" MOVE 34 TO WERT-WRK
                    WHEN "i" MOVE 35 TO WERT-WRK
                    WHEN "j" MOVE 36 TO WERT-WRK
                    WHEN "k" MOVE 37 TO WERT-WRK
                    WHEN "l" MOVE 38 TO WERT-WRK
                    WHEN "m" MOVE 39 TO WERT-WRK
                    WHEN "n" MOVE 40 TO WERT-WRK
                    WHEN "o" MOVE 41 TO WERT-WRK
                    WHEN "p" MOVE 42 TO WERT-WRK
                    WHEN "q" MOVE 43 TO WERT-WRK
                    WHEN "r" MOVE 44 TO WERT-WRK
                    WHEN "s" MOVE 45 TO WERT-WRK
                    WHEN "t" MOVE 46 TO WERT-WRK
                    WHEN "u" MOVE 47 TO WERT-WRK
                    WHEN "v" MOVE 48 TO WERT-WRK
                    WHEN "w" MOVE 49 TO WERT-WRK
                    WHEN "x" MOVE 50 TO WERT-WRK
                    WHEN "y" MOVE 51 TO WERT-WRK
                    WHEN "z" MOVE 52 TO WERT-WRK
                    WHEN "0" MOVE 53 TO WERT-WRK
                    WHEN "1" MOVE 54 TO WERT-WRK
                    WHEN "2" MOVE 55 TO WERT-WRK
                    WHEN "3" MOVE 56 TO WERT-WRK
                    WHEN "4" MOVE 57 TO WERT-WRK
                    WHEN "5" MOVE 58 TO WERT-WRK
                    WHEN "6" MOVE 59 TO WERT-WRK
                    WHEN "7" MOVE 60 TO WERT-WRK
                    WHEN "8" MOVE 61 TO WERT-WRK
                    WHEN "9" MOVE 62 TO WERT-WRK
                    WHEN OTHER MOVE 63 TO WERT-WRK
           END-EVALUATE.

     ‚* WERT ERMITTELN (2-STELLIG)
           COMPUTE  SCRM-WERT = (  (SRCDAT OF SRC-WRK + 13)
                                 / SRCSEQ OF SRC-WRK)
                                + (WERT-WRK * POS) - 13
           END-COMPUTE.

     ‚* WENN KEIN WERT ERMITTELT WURDE, DANN IST 42 BEKANNTLICH DIE
     ‚* ANTWORT AUF DAS LEBEN, DAS UNIVERSUM UND DEN GANZEN REST. AUCH
     ‚* FÜR DEN SCRAMBLER...
           IF       SCRM-WERT = ZEROES
                    MOVE 42 TO SCRM-WERT
           END-IF.

     ‚* WERT ERMITTELN
           MOVE     SCM-VAL(SCRM-WERT:1) TO VAL-WRK.

       SCR-VAL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VARIABLE DER TABELLE HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-VARIABLE SECTION.
       ADD-VAR-00.

     ‚* VARIABLE ÄNDERN
           PERFORM  SCRAMBLE-VAR.

     ‚* BEIM ERSTEN MAL TABELLE INITIALISIEREN
           IF       FIRST-TIME = ZEROES
                    MOVE 1      TO FIRST-TIME
                    MOVE ZEROES TO CNT
                    PERFORM MAX-TAB TIMES
                          ADD  1      TO CNT
                          MOVE ZEROES TO ZEILE OF SCM-TAB(CNT)
                    END-PERFORM
           END-IF.

           INITIALIZE CNT.
     ‚* DIE INTERNE TABELLE DURCHSUCHEN
           PERFORM  MAX-TAB TIMES
                    ADD 1 TO CNT
                    IF   ZEILE OF SCM-TAB(CNT) = ZEROES
                         MOVE SRCSEQ OF SRC-WRK TO
                              ZEILE OF SCM-TAB(CNT)
                         MOVE "VAR" TO VARTYP OF SCM-TAB(CNT)
                         MOVE VAR-WRK  TO VARNAME OF SCM-TAB(CNT)
                         MOVE SCRM-WRK TO VARSCMB OF SCM-TAB(CNT)
                         GO TO ADD-VAR-90
                    END-IF
           END-PERFORM.

       ADD-VAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VARIABLE DER TABELLE HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-SECTION SECTION.
       ADD-SEC-00.

       ADD-SEC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VARIABLE DER TABELLE HINZUFÜGEN
     ‚*--------------------------------------------------------------
       ADD-CONSTANT SECTION.
       ADD-CON-00.

       ADD-CON-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EIN BESTIMMTES WORT SUCHEN
     ‚*--------------------------------------------------------------
       GET-NEXT-WORD-UNTIL SECTION.
       GET-NXW-00.

     ‚* BEIM STRINGANFANG LOSLEGEN
           MOVE     1          TO NEW-READ.
           MOVE     SOURCE-WRK TO STR-WRK.

     ‚* SOLANGE WÖRTER LESEN BIS DAS WORT GEFUNDEN WURDE
       GET-NXW-10.
           PERFORM  GET-NEXT-WORD.
           IF       WORD = SPACES
            OR      WORD = SEARCH-WRK
                    GO TO GET-NXW-90
           END-IF.

           GO TO    GET-NXW-10.

       GET-NXW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NÄCHSTES WORD AUS STR-WRK ERMITTELN
     ‚*--------------------------------------------------------------
       GET-NEXT-WORD SECTION.
       GET-NWD-00.

           INITIALIZE WORD.
           INITIALIZE W-POS.
     ‚* STRING VON BEGIN AN LESEN
           IF       NEW-READ = 1
                    INITIALIZE NEW-READ
                    MOVE 1 TO  POS-STR
           END-IF.

     ‚* FÜHRENDE SPACES ÜBERLESEN
           PERFORM  WITH TEST BEFORE VARYING POS-STR FROM POS-STR BY 1
                    UNTIL POS-STR > LENGTH OF STR-WRK OR
                          STR-WRK(POS-STR:1) NOT = SPACES
                    CONTINUE
           END-PERFORM.

     ‚* STARTPOSITION MERKEN
           MOVE     POS-STR TO W-POS.

     ‚* ALLE ZEICHEN BIS ZUM NÄCHSTEN SPACE ÜBERNEHMEN
           INITIALIZE GNW-IDX.
           PERFORM  WITH TEST BEFORE VARYING POS-STR FROM POS-STR BY 1
                    UNTIL POS-STR > LENGTH OF STR-WRK OR
                          STR-WRK(POS-STR:1) = SPACES
                    IF   SEP-WORD-ALL NOT = ZEROES
     ‚*                  EINFACH NUR DAS WORT ÜBERNEHMEN
                         ADD 1 TO GNW-IDX
                         MOVE STR-WRK(POS-STR:1) TO WORD(GNW-IDX:1)
     ‚*                  IST DAS WORT EIN SONDERZEICHEN?
                         IF   WORD(1:1) = "("
                          OR  WORD(1:1) = ")"
                          OR  WORD(1:1) = ":"
                          OR  WORD(1:1) = "."
                          OR  WORD(1:1) = ","
                              ADD 1 TO POS-STR
                              GO TO GET-NWD-90
                         END-IF
     ‚*                  IST DAS NÄCHSTE ZEICHEN EIN SONDERZEICHEN?
                         IF   STR-WRK(POS-STR + 1:1) = "("
                          OR  STR-WRK(POS-STR + 1:1) = ")"
                          OR  STR-WRK(POS-STR + 1:1) = ":"
                          OR  STR-WRK(POS-STR + 1:1) = "."
                          OR  STR-WRK(POS-STR + 1:1) = ","
                              ADD 1 TO POS-STR
                              GO TO GET-NWD-90
                         END-IF
                    ELSE
     ‚*                  EINFACH NUR DAS WORT ÜBERNEHMEN
                         ADD 1 TO GNW-IDX
                         MOVE STR-WRK(POS-STR:1) TO WORD(GNW-IDX:1)
                    END-IF
           END-PERFORM.

       GET-NWD-90.
     ‚* WERTE ZURÜCKSETZEN
           MOVE     ZEROES TO SEP-WORD-ALL.

           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* TEILDATEIENBEZEICHNUNG ÜBERNEHMEN
     ‚*--------------------------------------------------------------
       CHANGE-TEILDATEIENTEXT SECTION.
       CHG-TDT-00.

           INITIALIZE RET-CODE.
     ‚* SOURCEINFORMATIONEN ERMITTELN
           INITIALIZE RTVMBRD-REC.
           MOVE     SRCLIB-WRK   TO OBJLIB   OF RTVMBRD-REC.
           MOVE     SRCRCN-WRK   TO FILENAME OF RTVMBRD-REC.
           MOVE     SRCMBR-WRK   TO MBRNAME  OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    GO TO CHG-TDT-90
           END-IF.

     ‚* JETZT JEDES WORD ÄNDERN
           INITIALIZE SCMSRC-WRK.
           MOVE     MBRTEXT OF RTVMBRD-REC TO STR-WRK.
           MOVE     1 TO NEW-READ.

       CHG-TDT-20.
           PERFORM  GET-NEXT-WORD.
           IF       WORD = SPACES
                    GO TO CHG-TDT-80
           END-IF.

     ‚* WORT SCRAMBLEN
           MOVE     WORD TO VAR-WRK.
           PERFORM  SCRAMBLE-VAR.
           MOVE     SCRM-WRK TO SCMSRC-WRK(W-POS:).

           GO TO    CHG-TDT-20.

       CHG-TDT-80.
     ‚* JETZT DEN TEXT ÄNDERN
           INITIALIZE CHGPFM-REC.
           MOVE     SRCLIB-WRK        TO LIBNAME  OF CHGPFM-REC.
           MOVE     SRCRCN-WRK        TO FILENAME OF CHGPFM-REC.
           MOVE     SRCMBR-WRK        TO MBRNAME  OF CHGPFM-REC.
           MOVE     SCMSRC-WRK        TO SRCTXT   OF CHGPFM-REC.
           CALL     "CLP0049" USING CHGPFM-REC
                                    RET-CODE
           END-CALL.

       CHG-TDT-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

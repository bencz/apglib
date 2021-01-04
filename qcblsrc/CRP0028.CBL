       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0028.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SOURCE + COPY-MODULE EINLESEN                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.06.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  COPY-MODULE AUFLÖSEN UND SOURCE EINLESEN     *
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

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0028".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

       01  SRC-WRK.
           COPY DDS-ALL-FORMATS OF QSETSRC.

       01  SOURCE-WRK                    LIKE SRCDTA OF SRC-WRK.
       01  LFDN-MAP                      LIKE MAP-LFDN OF MAPDTA-REC.

     ‚* LÄNGEN-VARIABLEN
       01  LEN-TMP                       PIC 9(4).
       01  POS-TMP                       PIC 9(4).

     ‚* VARIABLEN FÜR DIE ERMITTLUNG DES COPY-MODULS
       01  COPY-DDS-LIB                  PIC X(10).
       01  COPY-DDS-RCN                  PIC X(10).
       01  COPY-DDS-MBR                  PIC X(10).
       01  COPY-DDS-LEN                  PIC 9(4).
       01  COPY-DDS-POS                  PIC 9(4).

     ‚* VARIABLEN FÜR TEILDATEIPRÜFUNG
       01  CHECK-LIB                     PIC X(10).
       01  CHECK-RCN                     PIC X(10).
       01  CHECK-MBR                     PIC X(10).

     ‚* PARAMETER FÜR DAS CRP0025
       01  SRCLIB-WRK                    PIC X(10).
       01  SRCRCN-WRK                    PIC X(10).
       01  SRCMBR-WRK                    PIC X(10).
       01  SECTION-PTR                   USAGE POINTER.
       01  ANZREC-SAV                    PIC 9(9).
       01  FIRST-SEQNBR                  LIKE SRCSEQ OF SRC-WRK.
       01  LAST-SEQNBR                   LIKE SRCSEQ OF SRC-WRK.
       01  SRCZEI-WRK                    LIKE SRCSEQ OF SRC-WRK.

     ‚* PARAMETER FÜR DAS CRP0011
       01  NOCHECK-WRK                   PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SRCLIB                      PIC X(10).
       01  X-SRCRCN                      PIC X(10).
       01  X-SRCMBR                      PIC X(10).
       01  X-INP-PTR                     USAGE POINTER.
       01  X-SRC-PTR                     USAGE POINTER.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-SRCLIB
                                X-SRCRCN
                                X-SRCMBR
                                X-INP-PTR
                                X-SRC-PTR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0028"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB DIE TEILDATEI VORHANDEN
           MOVE     X-SRCLIB TO CHECK-LIB.
           MOVE     X-SRCRCN TO CHECK-RCN.
           MOVE     X-SRCMBR TO CHECK-MBR.
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* EINLESEN DER SOURCE
           MOVE     X-SRCLIB TO SRCLIB-WRK.
           MOVE     X-SRCRCN TO SRCRCN-WRK.
           MOVE     X-SRCMBR TO SRCMBR-WRK.
           IF       X-INP-PTR = NULL
                    PERFORM  READ-SOURCE
           ELSE
                    CALL     "CRP0016" USING X-INP-PTR
                                             X-SRC-PTR
                    END-CALL
           END-IF.

     ‚* JETZT ALLE COPY-MODULE SUCHEN UND ANHÄNGEN
           PERFORM  FIND-COPYMODULES.

           SET      MAP-PTR OF MAPDTA-REC TO X-SRC-PTR.
           MOVE     MAP-SORT      TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.

           CALL     "HLP0012" USING X-SRC-PTR
           END-CALL.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DURCHLESEN DER SOURCE UND SUCHE DES COPY-MODULS
     ‚*--------------------------------------------------------------
       FIND-COPYMODULES SECTION.
       FND-CPY-00.

           INITIALIZE RET-CODE.
     ‚* LAUFENDE NUMMER INITIALISIEREN
           MOVE     ZEROES  TO LFDN-MAP.

       FND-CPY-20.
           SET      MAP-PTR OF MAPDTA-REC TO X-SRC-PTR.
           MOVE     LFDN-MAP      TO MAP-LFDN OF MAPDTA-REC.
           MOVE     MAP-READ-LFDN TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    GO TO FND-CPY-90
           END-IF.

     ‚* ZEILE HOCHZÄHLEN
           ADD      1 TO LFDN-MAP.

     ‚* ZEILE ÜBERNEHMEN
           MOVE     MAP-REC OF MAPDTA-REC(10:)  TO SRC-WRK.
           MOVE     SRCSEQ OF SRC-WRK           TO SRCZEI-WRK.
           MOVE     SRCDTA OF SRC-WRK           TO SOURCE-WRK.

     ‚* IST DIE ZEILE SELBST AUCH EIN CPY-MODUL?
           IF       MAP-REC OF MAPDTA-REC(3:1) = "N"
            AND     MAP-KEY OF MAPDTA-REC(31:6) IS NUMERIC
                    MOVE MAP-KEY OF MAPDTA-REC(31:4) TO SRCZEI-WRK(1:4)
                    MOVE MAP-KEY OF MAPDTA-REC(35:2) TO SRCZEI-WRK(5:2)
           END-IF.

           PERFORM  GET-COPY-MODUL.
           IF       COPY-DDS-LIB = SPACES
            OR      COPY-DDS-RCN = SPACES
            OR      COPY-DDS-MBR = SPACES
                    GO TO FND-CPY-20
           END-IF.

     ‚* DIESE ZEILE DANN LÖSCHEN
           MOVE     MAP-DELETE    TO MAP-ACT  OF MAPDTA-REC.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-SRC-PTR TO MAP-PTR OF MAPDTA-REC.

     ‚* COPY-MODUL ANHÄNGEN
           MOVE     COPY-DDS-LIB TO SRCLIB-WRK.
           MOVE     COPY-DDS-RCN TO SRCRCN-WRK.
           MOVE     COPY-DDS-MBR TO SRCMBR-WRK.
           PERFORM  READ-SOURCE.

           GO TO    FND-CPY-20.

       FND-CPY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINLESEN DER SOURCE
     ‚*--------------------------------------------------------------
       READ-SOURCE SECTION.
       RED-SRC-00.

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
                                    X-SRC-PTR
                                    SECTION-PTR
                                    RET-CODE
           END-CALL.

     ‚* DEN BRAUCHEN WIR NICHT
           IF       SECTION-PTR NOT = NULL
                    SET      MAP-PTR OF MAPDTA-REC TO SECTION-PTR
                    MOVE     MAP-CLEAR TO MAP-ACT OF MAPDTA-REC
                    PERFORM  COPY-CALL-MAPDTA
           END-IF.
           SET      SECTION-PTR TO NULL.

       RED-SRC-90.
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
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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

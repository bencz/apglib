       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFZIFFERNBERECHNUNG                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  24.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFZIFFERNBERECHNUNG (KOPIE AUS LGPGM007)   *
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
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0007".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST  ".

     ‚* FELDER ZUR BERECHNUNG PRÜFZIFFER
       01  PRUEF-NUMMER.
        05 STELLE1                       PIC 9(1).
        05 STELLE2                       PIC 9(1).
        05 STELLE3                       PIC 9(1).
        05 STELLE4                       PIC 9(1).
        05 STELLE5                       PIC 9(1).
        05 STELLE6                       PIC 9(1).
        05 STELLE7                       PIC 9(1).
        05 STELLE8                       PIC 9(1).
        05 STELLE9                       PIC 9(1).
        05 STELLE10                      PIC 9(1).
        05 STELLE11                      PIC 9(1).
        05 STELLE12                      PIC 9(1).
        05 STELLE13                      PIC 9(1).
        05 STELLE14                      PIC 9(1).
        05 STELLE15                      PIC 9(1).
        05 STELLE16                      PIC 9(1).
        05 STELLE17                      PIC 9(1).
        05 STELLE18                      PIC 9(1).
        05 STELLE19                      PIC 9(1).
        05 STELLE20                      PIC 9(1).
        05 STELLE21                      PIC 9(1).
        05 STELLE22                      PIC 9(1).
        05 STELLE23                      PIC 9(1).
        05 STELLE24                      PIC 9(1).
        05 STELLE25                      PIC 9(1).
        05 STELLE26                      PIC 9(1).
       01  PRUEF-RED           REDEFINES PRUEF-NUMMER PIC 9(26).

       01  PRUEF-SUMME.
        05 PRUEF-ANF                     PIC 9(4).
        05 PRUEF-REST                    PIC 9(1).
       01  PRUEF-SUMME-RED     REDEFINES PRUEF-SUMME PIC 9(5).

     ‚* TEMPORÄRE INDIKATOREN
       01  PRUEF-ZIFFER                  PIC 9(1).
       01  PRUEF-ZIFFER2                 PIC 9(2).
       01  SYSNAME-WRK                   PIC X(8).
       01  SUMME                         PIC 9(9).
       01  REST                          PIC 9(9).
       01  DUMMY-WRK                     PIC 9(9).

       01  PRUEFNUMMER-2ST.
        05 STELLE-1-2                    PIC 9(2).
        05 STELLE-3-4                    PIC 9(2).
        05 STELLE-5-6                    PIC 9(2).
        05 STELLE-7-8                    PIC 9(2).
        05 STELLE-9-10                   PIC 9(2).
        05 STELLE-11-12                  PIC 9(2).
        05 STELLE-13-14                  PIC 9(2).
        05 STELLE-15-16                  PIC 9(2).
        05 STELLE-17-18                  PIC 9(2).
        05 STELLE-19-20                  PIC 9(2).
        05 STELLE-21-22                  PIC 9(2).
        05 STELLE-23-24                  PIC 9(2).
        05 STELLE-25-26                  PIC 9(2).

       01  CHECK-WRK                     PIC 9(5).
     ‚* APG: DIES SIND 1:1 DIE VARIABLEN VON DER LOGIK DIE NEC UNS
     ‚* AM 12.03.2009 GESCHICKT HAT FÜR DIE TROLLIEBERECHNUNG.
       01  CHECK.
        05 CHECK-1                       PIC 9 VALUE ZERO.
        05 CHECK-2                       PIC 9 VALUE ZERO.
        05 CHECK-3                       PIC 9 VALUE ZERO.
        05 CHECK-4                       PIC 9 VALUE ZERO.
        05 CHECK-5                       PIC 9 VALUE ZERO.

       01  TOTAAL                        PIC 9(2) VALUE ZEROES.
       01  FILLER                        REDEFINES TOTAAL.
        05 TOT-1                         PIC 9.
        05 TOT-2                         PIC 9.

       01  CHECK-1-3-5                   PIC 9(3) VALUE ZEROES.
       01  FILLER                        REDEFINES CHECK-1-3-5.
        05 CHECK-1-3-5-A                 PIC 9.
        05 CHECK-1-3-5-B                 PIC 9.
        05 CHECK-1-3-5-C                 PIC 9.

       01  WRK-CHECK                     PIC 9(4) VALUE ZEROES.
       01  FILLER                        REDEFINES WRK-CHECK.
        05 WRK-CHECK-0                   PIC 9.
        05 WRK-CHECK-1                   PIC 9.
        05 WRK-CHECK-3                   PIC 9.
        05 WRK-CHECK-5                   PIC 9.

       01  MODULUS-10                    PIC 9 VALUE ZERO.

       01  MOD10-TOT                     PIC 99.
       01  MOD10-TOT-R                   REDEFINES MOD10-TOT.
        03 MOD10-TOT-1                   PIC 9.
        03 MOD10-TOT-2                   PIC 9.
       01  MOD10-CHECK-DIGIT             PIC 9.
       01  MODULUS-A-10                  PIC 9(11).
       01  MODULUS-A-10-R                REDEFINES MODULUS-A-10.
        03 MOD10-A-11                    PIC 9.
        03 MOD10-A-10                    PIC 9.
        03 MOD10-A-9                     PIC 9.
        03 MOD10-A-8                     PIC 9.
        03 MOD10-A-7                     PIC 9.
        03 MOD10-A-6                     PIC 9.
        03 MOD10-A-5                     PIC 9.
        03 MOD10-A-4                     PIC 9.
        03 MOD10-A-3                     PIC 9.
        03 MOD10-A-2                     PIC 9.
        03 MOD10-A-1                     PIC 9.
       01  MODULUS-B-10                  PIC 9(6).
       01  MODULUS-B-10-R                REDEFINES MODULUS-B-10.
        03 MOD10-B-11                    PIC 9.
        03 MOD10-B-9                     PIC 9.
        03 MOD10-B-7                     PIC 9.
        03 MOD10-B-5                     PIC 9.
        03 MOD10-B-3                     PIC 9.
        03 MOD10-B-1                     PIC 9.

       01  BARCODE-TYP                   PIC X(1).
       01  LFS-INSTALL                   PIC X(3).

       01  POS                           PIC 9(4).
       01  PRUEFNUMMER-WRK               PIC 9(26).
       01  PRUEFNUMMER-RED     REDEFINES PRUEFNUMMER-WRK PIC X(26).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PRUEFNUMMER                 LIKE PRUEFNUMMER-WRK.
       01  X-BARCODE-TYP                 LIKE BARCODE-TYP.
       01  X-LFS-INSTALL                 LIKE LFS-INSTALL.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-PRUEFNUMMER
                                         X-BARCODE-TYP
                                         X-LFS-INSTALL.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0007" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* BERECHNUNGEN FÜR SPEZIELLE VERSIONEN DURCHFÜHREN?
           IF       ADDRESS OF X-LFS-INSTALL NOT = NULL
                    MOVE X-LFS-INSTALL TO LFS-INSTALL
           ELSE
                    MOVE "LFS"         TO LFS-INSTALL
           END-IF.
     ‚* WELCHE PRÜFZIFFER SOLL BERECHNET WERDEN?
           IF       ADDRESS OF X-BARCODE-TYP NOT = NULL
                    MOVE X-BARCODE-TYP TO BARCODE-TYP
           ELSE
                    MOVE "I"           TO BARCODE-TYP
           END-IF.

     ‚* PRÜFZIFFER ÜBERNEHMEN
           MOVE     X-PRUEFNUMMER      TO PRUEFNUMMER-RED.
     ‚* ERSTMAL DEN STRING NACH RECHTS VERSCHIEBEN, DANACH DANN ALLE
     ‚* SPACES DURCH ZEROES ERSETZEN
           MOVE     ZEROES TO POS.
           INSPECT  PRUEFNUMMER-RED TALLYING POS FOR CHARACTERS
                    BEFORE INITIAL SPACES.
           IF       POS = ZEROES
                    MOVE 1 TO POS
           END-IF.
           IF       POS NOT = LENGTH OF PRUEFNUMMER-RED
                    MOVE PRUEFNUMMER-RED(1:POS) TO
                         PRUEFNUMMER-RED(  LENGTH OF PRUEFNUMMER-RED
                                         - POS + 1:)
                    MOVE SPACES TO PRUEFNUMMER-RED(1:POS)
           END-IF.

     ‚* JETZT UNGÜLTIGE ZEICHEN ERSETZEN
           MOVE     ZEROES TO POS.
           PERFORM  LENGTH OF PRUEFNUMMER-RED TIMES
                    ADD  1 TO POS
                    IF   PRUEFNUMMER-RED(POS:1) IS NOT NUMERIC
                         MOVE ZEROES TO PRUEFNUMMER-RED(POS:1)
                    END-IF
           END-PERFORM.

           MOVE     PRUEFNUMMER-WRK    TO PRUEF-RED.
     ‚* EAN 13/8 ODER 2/5 INTERLEAVED ?
           EVALUATE BARCODE-TYP ALSO LFS-INSTALL
     ‚*             EAN-PRÜFZIFFER
                    WHEN "E" ALSO ANY
                         PERFORM PR-ZI-BER-EAN
     ‚*             2OF5-PRÜFZIFFER
                    WHEN "I" ALSO ANY
                         PERFORM PR-ZI-BER-INT
                         PERFORM PR-ZI-BER-2STELLIG
     ‚*             NECKERMANN TROLLIE-ETIKETT
                    WHEN "T" ALSO "NEC"
                         PERFORM MODULUS10-GEW2
     ‚*             NECKERMANN RETOUREN-ETIKETT
                    WHEN "R" ALSO "NEC"
                         PERFORM MODULUS10-GEW2-RETOURE
                    WHEN OTHER
                         GO TO ENDE
           END-EVALUATE.

       ENDE.
     ‚* JETZT WERTE FÜLLEN
           INITIALIZE SNDPGMMSG-REC.
           MOVE     PRUEF-ZIFFER   TO MSG     OF SNDPGMMSG-REC.
           MOVE     PRUEF-ZIFFER   TO MSGDTA  OF SNDPGMMSG-REC.
           MOVE     "CPF9897"      TO MSGID   OF SNDPGMMSG-REC.
           MOVE     "*STATUS"      TO MSGTYPE OF SNDPGMMSG-REC.
           MOVE     "*EXT"         TO TOPGMQ  OF SNDPGMMSG-REC.
           MOVE     "*STATUS"      TO MSGTYPE OF SNDPGMMSG-REC.
           CALL     "CLP0016" USING SNDPGMMSG-REC
                                    RET-CODE
           END-CALL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*-------------------------------------------------------------
     ‚* BERECHNEN PRÜFZIFFER EAN13 BZW EAN8
     ‚*-------------------------------------------------------------
       PR-ZI-BER-EAN SECTION.

           COMPUTE  PRUEF-SUMME-RED  =       STELLE1
                                       + 3 * STELLE2
                                       +     STELLE3
                                       + 3 * STELLE4
                                       +     STELLE5
                                       + 3 * STELLE6
                                       +     STELLE7
                                       + 3 * STELLE8
                                       +     STELLE9
                                       + 3 * STELLE10
                                       +     STELLE11
                                       + 3 * STELLE12
                                       +     STELLE13
                                       + 3 * STELLE14
                                       +     STELLE15
                                       + 3 * STELLE16
                                       +     STELLE17
                                       + 3 * STELLE18
                                       +     STELLE19
                                       + 3 * STELLE20
                                       +     STELLE21
                                       + 3 * STELLE22
                                       +     STELLE23
                                       + 3 * STELLE24
                                       +     STELLE25
                                       + 3 * STELLE26
           END-COMPUTE.

           IF       PRUEF-SUMME-RED  = 0
                    MOVE  10  TO  PRUEF-SUMME-RED
           END-IF.

           COMPUTE  PRUEF-ZIFFER = 10 - PRUEF-REST.

           EXIT.
      /
     ‚*-------------------------------------------------------------
     ‚* BERECHNEN PRÜFZIFFER 2 OF 5 INTERLEAVED
     ‚* VERFAHREN: MODULUS-10-VERFAHREN, GEWICHTUNG 3
     ‚* BEI QUERSUMMENBILDUNG WERDEN DIE PRODUKTE UND NICHT (WIE IM
     ‚* DDS-HANDBUCH AS/400) DIE EINZELNEN ZIFFERN ADDIERT
     ‚*-------------------------------------------------------------
       PR-ZI-BER-INT SECTION.

           COMPUTE  PRUEF-SUMME-RED  =       STELLE1
                                       + 3 * STELLE2
                                       +     STELLE3
                                       + 3 * STELLE4
                                       +     STELLE5
                                       + 3 * STELLE6
                                       +     STELLE7
                                       + 3 * STELLE8
                                       +     STELLE9
                                       + 3 * STELLE10
                                       +     STELLE11
                                       + 3 * STELLE12
                                       +     STELLE13
                                       + 3 * STELLE14
                                       +     STELLE15
                                       + 3 * STELLE16
                                       +     STELLE17
                                       + 3 * STELLE18
                                       +     STELLE19
                                       + 3 * STELLE20
                                       +     STELLE21
                                       + 3 * STELLE22
                                       +     STELLE23
                                       + 3 * STELLE24
                                       +     STELLE25
                                       + 3 * STELLE26
           END-COMPUTE.

           IF       PRUEF-REST = ZERO
                    MOVE  ZERO TO  PRUEF-ZIFFER
           ELSE
                    SUBTRACT PRUEF-REST FROM 10
                    GIVING PRUEF-ZIFFER
           END-IF.

           EXIT.
      /
     ‚*-------------------------------------------------------------
     ‚* BERECHNEN 2STELLIGE PRÜFZIFFER MIT 2STELLIGER GEWICHTUNG
     ‚*-------------------------------------------------------------
       PR-ZI-BER-2STELLIG SECTION.
       2ST-PRZ-00.

           MOVE     PRUEFNUMMER-WRK TO PRUEFNUMMER-2ST.

           COMPUTE  SUMME =   17 * STELLE-1-2
                            + 31 * STELLE-3-4
                            + 17 * STELLE-5-6
                            + 31 * STELLE-7-8
                            + 17 * STELLE-9-10
                            + 31 * STELLE-11-12
                            + 17 * STELLE-13-14
                            + 31 * STELLE-15-16
                            + 17 * STELLE-17-18
                            + 31 * STELLE-19-20
                            + 17 * STELLE-21-22
                            + 31 * STELLE-23-24
                            + 17 * STELLE-25-26
           END-COMPUTE.

           MOVE     ZEROES TO DUMMY-WRK.
           MOVE     ZEROES TO REST.

     ‚* DIE SUMME WIRD DURCH 97 GETEILT UND DER REST VON 97 ABGEZOGEN
     ‚* DIES IST DANN DIE 2STELLIGE PRÜFZIFFER
           DIVIDE   SUMME BY 97 GIVING DUMMY-WRK REMAINDER REST.
           MOVE     ZEROES TO PRUEF-ZIFFER2.
           COMPUTE  PRUEF-ZIFFER2 = 97 - REST.

       2ST-PRZ-90.
           EXIT.
      /
     ‚*-------------------------------------------------------------
     ‚* APG: BERECHNEN PRÜFZIFFER 2 OF 5 INTERLEAVED
     ‚* VERFAHREN: MODULUS-10-VERFAHREN, GEWICHTUNG 2
     ‚*-------------------------------------------------------------
       MODULUS10-GEW2 SECTION.
       MOD-GW2-00.

           INITIALIZE CHECK.
           INITIALIZE CHECK-WRK.
           INITIALIZE TOTAAL.
           INITIALIZE CHECK-1-3-5.
           INITIALIZE WRK-CHECK.
           INITIALIZE MODULUS-10.

           COMPUTE  CHECK-WRK = PRUEFNUMMER-WRK.
           MOVE     CHECK-WRK TO CHECK.
     ‚* LOGIK VON NECKERMANN, ZUR BESSEREN VERSTÄNDLICHKEIT GLEICH-
     ‚* GEHALTEN SOLLTE NEC ETWAS ÄNDERN
           MOVE     CHECK-1               TO CHECK-1-3-5-A.
           MOVE     CHECK-3               TO CHECK-1-3-5-B.
           MOVE     CHECK-5               TO CHECK-1-3-5-C.
           COMPUTE  WRK-CHECK  = CHECK-1-3-5 * 2.
           COMPUTE  TOTAAL     = WRK-CHECK-0 + WRK-CHECK-1 + CHECK-2 +
                                 WRK-CHECK-3 + CHECK-4 + WRK-CHECK-5.
           SUBTRACT TOT-2 FROM 10 GIVING MODULUS-10.

     ‚* ERGEBNIS ÜBERNEHMEN
           MOVE     MODULUS-10 TO PRUEF-ZIFFER.

       MOD-GW2-90.
           EXIT.
      /
     ‚*-------------------------------------------------------------
     ‚* APG: BERECHNEN PRÜFZIFFER 2 OF 5 INTERLEAVED
     ‚* VERFAHREN: MODULUS-10-VERFAHREN, GEWICHTUNG 2
     ‚*-------------------------------------------------------------
       MODULUS10-GEW2-RETOURE SECTION.
       MOD-GR2-00.

           INITIALIZE MODULUS-A-10.
           INITIALIZE MODULUS-B-10.
           COMPUTE  MODULUS-A-10 = PRUEFNUMMER-WRK.
           MOVE     MOD10-A-1            TO MOD10-B-1.
           MOVE     MOD10-A-3            TO MOD10-B-3.
           MOVE     MOD10-A-5            TO MOD10-B-5.
           MOVE     MOD10-A-7            TO MOD10-B-7.
           MOVE     MOD10-A-9            TO MOD10-B-9.
           MOVE     MOD10-A-11           TO MOD10-B-11.
           COMPUTE  MODULUS-B-10 = MODULUS-B-10 * 2.
           MOVE     MOD10-B-1            TO MOD10-A-1.
           MOVE     MOD10-B-3            TO MOD10-A-3.
           MOVE     MOD10-B-5            TO MOD10-A-5.
           MOVE     MOD10-B-7            TO MOD10-A-7.
           MOVE     MOD10-B-9            TO MOD10-A-9.
           MOVE     MOD10-B-11           TO MOD10-A-11.
           COMPUTE  MOD10-TOT =       MOD10-A-1
                                    + MOD10-A-2
                                    + MOD10-A-3
                                    + MOD10-A-4
                                    + MOD10-A-5
                                    + MOD10-A-6
                                    + MOD10-A-7
                                    + MOD10-A-8
                                    + MOD10-A-9
                                    + MOD10-A-10
                                    + MOD10-A-11
           END-COMPUTE.

           IF       MOD10-TOT-2 = ZEROES
                    MOVE 0 TO MOD10-CHECK-DIGIT
           ELSE
                    COMPUTE MOD10-CHECK-DIGIT = 10 - MOD10-TOT-2
           END-IF.

     ‚* ERGEBNIS ÜBERNEHMEN
           MOVE     MOD10-CHECK-DIGIT TO PRUEF-ZIFFER.

       MOD-GR2-90.
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

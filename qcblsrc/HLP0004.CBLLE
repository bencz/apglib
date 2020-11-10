       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFBEREITEN TELEFONNUMMER UND VERGLEICH                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  01.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TELEFONNUMMER AUFBEREITEN                    *
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

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0004".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  TELEPHONE-WRK                 PIC X(20).
       01  TELEPHONE-NUM                 PIC X(20).

       01  PLUS-WRK                      PIC 9(1).
       01  POS                           PIC 9(4).
       01  LEN                           PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TELEPHONE                   PIC X(20).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TELEPHONE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0004" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN OB ES DIE TELEFONNUMMER GIBT
           MOVE     X-TELEPHONE   TO TELEPHONE-WRK.
           PERFORM  GET-NUMBER-NUMERIC.
           MOVE     TELEPHONE-NUM TO X-TELEPHONE.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* NUMMER NUMERISCH AUFBEREITEN
     ‚*--------------------------------------------------------------
       GET-NUMBER-NUMERIC SECTION.
       GET-NBR-00.

           INITIALIZE TELEPHONE-NUM.
     ‚* ALLE ZEICHEN AUFBEREITEN
           MOVE     1      TO POS.
           MOVE     1      TO LEN.
           MOVE     ZEROES TO PLUS-WRK.
           PERFORM  WITH TEST BEFORE UNTIL POS =
                     LENGTH OF TELEPHONE-WRK
     ‚*             NUMERISCHE WERTE WERDEN ÜBERNOMMEN
                    IF   TELEPHONE-WRK(POS:1) IS NUMERIC
                         MOVE TELEPHONE-WRK(POS:1) TO
                              TELEPHONE-NUM(LEN:1)
                         ADD 1 TO LEN
                    END-IF
     ‚*             EIN + WIRD IN 00 GEÄNDERT
                    IF   TELEPHONE-WRK(POS:1) = "+"
                         MOVE "00" TO TELEPHONE-NUM(LEN:2)
                         MOVE 1    TO PLUS-WRK
                         ADD 2 TO LEN
                    END-IF
     ‚*             POSITION HOCHZÄHLEN
                    ADD 1 TO POS
           END-PERFORM.

     ‚* GIBT ES EIN + SO WURDE AUCH +49 EINGEGEBEN, ALSO STEHT DORT
     ‚* JETZT 0049. ABER WENN ES KEIN PLUS GIBT, WURDE EVTL. AUCH
     ‚* KEIN 49 EINGETRAGEN. DIES JETZT GLEICHZIEHEN
           IF       PLUS-WRK               = ZEROES
            AND     TELEPHONE-NUM(1:2) NOT = "00"
     ‚*             NUR DIE ERSTEN 3 WENN SCHON EINE FÜHRENDE NULL
     ‚*             EINGEGEBEN WURDE, DIESE WIRD DANN ÜBERSCHREIBEN
                    IF   TELEPHONE-NUM(1:1) = "0"
                         MOVE TELEPHONE-NUM TO TELEPHONE-NUM(4:)
                    ELSE
                         MOVE TELEPHONE-NUM TO TELEPHONE-NUM(5:)
                    END-IF
                    MOVE "0049"        TO TELEPHONE-NUM(1:4)
           END-IF.

       GET-NBR-90.
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

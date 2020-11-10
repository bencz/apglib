       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9100.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSFÜHREN ABBRUCHTÄTIGKEIT NACH ERFOLGTER PRÜFUNG             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ABBRUCHAUSFÜHRUNG                            *
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
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9100".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PGM                         LIKE PGM-WRK.
       01  X-TYP                         PIC X(8).
       01  X-ENDMODE                     PIC 9(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PGM
                                X-TYP
                                X-ENDMODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9100 " TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           EVALUATE X-ENDMODE
     ‚*             NICHTS MACHEN
                    WHEN ZEROES
                         GO TO ENDE
     ‚*             WINDOW ANZEIGEN
                    WHEN 1
                         CALL "CFP9101" IN LIBRARY "APG"
                                        USING X-PGM
                                              X-TYP
                         END-CALL
     ‚*             BLUESCREEN
                    WHEN 2
                         CALL "CFP9102" IN LIBRARY "APG"
                                        USING X-PGM
                                              X-TYP
                         END-CALL
     ‚*             BLUESCREEN + ABBRUCH
                    WHEN 3
                         CALL "CFP9102" IN LIBRARY "APG"
                                        USING X-PGM
                                              X-TYP
                         END-CALL
     ‚*                  JETZT BEENDEN
                         CALL "CFP9107" IN LIBRARY "APG"
                         END-CALL
     ‚*             4 UND 5 WERDEN IM CPY-MODUL PERMISSION GEHANDLET
     ‚*              => 4 = GO TO ENDE, 5 = GOBACK
                    WHEN 4
                    WHEN 5
                         GO TO ENDE
     ‚*             ABBRUCH
                    WHEN 7
                         CALL "CFP9107" IN LIBRARY "APG"
                         END-CALL
     ‚*             SIGNOFF
                    WHEN 9
                         CALL "CFP9109" IN LIBRARY "APG"
                         END-CALL
                    WHEN OTHER
                         GO TO ENDE
           END-EVALUATE.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /

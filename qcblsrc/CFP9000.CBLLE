       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFRUF VON DATEN FÜR PROGRAMMSTART UND PROGRAMMENDE           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  14.12.2007                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PGM-INIT/PGM-EXIT                            *
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

       01  PGM-WRK                       PIC X(10) VALUE "CFP9000".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  FIRST-CALL-INFO               PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-AKTION                      PIC X(10).
       01  X-OUTPUT                      LIKE JOBINFO.
       01  X-PGM                         PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-AKTION
                                X-OUTPUT
                                X-PGM.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9000 " TO PGM-WRK.

     ‚* AUSWERTEN DER AKTION
           EVALUATE X-AKTION WHEN "INIT" PERFORM PGM-INIT
                             WHEN "EXIT" PERFORM PGM-EXIT
                             WHEN OTHER  GO TO ENDE
           END-EVALUATE.

       ENDE.
           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN BESTIMMTER TÄTIGKEITEN AM PROGRAMMSTART
     ‚*--------------------------------------------------------------
       PGM-INIT SECTION.
       PGM-INI-00.

     ‚* ERMITTELN USER UND BILDSCHIRM
           IF       FIRST-CALL-INFO NOT = "1"
                    MOVE "1" TO FIRST-CALL-INFO
                    CALL           "CLP0001"  IN LIBRARY "APG"
                                         USING   T-USER    OF JOBINFO
                                                 T-TERM    OF JOBINFO
                                                 T-CURLIB  OF JOBINFO
                                                 T-LIBL    OF JOBINFO
                    END-CALL
           END-IF.

     ‚* SPRACHE IMMER NEU ERMITTELN
           CALL     "CLP0056" USING T-SPRACHE OF JOBINFO
           END-CALL.

     ‚* ZURÜCKGEBEN DER JOBDATEN
           MOVE     JOBINFO TO X-OUTPUT.

       PGM-INI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN BESTIMMTER TÄTIGKEITEN AM PROGRAMMSTART
     ‚*--------------------------------------------------------------
       PGM-EXIT SECTION.
       PGM-EXT-00.

           INITIALIZE RET-CODE.

       PGM-EXT-90.
           EXIT.
      /

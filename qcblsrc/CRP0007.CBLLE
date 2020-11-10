       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0007.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB POINTER-KEY VORHANDEN IST                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN OB KEY VORHANDEN IST                  *
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
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0007".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
       01  X-KEY                         LIKE MAP-KEY OF MAPDTA-REC.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-POINTER
                                         X-KEY
                                         X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0007" TO PGM-WRK.

           INITIALIZE X-RETCODE.
     ‚* JETZT IN MAPDTA DIE SOURCE SPEICHERN
           INITIALIZE MAPDTA-REC.
           MOVE     X-KEY              TO MAP-KEY OF MAPDTA-REC.
           MOVE     MAP-READ           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-POINTER.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /

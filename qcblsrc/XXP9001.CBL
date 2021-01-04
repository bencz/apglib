       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* FEHLERAFTES PGM FÜR FALSCHEN PARAMETER AUFRUF                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  15.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  FEHLERHAFTES BEISPIEL-PGMˆ!!!‚               *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9001".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PARM-WRK1                   PIC X(10000).
       01  X-PARM-WRK2                   PIC X(10000).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PARM-WRK1
                                X-PARM-WRK2.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* IN X-PGM-WRK1 STEHT NUN "HALLO WELT" GETRENNT DURCH DIE
     ‚* RESTLICHEN LEERZEICHEN DES ERSTEN 50-STELLIGEN PARAMETERS
           GO TO    ENDE.

     ‚* NUR DAMIT DER DEBUGGER DIE VARIABLEN KENNT
           MOVE     X-PARM-WRK1  TO X-PARM-WRK1.
           MOVE     X-PARM-WRK2  TO X-PARM-WRK2.

       ENDE.

           GOBACK.
      /

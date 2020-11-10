       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9009.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* POINTER-TEST                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.04.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST MIT POINTERN                            *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9009".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  PTR-TAB                       OCCURS 10 TIMES.
        05 PTR-WRK                       USAGE POINTER.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.


       ENDE.

           GOBACK.
      /

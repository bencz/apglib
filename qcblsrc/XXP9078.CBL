       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9078.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LOW-VALUE/HIGH VALUE                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.04.2016                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST PROGRAM                                 *
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

       01  PGM-WRK                        PIC X(10) VALUE "XXP9078".

       01  VARIABLE1                      PIC X(20).
       01  VARIABLE2                 LIKE VARIABLE1 (+5).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PARAM             USAGE POINTER.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PARAM.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           CALL     "XXP9079" USING X-PARAM.

       ENDE.
           GOBACK.
      /

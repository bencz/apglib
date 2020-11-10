       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9072.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  14.10.2014                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
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

       01  PGM-WRK                        PIC X(10) VALUE "XXP9072".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

       01  BUF-PTR                        USAGE IS POINTER.

      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       COPY TRGBUF OF QSYSINC-QCBLLESRC.
       01  TRG-PARM2                      PIC S9(00009) BINARY.
       01  OLD-RECORD.
           COPY DDS-ALL-FORMATS OF PTSTTRP.
       01  NEW-RECORD.
           COPY DDS-ALL-FORMATS OF PTSTTRP.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING QDB-TRIGGER-BUFFER
                                TRG-PARM2.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           SET      BUF-PTR TO ADDRESS OF QDB-TRIGGER-BUFFER.
           SET      BUF-PTR UP BY OLD-RECORD-OFFSET.
           SET      ADDRESS OF OLD-RECORD TO BUF-PTR.

           SET      BUF-PTR TO ADDRESS OF QDB-TRIGGER-BUFFER.
           SET      BUF-PTR UP BY NEW-RECORD-OFFSET.
           SET      ADDRESS OF NEW-RECORD TO BUF-PTR.

       ENDE.

           GOBACK.
      /

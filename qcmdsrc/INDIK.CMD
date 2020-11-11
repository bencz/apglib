             CMD        PROMPT('Indikatoren prüfen')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('INDIK') RSTD(*NO) MIN(0)
             PARM       KWD(DSPLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Bibliothek')
             PARM       KWD(DSPFILE) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Datei')
             PARM       KWD(DSPMBR) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Teildatei')

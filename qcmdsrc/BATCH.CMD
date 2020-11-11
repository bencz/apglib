             CMD        PROMPT('BATCHPROGRAMM STARTEN')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('BATCH') RSTD(*NO) MIN(0)
             PARM       KWD(PARM) TYPE(*CHAR) LEN(1) RSTD(*NO)  +
                         MIN(0) CHOICE('0/1') PROMPT('Parameter')

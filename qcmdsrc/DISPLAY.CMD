             CMD        PROMPT('Aufruf DISPLAY')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('DISPLAY') RSTD(*NO) MIN(0)
             PARM       KWD(DISPLAY) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Display')
             PARM       KWD(FMT) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Format')
             PARM       KWD(SFL) TYPE(*CHAR) LEN(1) RSTD(*NO)  +
                         MIN(0) CHOICE('(0=nein/1=ja)') DFT(0) +
                         PROMPT('Subfile')

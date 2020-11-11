             CMD        PROMPT('KD-INFORMATION')
             PARM       KWD(CALLPGM) TYPE(*CHAR) LEN(10) +
                          CONSTANT('KDINFO') RSTD(*NO) MIN(0)
             PARM       KWD(DIENST) TYPE(*CHAR) LEN(10) RSTD(*NO)  +
                         MIN(0) CHOICE('Name') PROMPT('Dienst')

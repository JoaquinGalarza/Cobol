 000001        IDENTIFICATION DIVISION.
 000002         PROGRAM-ID PGMDBZ33.
 000003        ENVIRONMENT DIVISION.
 000004        CONFIGURATION SECTION.
 000005        SPECIAL-NAMES.
 000006            DECIMAL-POINT IS COMMA.
 000007        INPUT-OUTPUT SECTION.
 000008        FILE-CONTROL.
 000009              SELECT CLIENTE ASSIGN CLIENTE
 000010                     FILE STATUS IS FS-CLIENTE.
 000011
 000012              SELECT SALIDA  ASSIGN SALIDA
 000013                     FILE STATUS IS FS-SALIDA.
 000014
 000015        DATA DIVISION.
 000016        FILE SECTION.
 000017        FD CLIENTE
 000018             BLOCK CONTAINS 0 RECORDS
 000019             RECORDING MODE IS F.
 000020        01 REG-CLIENTE PIC X(203).
 000021
 000022        FD SALIDA
 000023             BLOCK CONTAINS 0 RECORDS
 000024             RECORDING MODE IS F.
 000025
 000026        01 REG-SALIDA PIC X(132).
 000027       ************************************
 000028       *             WORKING              *
 000029       ************************************
 000030        WORKING-STORAGE SECTION.
 000031       ************************************
 000032       *           FILE STATUS            *
 000033       ************************************
 000034        01  FS-FILE-STATUS.
 000035            05  FS-CLIENTE      PIC XX    VALUE SPACES.
 000036                88  FS-CLIENTE-OK         VALUE '00'.
 000037                88  FS-CLIENTE-EOF        VALUE '10'.
 000038            05  FS-CUENTA       PIC X     VALUE SPACES.
 000039                88  FS-CUENTA-OK          VALUE 'Y'.
 000040                88  FS-CUENTA-EOF         VALUE 'N'.
 000041            05  FS-SALIDA       PIC XX    VALUE SPACES.
 000042                88  FS-SALIDA-OK          VALUE '00'.
 000043                88  FS-SALIDA-EOF         VALUE '10'.
 000044       ************************************
 000045       *          LAYOUT CLIENTE          *
 000046       ***********************************
 000047        01 WS-CLIENTE.
 000048           05 TIPO-DOCUMENTO       PIC X(2).
 000049           05 NRO-DOCUMENTO        PIC S9(11)V USAGE COMP-3.
 000050           05 CLI-NRO-CLIENTE      PIC S9(5)V USAGE COMP-3.
 000051           05 NOMBRE-CLIENTE       PIC X(30).
 000052           05 APELLIDO-CLIENTE     PIC X(30).
 000053           05 DOMICILIO            PIC X(30).
 000054           05 CIUDAD               PIC X(30).
 000055           05 CODIGO-POSTAL        PIC X(8).
 000056           05 NACIONALIDAD         PIC X(30).
 000057           05 FECHA-DE-ALTA        PIC X(10).
 000058           05 FECHA-DE-BAJA        PIC X(10).
 000059           05 ESTADO-CIVIL         PIC X(2).
 000060           05 SEXO                 PIC X(2).
 000061           05 CORREO-ELECTRONICO   PIC X(30).
 000062           05 FECCHA-NACIMIENTO    PIC X(10).
 000063       ************************************
 000064       *          CONTADORES              *
 000065       ************************************
 000066        01 CN-CONTADORES.
 000067            05 CN-NOVEDADES-FD       PIC 9(05)  VALUE ZEROS.
 000068            05 CN-NOVEDADES-NFD      PIC 9(05)  VALUE ZEROS.
 000069            05 CN-NOVEDADES-ER       PIC 9(05)  VALUE ZEROS.
 000070            05 CN-NOVEDADES-GRABADAS PIC 9(05)  VALUE ZEROS.
 000071            05 CN-CUENTA-LINEA       PIC 9(05)  VALUE ZEROS.
 000072            05 CN-CUENTAS            PIC 9(05)  VALUE ZEROS.
 000073       ************************************
 000074       *          CONSTANTES              *
 000075       ************************************
 000076        01 CT-CONSTANTES.
 000077            05 CT-1000               PIC 9(04)  VALUE 1000.
 000078       ************************************
 000079       *          NUMERO DE ALU           *
 000080       ************************************
 000081 CIELO  01  WS-NRO-ALU.
 000082            05  WS-ALU               PIC 9(05)         VALUE ZEROS.
 000083        01  CR-NRO.
 000084            05  CR-NRO-CLI           PIC S9(5)V USAGE COMP-3.
 000085       ************************************
 000086       *         FECHA DE PROCESO         *
 000087       ************************************
 000088        01  WS-AREA.
 000089            05  WS-AREA-AA       PIC 9(04)         VALUE ZEROS.
 000090            05  WS-AREA-MM       PIC 9(02)         VALUE ZEROS.
 000091            05  WS-AREA-DD       PIC 9(02)         VALUE ZEROS.
 000092
 000093        01  WS-FECHA.
 000094            05  WS-FECHA-AA      PIC 9(04)         VALUE ZEROS.
 000095            05  WS-SEP1          PIC X(01)         VALUE '-'.
 000096            05  WS-FECHA-MM      PIC 9(02)         VALUE ZEROS.
 000097            05  WS-SEP2          PIC X(01)         VALUE '-'.
 000098            05  WS-FECHA-DD      PIC 9(02)         VALUE ZEROS.
 000099       ************************************
 000100       *       VARIABLES IMPRESION        *
 000101       ************************************
 000102        01  WS-REG-CLIENTE.
 000103            05  FILLER    PIC X(2)    VALUE SPACES.
 000104            05  WS-CLI-NOM    PIC X(30)    VALUE SPACES.
 000105            05  FILLER    PIC X(2)    VALUE SPACES.
 000106            05  FILLER    PIC X(01)    VALUE '|'.
 000107            05  FILLER    PIC X(2)    VALUE SPACES.
 000108            05  WS-CLI-APE    PIC X(30)    VALUE SPACES.
 000109            05  FILLER    PIC X(2)    VALUE SPACES.
 000110            05  FILLER    PIC X(01)    VALUE '|'.
 000111            05  FILLER    PIC X(2)    VALUE SPACES.
 000112            05  WS-CLI-NRO-CLI    PIC X(05)    VALUE SPACES.
 000113            05  FILLER    PIC X(8)    VALUE SPACES.
 000114            05  FILLER    PIC X(01)    VALUE '|'.
 000115            05  FILLER    PIC X(2)    VALUE SPACES.
 000116            05  WS-CLI-TIP-DOC    PIC X(02)    VALUE SPACES.
 000117            05  FILLER    PIC X(14)    VALUE SPACES.
 000118            05  FILLER    PIC X(01)    VALUE '|'.
 000119            05  FILLER    PIC X(2)    VALUE SPACES.
 000120            05  WS-CLI-NRO-DOC    PIC X(11)    VALUE SPACES.
 000121            05  FILLER    PIC X(4)    VALUE SPACES.
 000122            05  FILLER    PIC X(01)    VALUE '|'.
 000123            05  FILLER    PIC X(2)    VALUE SPACES.
 000124            05  WS-CLI-FECHA-ALTA    PIC X(10)    VALUE SPACES.
 000125            05  FILLER    PIC X(2)    VALUE SPACES.
 000126            05  FILLER    PIC X(01)    VALUE '|'.
 000127            05  FILLER    PIC X(2)    VALUE SPACES.
 000128            05  WS-CLI-FECHA-BAJA    PIC X(10)    VALUE SPACES.
 000129            05  FILLER    PIC X(2)    VALUE SPACES.
 000130            05  FILLER    PIC X(01)    VALUE '|'.
 000131
 000132
 000133        01  WS-REG-CUENTA.
 000134            05  FILLER    PIC X(2)    VALUE SPACES.
 000135            05  WS-CUE-TIP-CUE    PIC X(02)    VALUE SPACES.
 000136            05  FILLER    PIC X(11)    VALUE SPACES.
 000137            05  FILLER    PIC X(01)    VALUE '|'.
 000138            05  FILLER    PIC X(2)    VALUE SPACES.
 000139            05  WS-CUE-NRO-CUE    PIC X(15)    VALUE SPACES.
 000140            05  FILLER    PIC X(2)    VALUE SPACES.
 000141            05  FILLER    PIC X(01)    VALUE '|'.
 000142            05  FILLER    PIC X(2)    VALUE SPACES.
 000143            05  WS-CUE-MONEDA    PIC X(02)    VALUE SPACES.
 000144            05  FILLER    PIC X(6)    VALUE SPACES.
 000145            05  FILLER    PIC X(01)    VALUE '|'.
 000146            05  FILLER    PIC X(2)    VALUE SPACES.
 000147            05  WS-CUE-CBU    PIC X(11)    VALUE SPACES.
 000148            05  FILLER    PIC X(2)    VALUE SPACES.
 000149            05  FILLER    PIC X(01)    VALUE '|'.
 000150            05  FILLER    PIC X(2)    VALUE SPACES.
 000151            05  WS-CUE-NRO-CLI    PIC X(05)    VALUE SPACES.
 000152            05  FILLER    PIC X(8)    VALUE SPACES.
 000153            05  FILLER    PIC X(01)    VALUE '|'.
 000154            05  FILLER    PIC X(2)    VALUE SPACES.
 000155            05  WS-CUE-SALDO-ACT    PIC -ZZZ.ZZZ.999,99 VALUE ZEROS.
 000156            05  FILLER    PIC X(2)    VALUE SPACES.
 000157            05  FILLER    PIC X(01)    VALUE '|'.
 000158            05  FILLER    PIC X(2)    VALUE SPACES.
 000159            05  WS-CUE-FECHA-ACT    PIC X(10)    VALUE SPACES.
 000160            05  FILLER    PIC X(4)    VALUE SPACES.
 000161            05  FILLER    PIC X(01)    VALUE '|'.
 000162            05  FILLER    PIC X(2)    VALUE SPACES.
 000163            05  WS-CUE-FECHA-ULT    PIC X(10)    VALUE SPACES.
 000164            05  FILLER    PIC X(11)    VALUE SPACES.
 000165            05  FILLER    PIC X(01)    VALUE '|'.
 000166       ************************************
 000167       *            IMPRESION             *
 000168       ************************************
 000169        01  IP-TITULO.
 000170            05  FILLER      PIC X(20) VALUE  SPACES.
 000171            05  FILLER      PIC X(29) VALUE
 000172             'LISTADO DE CLIENTES Y CUENTAS'.
 000173            05  FILLER      PIC X(05) VALUE  SPACES.
 000174            05  FILLER      PIC X(07) VALUE  'FECHA: '.
 000175            05  IP-FECHA    PIC X(10) VALUE  SPACES.
 000176            05  FILLER      PIC X(05) VALUE  SPACES.
 000177            05  FILLER      PIC X(05) VALUE  'ALU: '.
 000178            05  IP-ALU      PIC X(04) VALUE  SPACES.
 000179
 000180        01  IP-SUBTITULO.
 000181            05  IP-BARRA.
 000182                10  FILLER        PIC X(54) VALUE
 000183                '------------------------------------------------------'.
 000184                10  FILLER        PIC X(54) VALUE
 000185                '------------------------------------------------------'.
 000186            05  IP-INTRODUCCION-CLIENTE.
 000187                10  FILLER    PIC X(38)    VALUE SPACES.
 000188                10  FILLER    PIC X(07)    VALUE 'CLIENTE'.
 000189            05  IP-INTRODUCCION-CUENTA.
 000190                10  FILLER    PIC X(38)    VALUE SPACES.
 000191                10  FILLER    PIC X(07)    VALUE 'CUENTAS'.
 000192            05  IP-NO-CUENTAS.
 000193                10  FILLER    PIC X(38)    VALUE SPACES.
 000194                10  FILLER    PIC X(19)    VALUE 'CLIENTE SIN CUENTAS'.
 000195            05  IP-CLIENTE.
 000196                10  FILLER    PIC X(14)    VALUE SPACES.
 000197                10  FILLER    PIC X(6)    VALUE 'NOMBRE'.
 000198                10  FILLER    PIC X(14)    VALUE SPACES.
 000199                10  FILLER    PIC X(01)    VALUE  '|'.
 000200                10  FILLER    PIC X(13)    VALUE SPACES.
 000201                10  FILLER    PIC X(8)    VALUE 'APELLIDO'.
 000202                10  FILLER    PIC X(13)    VALUE SPACES.
 000203                10  FILLER    PIC X(01)    VALUE  '|'.
 000204                10  FILLER    PIC X(2)    VALUE SPACES.
 000205                10  FILLER    PIC X(11)    VALUE 'NRO CLIENTE'.
 000206                10  FILLER    PIC X(2)    VALUE SPACES.
 000207                10  FILLER    PIC X(01)    VALUE  '|'.
 000208                10  FILLER    PIC X(2)    VALUE SPACES.
 000209                10  FILLER    PIC X(14)    VALUE 'TIPO DOCUMENTO'.
 000210                10  FILLER    PIC X(2)    VALUE SPACES.
 000211                10  FILLER    PIC X(01)    VALUE  '|'.
 000212                10  FILLER    PIC X(2)    VALUE SPACES.
 000213                10  FILLER    PIC X(13)    VALUE 'NRO DOCUMENTO'.
 000214                10  FILLER    PIC X(2)    VALUE SPACES.
 000215                10  FILLER    PIC X(01)    VALUE  '|'.
 000216                10  FILLER    PIC X(2)    VALUE SPACES.
 000217                10  FILLER    PIC X(10)    VALUE 'FECHA ALTA'.
 000218                10  FILLER    PIC X(2)    VALUE SPACES.
 000219                10  FILLER    PIC X(01)    VALUE  '|'.
 000220                10  FILLER    PIC X(2)    VALUE SPACES.
 000221                10  FILLER    PIC X(10)    VALUE 'FECHA BAJA'.
 000222                10  FILLER    PIC X(2)    VALUE SPACES.
 000223                10  FILLER    PIC X(01)    VALUE  '|'.
 000224            05  IP-CUENTA.
 000225                10  FILLER    PIC X(2)    VALUE SPACES.
 000226                10  FILLER    PIC X(11)    VALUE 'TIPO CUENTA'.
 000227                10  FILLER    PIC X(2)    VALUE SPACES.
 000228                10  FILLER    PIC X(01)    VALUE  '|'.
 000229                10  FILLER    PIC X(4)    VALUE SPACES.
 000230                10  FILLER    PIC X(10)    VALUE 'NRO CUENTA'.
 000231                10  FILLER    PIC X(5)    VALUE SPACES.
 000232                10  FILLER    PIC X(01)    VALUE  '|'.
 000233                10  FILLER    PIC X(2)    VALUE SPACES.
 000234                10  FILLER    PIC X(6)    VALUE 'MONEDA'.
 000235                10  FILLER    PIC X(2)    VALUE SPACES.
 000236                10  FILLER    PIC X(01)    VALUE  '|'.
 000237                10  FILLER    PIC X(6)    VALUE SPACES.
 000238                10  FILLER    PIC X(3)    VALUE 'CBU'.
 000239                10  FILLER    PIC X(6)    VALUE SPACES.
 000240                10  FILLER    PIC X(01)    VALUE  '|'.
 000241                10  FILLER    PIC X(2)    VALUE SPACES.
 000242                10  FILLER    PIC X(11)    VALUE 'NRO CLIENTE'.
 000243                10  FILLER    PIC X(2)    VALUE SPACES.
 000244                10  FILLER    PIC X(01)    VALUE  '|'.
 000245                10  FILLER    PIC X(4)    VALUE SPACES.
 000246                10  FILLER    PIC X(12)    VALUE 'SALDO ACTUAL'.
 000247                10  FILLER    PIC X(5)    VALUE SPACES.
 000248                10  FILLER    PIC X(01)    VALUE '|'.
 000249                10  FILLER    PIC X(2)    VALUE SPACES.
 000250                10  FILLER    PIC X(12)    VALUE 'FECHA ACTUAL'.
 000251                10  FILLER    PIC X(2)    VALUE SPACES.
 000252                10  FILLER    PIC X(01)    VALUE  '|'.
 000253                10  FILLER    PIC X(2)    VALUE SPACES.
 000254                10  FILLER    PIC X(19)    VALUE 'FECHA ULTIMO CIERRE'.
 000255                10  FILLER    PIC X(2)    VALUE SPACES.
 000256                10  FILLER    PIC X(01)    VALUE  '|'.
 000257       ************************************
 000258       *             SQL CODE             *
 000259       ************************************
 000260        77  FS-SQLCODE               PIC -999          VALUE ZEROS.
 000261       ************************************
 000262       *              SQL                 *
 000263       ************************************
 000264             EXEC SQL
 000265               INCLUDE SQLCA
 000266             END-EXEC.
 000267       *
 000268             EXEC SQL
 000269               INCLUDE TBCUENT
 000270             END-EXEC.
 000271       *
 000272             EXEC SQL
 000273               DECLARE CUENTA_CURSOR CURSOR
 000274               FOR
 000275                SELECT TIPO_CUENTA, NRO_CUENTA,
 000276                       MONEDA, CBU, NRO_CLIENTE,
 000277                       SALDO_ACTUAL, FECHA_ACTUAL,
 000278                       FECHA_ULTIMO_CIERRE
 000279       *
 000280                       FROM ITPFBIO.TBCUENTAS
 000281       *
 000282                       WHERE NRO_CLIENTE = :CLI-NRO-CLIENTE
 000283             END-EXEC.
 000284       **************************************
 000285        PROCEDURE DIVISION.
 000286       **************************************
 000287       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000288       **************************************
 000289        MAIN-PROGRAM.
 000290            PERFORM 1000-I-INICIO
 000291               THRU 1000-F-INICIO
 000292       *
 000293            PERFORM 2000-I-PROCESO
 000294               THRU 2000-F-PROCESO
 000295               UNTIL FS-CLIENTE-EOF
 000296       *
 000297            PERFORM 9999-I-FINAL
 000298               THRU 9999-F-FINAL
 000299            .
 000300        F-MAIN-PROGRAM. GOBACK.
 000301       **************************************
 000302       *  CUERPO INICIO APERTURA ARCHIVOS   *
 000303       **************************************
 000304        1000-I-INICIO.
 000305 TIERRA     ACCEPT WS-NRO-ALU FROM SYSIN
 000306       *
 000307            ADD 1 TO WS-ALU
 000308       *
 000309            SUBTRACT 1 FROM WS-ALU
 000310       *
 000311            PERFORM 7000-I-FECHA
 000312               THRU 7000-F-FECHA
 000313       *
 000314            OPEN INPUT CLIENTE
 000315       *
 000316            IF NOT FS-CLIENTE-OK
 000317               DISPLAY '* ERROR EN OPEN CLIENTE = ' FS-CLIENTE
 000318               MOVE 9999 TO RETURN-CODE
 000319               SET  FS-CLIENTE-EOF TO TRUE
 000320            END-IF
 000321       *
 000322            OPEN OUTPUT SALIDA
 000323       *
 000324            IF NOT FS-SALIDA-OK
 000325               DISPLAY '* ERROR EN OPEN SALIDA = ' FS-SALIDA
 000326               MOVE 9999 TO RETURN-CODE
 000327               SET  FS-CLIENTE-EOF TO TRUE
 000328            END-IF
 000329       *
 000330            PERFORM 9000-I-TITULO
 000331               THRU 9000-F-TITULO
 000332       *
 000333            PERFORM 3000-I-LEER-CLIENTE
 000334               THRU 3000-F-LEER-CLIENTE
 000335            .
 000336        1000-F-INICIO. EXIT.
 000337       **************************************
 000338       *  CUERPO PRINCIPAL DE PROCESOS      *
 000339       **************************************
 000340        2000-I-PROCESO.
 000341            PERFORM 2500-I-OPEN-CUENTA
 000342               THRU 2500-F-OPEN-CUENTA
 000343       *
 000344            PERFORM 8500-I-SEP-CUENTA
 000345               THRU 8500-F-SEP-CUENTA
 000346       *
 000347            PERFORM 8200-I-LAYOUT-CUENTA
 000348               THRU 8200-F-LAYOUT-CUENTA
 000349       *
 000350            PERFORM 3500-I-LEER-CUENTA
 000351               THRU 3500-F-LEER-CUENTA
 000352               UNTIL FS-CUENTA-EOF
 000353       *
 000354            PERFORM 6500-I-CLOSE-CUENTA
 000355               THRU 6500-F-CLOSE-CUENTA
 000356       *
 000357            PERFORM 3000-I-LEER-CLIENTE
 000358               THRU 3000-F-LEER-CLIENTE
 000359       *
 000360            SET FS-CUENTA-OK TO TRUE
 000361            .
 000362        2000-F-PROCESO. EXIT.
 000363       **************************************
 000364       *         OPEN CURSOR CUENTA         *
 000365       **************************************
 000366        2500-I-OPEN-CUENTA.
 000367            EXEC SQL
 000368               OPEN CUENTA_CURSOR
 000369            END-EXEC
 000370       *
 000371            IF SQLCODE NOT EQUAL ZEROS
 000372               MOVE SQLCODE   TO FS-SQLCODE
 000373               DISPLAY '* ERROR OPEN CURSOR CUENTA    = ' FS-SQLCODE
 000374               MOVE 9999 TO RETURN-CODE
 000375               SET  FS-CLIENTE-EOF TO TRUE
 000376            ELSE
 000377               INITIALIZE CN-CUENTAS
 000378            END-IF
 000379            .
 000380        2500-F-OPEN-CUENTA. EXIT.
 000381       **************************************
 000382       *            LEER CLIENTE            *
 000383       **************************************
 000384        3000-I-LEER-CLIENTE.
 000385            DISPLAY 'INICIO DE LEER CLIENTE'
 000386            READ CLIENTE INTO WS-CLIENTE
 000387       *
 000388            EVALUATE TRUE
 000389       *
 000390            WHEN FS-CLIENTE-OK
 000391               ADD 1 TO CN-NOVEDADES-FD
 000392       *
 000393               PERFORM 8000-I-SEP-CLIENTE
 000394                  THRU 8000-F-SEP-CLIENTE
 000395       *
 000396               PERFORM 8100-I-LAYOUT-CLIENTE
 000397                  THRU 8100-F-LAYOUT-CLIENTE
 000398       *
 000399            WHEN FS-CLIENTE-EOF
 000400                CONTINUE
 000401       *
 000402            WHEN OTHER
 000403                ADD 1 TO CN-NOVEDADES-ER
 000404                DISPLAY '* ERROR EN LECTURA CLIENTE  ' FS-CLIENTE
 000405                MOVE 9999 TO RETURN-CODE
 000406                SET FS-CLIENTE-EOF TO TRUE
 000407
 000408            END-EVALUATE
 000409            DISPLAY 'FIN DE LEER CLIENTE'
 000410            .
 000411        3000-F-LEER-CLIENTE. EXIT.
 000412       **************************************
 000413       *            LEER CUENTA             *
 000414       **************************************
 000415        3500-I-LEER-CUENTA.
 000416            DISPLAY 'INICIO DE LEER CUENTA'
 000417            EXEC SQL
 000418                 FETCH CUENTA_CURSOR
 000419                 INTO  :DCLCUEN.TIPO-CUENTA,
 000420                       :DCLCUEN.NRO-CUENTA,
 000421                       :DCLCUEN.MONEDA,
 000422                       :DCLCUEN.CBU,
 000423                       :DCLCUEN.CUE-NRO-CLIENTE,
 000424                       :DCLCUEN.SALDO-ACTUAL,
 000425                       :DCLCUEN.FECHA-ACTUAL,
 000426                       :DCLCUEN.FECHA-ULTIMO-CIERRE
 000427            END-EXEC
 000428       *
 000429            EVALUATE SQLCODE
 000430       *
 000431            WHEN ZEROS
 000432               ADD 1 TO CN-NOVEDADES-FD
 000433               PERFORM 5500-I-GRABAR-CUENTA
 000434                  THRU 5500-F-GRABAR-CUENTA
 000435       *
 000436               ADD 1 TO CN-CUENTAS
 000437       *
 000438            WHEN +100
 000439               SET FS-CUENTA-EOF TO TRUE
 000440       *
 000441               IF CN-CUENTAS EQUAL TO ZEROS
 000442                 WRITE REG-SALIDA  FROM IP-NO-CUENTAS
 000443       *
 000444                 IF NOT FS-SALIDA-OK
 000445                   DISPLAY '* ERROR EN GRABAR NO CUENTAS = ' FS-SALIDA
 000446                   MOVE 9999 TO RETURN-CODE
 000447                   SET  FS-CLIENTE-EOF TO TRUE
 000448                 ELSE
 000449                   ADD 1 TO CN-CUENTA-LINEA
 000450                 END-IF
 000451
 000452               END-IF
 000453       *
 000454            WHEN OTHER
 000455               MOVE SQLCODE   TO FS-SQLCODE
 000456               DISPLAY '* ERROR LEER CURSOR CUENTA = ' FS-SQLCODE
 000457               MOVE 9999 TO RETURN-CODE
 000458               SET FS-CLIENTE-EOF TO TRUE
 000459            END-EVALUATE
 000460            DISPLAY 'FIN DE LEER CUENTA'
 000461            .
 000462        3500-F-LEER-CUENTA. EXIT.
 000463       **************************************
 000464       *           MOVER CLIENTE            *
 000465       **************************************
 000466        4000-I-MOVER-CLIENTE.
 000467       *
 000468            MOVE TIPO-DOCUMENTO            TO WS-CLI-TIP-DOC
 000469       *
 000470            MOVE NRO-DOCUMENTO             TO WS-CLI-NRO-DOC
 000471       *
 000472            MOVE CLI-NRO-CLIENTE           TO WS-CLI-NRO-CLI
 000473       *
 000474            MOVE NOMBRE-CLIENTE            TO WS-CLI-NOM
 000475       *
 000476            MOVE APELLIDO-CLIENTE          TO WS-CLI-APE
 000477       *
 000478            MOVE FECHA-DE-ALTA             TO WS-CLI-FECHA-ALTA
 000479       *
 000480            MOVE FECHA-DE-BAJA             TO WS-CLI-FECHA-BAJA
 000481            .
 000482        4000-F-MOVER-CLIENTE. EXIT.
 000483       **************************************
 000484       *           MOVER CUENTA             *
 000485       **************************************
 000486        4500-I-MOVER-CUENTA.
 000487       *
 000488            MOVE TIPO-CUENTA               TO WS-CUE-TIP-CUE
 000489       *
 000490            MOVE NRO-CUENTA                TO WS-CUE-NRO-CUE
 000491       *
 000492            MOVE MONEDA                    TO WS-CUE-MONEDA
 000493       *
 000494            MOVE CBU                       TO WS-CUE-CBU
 000495       *
 000496            MOVE CUE-NRO-CLIENTE           TO WS-CUE-NRO-CLI
 000497       *
 000498            MOVE SALDO-ACTUAL              TO WS-CUE-SALDO-ACT
 000499       *
 000500            MOVE FECHA-ACTUAL              TO WS-CUE-FECHA-ACT
 000501       *
 000502            MOVE FECHA-ULTIMO-CIERRE       TO WS-CUE-FECHA-ULT
 000503            .
 000504        4500-F-MOVER-CUENTA. EXIT.
 000505       **************************************
 000506       *             GRABAR CLIENTE         *
 000507       **************************************
 000508        5000-I-GRABAR-CLIENTE.
 000509            IF CN-CUENTA-LINEA GREATER 60
 000510                PERFORM 9000-I-TITULO THRU 9000-F-TITULO
 000511            END-IF
 000512       *
 000513            PERFORM 4000-I-MOVER-CLIENTE
 000514               THRU 4000-F-MOVER-CLIENTE
 000515       *
 000516            WRITE REG-SALIDA   FROM WS-REG-CLIENTE
 000517       *
 000518            IF NOT FS-SALIDA-OK
 000519              DISPLAY '* ERROR EN GRABAR SALIDA CLIENTE = ' FS-SALIDA
 000520              MOVE 9999 TO RETURN-CODE
 000521              SET  FS-CLIENTE-EOF TO TRUE
 000522            ELSE
 000523              ADD 1                         TO  CN-NOVEDADES-GRABADAS
 000524              ADD 1                         TO CN-CUENTA-LINEA
 000525            END-IF
 000526            .
 000527        5000-F-GRABAR-CLIENTE. EXIT.
 000528       **************************************
 000529       *             GRABAR CUENTA          *
 000530       **************************************
 000531        5500-I-GRABAR-CUENTA.
 000532            IF CN-CUENTA-LINEA GREATER 60
 000533                PERFORM 9000-I-TITULO THRU 9000-F-TITULO
 000534            END-IF
 000535       *
 000536            PERFORM 4500-I-MOVER-CUENTA
 000537               THRU 4500-F-MOVER-CUENTA
 000538       *
 000539            WRITE REG-SALIDA   FROM WS-REG-CUENTA
 000540       *
 000541            IF NOT FS-SALIDA-OK
 000542              DISPLAY '* ERROR EN GRABAR SALIDA CUENTA = ' FS-SALIDA
 000543              MOVE 9999 TO RETURN-CODE
 000544              SET  FS-CLIENTE-EOF TO TRUE
 000545            ELSE
 000546              ADD 1                         TO  CN-NOVEDADES-GRABADAS
 000547              ADD 1                         TO CN-CUENTA-LINEA
 000548            END-IF
 000549            .
 000550        5500-F-GRABAR-CUENTA. EXIT.
 000551       **************************************
 000552       *         CLOSE CURSOR CUENTA        *
 000553       **************************************
 000554        6500-I-CLOSE-CUENTA.
 000555            EXEC SQL
 000556               CLOSE CUENTA_CURSOR
 000557            END-EXEC
 000558       *
 000559            IF SQLCODE NOT EQUAL ZEROS
 000560               MOVE SQLCODE TO FS-SQLCODE
 000561               DISPLAY '* ERROR CLOSE CURSOR CUENTA = ' FS-SQLCODE
 000562               MOVE 9999 TO RETURN-CODE
 000563            END-IF
 000564            .
 000565        6500-F-CLOSE-CUENTA. EXIT.
 000566       **************************************
 000567       *           TOMAR FECHA              *
 000568       **************************************
 000569        7000-I-FECHA.
 000570            ACCEPT WS-AREA FROM DATE YYYYMMDD
 000571       *
 000572            MOVE WS-AREA-AA TO WS-FECHA-AA
 000573       *
 000574            MOVE WS-AREA-MM TO WS-FECHA-MM
 000575       *
 000576            MOVE WS-AREA-DD TO WS-FECHA-DD
 000577            .
 000578        7000-F-FECHA. EXIT.
 000579       **************************************
 000580       *          SEPARADOR CLIENTE         *
 000581       **************************************
 000582        8000-I-SEP-CLIENTE.
 000583            WRITE REG-SALIDA FROM IP-BARRA
 000584       *
 000585            IF NOT FS-SALIDA-OK
 000586              DISPLAY '* ERROR EN GRABAR SEPARADOR BARRA = ' FS-SALIDA
 000587              MOVE 9999 TO RETURN-CODE
 000588              SET  FS-CLIENTE-EOF TO TRUE
 000589            ELSE
 000590              ADD 1 TO CN-CUENTA-LINEA
 000591            END-IF
 000592       *
 000593            WRITE REG-SALIDA FROM IP-INTRODUCCION-CLIENTE
 000594       *
 000595            IF NOT FS-SALIDA-OK
 000596              DISPLAY '* ERROR EN GRABAR SEPARADOR CLIENTE = ' FS-SALIDA
 000597              MOVE 9999 TO RETURN-CODE
 000598              SET  FS-CLIENTE-EOF TO TRUE
 000599            ELSE
 000600              ADD 1 TO CN-CUENTA-LINEA
 000601            END-IF
 000602            .
 000603        8000-F-SEP-CLIENTE. EXIT.
 000604       **************************************
 000605       *          LAYOUT CLIENTE            *
 000606       **************************************
 000607        8100-I-LAYOUT-CLIENTE.
 000608            WRITE REG-SALIDA FROM IP-CLIENTE
 000609       *
 000610            IF NOT FS-SALIDA-OK
 000611              DISPLAY '* ERROR EN GRABAR IPCLIENTE = ' FS-SALIDA
 000612              MOVE 9999 TO RETURN-CODE
 000613              SET  FS-CLIENTE-EOF TO TRUE
 000614            ELSE
 000615              ADD 1                         TO CN-CUENTA-LINEA
 000616              PERFORM  5000-I-GRABAR-CLIENTE
 000617                 THRU  5000-F-GRABAR-CLIENTE
 000618            END-IF
 000619            .
 000620        8100-F-LAYOUT-CLIENTE. EXIT.
 000621       **************************************
 000622       *          LAYOUT CUENTA             *
 000623       **************************************
 000624        8200-I-LAYOUT-CUENTA.
 000625            WRITE REG-SALIDA FROM IP-CUENTA
 000626       *
 000627            IF NOT FS-SALIDA-OK
 000628              DISPLAY '* ERROR EN GRABAR IPCUENTA = ' FS-SALIDA
 000629              MOVE 9999 TO RETURN-CODE
 000630              SET  FS-CLIENTE-EOF TO TRUE
 000631            ELSE
 000632              ADD 1                         TO CN-CUENTA-LINEA
 000633            END-IF
 000634            .
 000635        8200-F-LAYOUT-CUENTA. EXIT.
 000636       **************************************
 000637       *          SEPARADOR CUENTA          *
 000638       **************************************
 000639        8500-I-SEP-CUENTA.
 000640            WRITE REG-SALIDA FROM IP-INTRODUCCION-CUENTA
 000641       *
 000642            IF NOT FS-SALIDA-OK
 000643              DISPLAY '* ERROR EN GRABAR SEPARADOR CUENTA = ' FS-SALIDA
 000644              MOVE 9999 TO RETURN-CODE
 000645              SET  FS-CLIENTE-EOF TO TRUE
 000646            ELSE
 000647              ADD 1 TO CN-CUENTA-LINEA
 000648            END-IF
 000649            .
 000650        8500-F-SEP-CUENTA. EXIT.
 000651       **************************************
 000652       *           GRABAR TITULO            *
 000653       **************************************
 000654        9000-I-TITULO.
 000655            MOVE WS-FECHA TO IP-FECHA
 000656       *
 000657            MOVE WS-ALU   TO IP-ALU
 000658       *
 000659            WRITE  REG-SALIDA    FROM IP-TITULO AFTER PAGE
 000660       *
 000661            IF NOT FS-SALIDA-OK
 000662               DISPLAY '* ERROR EN IMPRIMIR TITULO = ' FS-SALIDA
 000663               MOVE 9999 TO RETURN-CODE
 000664               SET  FS-CLIENTE-EOF TO TRUE
 000665            ELSE
 000666               MOVE 1 TO CN-CUENTA-LINEA
 000667            END-IF
 000668            .
 000669        9000-F-TITULO. EXIT.
 000670       **************************************
 000671       *  CUERPO FINAL CIERRE DE FILES      *
 000672       **************************************
 000673        9999-I-FINAL.
 000674            CLOSE CLIENTE
 000675               IF NOT FS-CLIENTE-OK
 000676                 DISPLAY '* ERROR EN CLOSE CLIENTE =  ' FS-CLIENTE
 000677                 MOVE 9999 TO RETURN-CODE
 000678                 SET FS-CLIENTE-EOF TO TRUE
 000679               END-IF
 000680       *
 000681            CLOSE SALIDA
 000682               IF NOT FS-SALIDA-OK
 000683                 DISPLAY '* ERROR EN CLOSE SALIDA =  ' FS-SALIDA
 000684                 MOVE 9999 TO RETURN-CODE
 000685                 SET FS-SALIDA-EOF TO TRUE
 000686               END-IF
 000687       *
 000688            DISPLAY 'NOVEDADES ENCONTRADAS: ' CN-NOVEDADES-FD
 000689            DISPLAY 'NOVEDADES NO ENCONTRADAS: ' CN-NOVEDADES-NFD
 000690            DISPLAY 'NOVEDADES ERRONEAS: ' CN-NOVEDADES-ER
 000691            DISPLAY 'REGISTROS GRABADOS: ' CN-NOVEDADES-GRABADAS
 000692            .
 000693        9999-F-FINAL. EXIT.























 Command ===>                                                  Scroll ===> CSR
  F1=Help      F2=Split     F3=Exit      F5=Rfind     F6=Rchange   F7=Up
  F8=Down      F9=Swap     F10=Left     F11=Right    F12=Cancel
 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .
   -EDIT    *EDIT

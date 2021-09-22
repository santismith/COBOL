      ******************************************************************
      * ESQUELETO CORTE DE CONTROL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORTCTRL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO "CUENTAS.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA.

           SELECT SALIDA ASSIGN TO "salidacuentas.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA.
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA.
           COPY CPY001.

       FD  SALIDA.
           01 REG-SALIDA PIC X(150).

       WORKING-STORAGE SECTION.

       01  ESTRUCTURA-SALIDA.
           05 TEXTO1                       PIC X(40).
           05 TOTALES1                     PIC 9(9).

       01  ESTRUCTURA-DATOS-ENTRADA.
           05 PAIS                         PIC X(03).
           05 FILLER                       PIC X(1) VALUE "|".
           05 SUCURSAL                     PIC 9(02).
           05 FILLER                       PIC X(1) VALUE "|".
           05 TIPO-CTA                     PIC X(02).
           05 FILLER                       PIC X(1) VALUE "|".
           05 CUENTA                       PIC 9(03).
           05 FILLER                       PIC X(1) VALUE "|".
           05 SALDO                        PIC 9(07).

       01  VARIABLES.
           05 FS-ENTRADA               PIC XX.
               88 FS-ENTRADA-OK        VALUE "00".
               88 FS-ENTRADA-FIN       VALUE "10".

           05 FS-SALIDA                PIC XX.
               88 FS-SALIDA-OK         VALUE "00".
               88 FS-SALIDA-FIN        VALUE "10".

           05 WS-PAIS-ANT                  PIC X(03).
           05 WS-SUC-ANT                   PIC X(03).
           05 WS-TIPCTA-ANT                PIC X(02).
           05 WS-SALDO-ANT                 PIC X(03).
           05 WS-TOTAL-CTASXPAIS           PIC 9(9).
           05 WS-TOTAL-CTASXSUC            PIC 9(9).
           05 WS-TOTAL-SALDOXPAIS          PIC 9(9).
           05 WS-TOTAL-SALDOXSUC           PIC 9(9).
       77  WS-CONT-LECTURA                 PIC 99.

       PROCEDURE DIVISION.
           PERFORM 1000-I-INICIO
           PERFORM 2000-I-PROCESO UNTIL FS-ENTRADA-FIN.
           PERFORM 9000-I-FINAL.
               STOP RUN.
      *----------------------------------------------------------------*
       1000-I-INICIO.
           INITIALIZE VARIABLES
           PERFORM 1100-ABRIRENTRADA.
           PERFORM 1150-ABRIRSALIDA.
           PERFORM 5000-LEERENTRADA.
       1000-F-INICIO.EXIT.
      *----------------------------------------------------------------*
       1100-ABRIRENTRADA.
           OPEN INPUT ENTRADA.

           IF NOT FS-ENTRADA-OK
               DISPLAY "ERROR APERTURA ENTRADA FS: " FS-ENTRADA
               PERFORM 9000-I-FINAL
           END-IF.
       1100-F-ABRIRENTRADA.EXIT.
      *----------------------------------------------------------------*
       1150-ABRIRSALIDA.
           OPEN OUTPUT SALIDA.

           IF NOT FS-SALIDA-OK
               DISPLAY "ERROR APERTURA SALIDA FS: " FS-SALIDA
               PERFORM 9000-I-FINAL
           END-IF.
       1150-F-ABRIRSALIDA.EXIT.
      *----------------------------------------------------------------*
       2000-I-PROCESO.
           MOVE CTA-PAIS TO WS-PAIS-ANT
           INITIALIZE WS-TOTAL-CTASXPAIS
           INITIALIZE WS-TOTAL-SALDOXPAIS

           PERFORM 2100-I-CORTE-CONTROL-PAIS
           UNTIL CTA-PAIS <> WS-PAIS-ANT OR FS-ENTRADA-FIN

           PERFORM 3000-TOTALIZAR-CTASXPAIS
           PERFORM 3001-TOTALIZAR-SALDOXPAIS.
       2000-F-PROCESO.EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-I-CORTE-CONTROL-PAIS.
           MOVE CTA-SUCURSAL TO WS-SUC-ANT
           INITIALIZE WS-TOTAL-CTASXSUC
           INITIALIZE WS-TOTAL-SALDOXSUC

           PERFORM 2200-I-CORTE-CONTROL-SALDO
           UNTIL CTA-SUCURSAL<> WS-SUC-ANT
           OR CTA-PAIS <> WS-PAIS-ANT
           OR FS-ENTRADA-FIN.

           PERFORM 3002-TOTALIZAR-CTASXSUCURSAL
           PERFORM 3003-TOTALIZAR-SALDOXSUCURSAL.
       2100-F-CORTE-CONTROL-SUCURSAL.EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-I-CORTE-CONTROL-SALDO.
           ADD 1 TO WS-TOTAL-CTASXPAIS WS-TOTAL-CTASXSUC.
           ADD CTA-SALDO TO WS-TOTAL-SALDOXPAIS WS-TOTAL-SALDOXSUC

           MOVE CTA-PAIS TO PAIS
           MOVE CTA-SUCURSAL TO SUCURSAL
           MOVE CTA-TIPO-CTA TO TIPO-CTA
           MOVE CTA-CUENTA TO CUENTA
           MOVE CTA-SALDO TO SALDO

           WRITE REG-SALIDA FROM ESTRUCTURA-DATOS-ENTRADA.
           PERFORM 5000-LEERENTRADA.
       2200-F-CORTE-CONTROL-SALDO.EXIT.
      *----------------------------------------------------------------*
       3000-TOTALIZAR-CTASXPAIS.
           MOVE WS-TOTAL-CTASXPAIS TO TOTALES1
           MOVE "CANT TOTAL DE CUENTAS POR PAIS: " TO TEXTO1
           PERFORM 6000-ESCRIBIRSALIDA.
       3000-F-TOTALIZAR-CTASXPAIS.EXIT.
      *----------------------------------------------------------------*
       3001-TOTALIZAR-SALDOXPAIS.
           MOVE WS-TOTAL-SALDOXPAIS TO TOTALES1
           MOVE "SALDO TOTAL DEL PAIS: " TO TEXTO1
           PERFORM 6000-ESCRIBIRSALIDA.
       3001-F-TOTALIZAR-SALDOXPAIS.EXIT.
      *----------------------------------------------------------------*
       3002-TOTALIZAR-CTASXSUCURSAL.
           MOVE WS-TOTAL-CTASXSUC TO TOTALES1
           MOVE "CANT TOTAL DE CUENTAS POR SUCURSAL: " TO TEXTO1
           PERFORM 6000-ESCRIBIRSALIDA.
       3002-F-TOTALIZAR-CTASXSUCURSAL.EXIT.
      *----------------------------------------------------------------*
       3003-TOTALIZAR-SALDOXSUCURSAL.
           MOVE WS-TOTAL-SALDOXSUC TO TOTALES1
           MOVE "SALDO POR SUCURSAL: " TO TEXTO1
           PERFORM 6000-ESCRIBIRSALIDA.
       3003-F-TOTALIZAR-SALDOXSUCURSAL.EXIT.
      *----------------------------------------------------------------*
       5000-LEERENTRADA.
           READ ENTRADA
           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                   ADD 1 TO WS-CONT-LECTURA
               WHEN FS-ENTRADA-FIN
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR LECTURA ENTRADA"
           END-EVALUATE.
       5000-F-LEERENTRADA.EXIT.
      *----------------------------------------------------------------*
       6000-ESCRIBIRSALIDA.
           IF NOT FS-SALIDA-OK
               DISPLAY "ERROR ESCRITURA SALIDA FS: " FS-SALIDA
           ELSE
               WRITE REG-SALIDA FROM ESTRUCTURA-SALIDA
           END-IF.
       6000-F-ESCRIBIRSALIDA.
      *----------------------------------------------------------------*
       9000-I-FINAL.
           MOVE " TOTAL REGISTROS LEIDOS " TO TEXTO1
           MOVE WS-CONT-LECTURA TO TOTALES1
           WRITE REG-SALIDA FROM ESTRUCTURA-SALIDA
           CLOSE ENTRADA.
           CLOSE SALIDA.
       9000-F-FINAL.
           EXIT.
      *----------------------------------------------------------------*

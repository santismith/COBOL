       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORTCTRL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO1 ASSIGN TO "PAISHAB.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA1.

           SELECT ARCHIVO2 ASSIGN TO "PAISLAT.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-ENTRADA2.

           SELECT SALIDA ASSIGN TO "SALIDA.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS FS-SALIDA.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO1.
           01 PAIS-HABITANTES.
            05 ARCHIVO1-COD-PAIS           PIC X(3).
            05 ARCHIVO1-HABITANTES         PIC 9(9).

       FD ARCHIVO2.
           01 PAIS-DESCRIPCION.
               05 ARCHIVO2-COD-PAIS        PIC X(3).
               05 ARCHIVO2-DES-PAIS        PIC X(40).


       FD  SALIDA.
           01 ARCHIVO3.
               05 ARCHIVO3-PAIS            PIC X(40).
               05 ARCHIVO3-HABITANTES      PIC 9(9).

       WORKING-STORAGE SECTION.

       01  ESTRUCTURA-SALIDA.
           05 PAIS                         PIC X(40).
           05 HABITANTES                   PIC 9(9).

       01  VARIABLES.
           05 FS-ENTRADA1                  PIC XX.
               88 FS-ENTRADA1-OK           VALUE "00".
               88 FS-ENTRADA1-FIN          VALUE "10".

           05 FS-ENTRADA2                  PIC XX.
               88 FS-ENTRADA2-OK           VALUE "00".
               88 FS-ENTRADA2-FIN          VALUE "10".

           05 FS-SALIDA                    PIC XX.
               88 FS-SALIDA-OK             VALUE "00".
               88 FS-SALIDA-FIN            VALUE "10".

       77  WS-CONT-LECTURA                PIC 99.

       PROCEDURE DIVISION.
           PERFORM 1000-I-INICIO
           PERFORM 2000-I-PROCESO UNTIL FS-ENTRADA1-FIN
           AND FS-ENTRADA2-FIN.
           PERFORM 9000-I-FINAL.
               STOP RUN.
      *----------------------------------------------------------------*
       1000-I-INICIO.
           INITIALIZE VARIABLES
           PERFORM 1100-ABRIRENTRADAS.
           PERFORM 1150-ABRIRSALIDA.
           PERFORM 5000-LEERARCHIVO1.
           PERFORM 5001-LEERARCHIVO2.
       1000-F-INICIO.EXIT.
      *----------------------------------------------------------------*
       1100-ABRIRENTRADAS.
           OPEN INPUT ARCHIVO1.

           IF NOT FS-ENTRADA1-OK
               DISPLAY "ERROR APERTURA ARCHIVO1-HABITANTES FS: "
               FS-ENTRADA1
               PERFORM 9000-I-FINAL
           END-IF.

           OPEN INPUT ARCHIVO2.

           IF NOT FS-ENTRADA2-OK
               DISPLAY "ERROR APERTURA ARCHIVO 2 FS: " FS-ENTRADA2
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

           IF ARCHIVO1-COD-PAIS > ARCHIVO2-COD-PAIS
               PERFORM 5001-LEERARCHIVO2
           ELSE IF ARCHIVO1-COD-PAIS < ARCHIVO2-COD-PAIS
               PERFORM 5000-LEERARCHIVO1
           ELSE IF ARCHIVO1-COD-PAIS = ARCHIVO2-COD-PAIS
               MOVE ARCHIVO1-HABITANTES TO HABITANTES
               MOVE ARCHIVO2-DES-PAIS   TO PAIS
               PERFORM 4000-ESCRIBIR-SALIDA
               PERFORM 5000-LEERARCHIVO1
           END-IF.

       2000-F-PROCESO.
           EXIT.
      *----------------------------------------------------------------*
       4000-ESCRIBIR-SALIDA.
           IF NOT FS-SALIDA-OK
               DISPLAY "ERROR ESCRITURA SALIDA FS: " FS-SALIDA
           ELSE
               WRITE ARCHIVO3 FROM ESTRUCTURA-SALIDA
           END-IF.
       4000-F-ESCRIBIR-SALIDA.
      *----------------------------------------------------------------*
       5000-LEERARCHIVO1.
           READ ARCHIVO1
           EVALUATE TRUE
               WHEN FS-ENTRADA1-OK
                   ADD 1 TO WS-CONT-LECTURA
               WHEN FS-ENTRADA1-FIN
                   MOVE HIGH-VALUES TO ARCHIVO1-COD-PAIS
               WHEN OTHER
                   DISPLAY "ERROR LECTURA ENTRADA 1 "
                   PERFORM 9000-I-FINAL
           END-EVALUATE.
       5000-F-LEERENTRADA.EXIT.
      *----------------------------------------------------------------*
       5001-LEERARCHIVO2.
           READ ARCHIVO2
           EVALUATE TRUE
               WHEN FS-ENTRADA2-OK
                   ADD 1 TO WS-CONT-LECTURA
               WHEN FS-ENTRADA2-FIN
                   MOVE HIGH-VALUES TO ARCHIVO2-COD-PAIS
               WHEN OTHER
                   DISPLAY "ERROR LECTURA ENTRADA 2 " FS-ENTRADA2
                   PERFORM 9000-I-FINAL
           END-EVALUATE.
       5001-F-LEERENTRADA.EXIT.
      *----------------------------------------------------------------*
       9000-I-FINAL.
           CLOSE ARCHIVO1.
           CLOSE ARCHIVO2.
           CLOSE SALIDA.
           DISPLAY "REGISTROS LEIDOS: " WS-CONT-LECTURA
           STOP RUN.
       9000-F-FINAL.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EX02.
       AUTHOR.        LEVI LIMA BELTRAO.
       DATE-WRITTEN.  23/03/2025.
       DATE-COMPILED. 23/03/2025
      *REMARKS        LE OS REGISTROS NO ARQUIVO
      *               CADALU(NUMERO,NOME,SEXO,
      *               DATA NASCIMENTO) E GRAVA EM
      *               CADATU(NUMERO,NOME,DATA NASCIMENTO).

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  LEVI-PC.
       OBJECT-COMPUTER.  LEVI-PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADATU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
       
       01 REG-ENT.
           02 NUM-ENT     PIC 9(05).
           02 NOME-ENT    PIC X(20).
           02 SEXO-ENT    PIC X(01).
           02 DATA-ENT.
               03 DIA-ENT PIC 9(02).
               03 MES-ENT PIC 9(02).
               03 ANO-ENT PIC 9(04).

       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".

       01 REG-SAI.
           02 NUM-SAI     PIC 9(05).
           02 NOME-SAI    PIC X(20).
           02 DATA-SAI.
               03 DIA-SAI PIC 9(02).
               03 MES-SAI PIC 9(02).
               03 ANO-SAI PIC 9(04).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.
       EXFIX.

       PERFORM INICIO.
       PERFORM PRINCIPAL
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
           OPEN INPUT CADALU
               OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
               AT END
               MOVE "SIM" TO FIM-ARQ.
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE NUM-ENT TO NUM-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE DATA-ENT TO DATA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.
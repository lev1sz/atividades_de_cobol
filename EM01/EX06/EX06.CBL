       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX06.
       AUTHOR.           LEVI LIMA BELTRAO.
       DATE-WRITTEN.     24/03/2025.
       DATE-COMPILED.    24/03/2025.
       SECURITY.         APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.          LE OS REGISTROS NO ARQUIVO CADENT
      *                  (MATRICULA, NOME, SALARIO BRUTO)
      *                  E GRAVA NO ARQUIVO CADSAI (MATRICULA,
      *                  NOME, SALARIO BRUTO) APENAS AQUELES
      *                  COM SALARIO ACIMA DE 3000.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   LEVI-PC.
       OBJECT-COMPUTER.   LEVI-PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADENT ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADENT.DAT".

       01 REG-ENT.
           02 MAT-ENT      PIC 9(05).
           02 NOME-ENT     PIC X(30).
           02 SAL-ENT      PIC 9(05)V99.
       
       FD CADSAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".

       01 REG-SAI.
           02 MAT-SAI      PIC 9(05).
           02 NOME-SAI     PIC X(30).
           02 SAL-SAI      PIC 9(05)V99.
        
       WORKING-STORAGE SECTION.
       77 FIM-ARQ                 PIC X(03)       VALUE IS "NAO".
       77 SAL-NUM                 PIC 9(07).

       PROCEDURE DIVISION.
       EXFIX.
       
       PERFORM INICIO.
       PERFORM PRINCIPAL
           UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
           OPEN INPUT CADENT
               OUTPUT CADSAI.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADENT
               AT END
               MOVE "SIM" TO FIM-ARQ.
       
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
       
       GRAVACAO.
           IF SAL-ENT NOT < 3000 THEN
               MOVE MAT-ENT TO MAT-SAI
               MOVE NOME-ENT TO NOME-SAI
               MOVE SAL-ENT TO SAL-SAI
               WRITE REG-SAI.
                              
       TERMINO.
           CLOSE CADENT
                 CADSAI.
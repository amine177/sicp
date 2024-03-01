; n = 5
; number-bits-most-frequent : 1
; number-bits-least-frequent : 4

'((S1 1) (S2 2) (S3 4) (S4 16) (S5 64))
'(((((leaf S1 1)
     (leaf S2 2)
     (S1 S2)
     3)
    (leaf S3 4)
    (S1 S2 S3)
    7)
   (leaf S4 16)
   (S1 S2 S3 S4)
   23)
  (leaf S8 64)
  (S1 S2 S3 S4 S5)
  87)
 
; n = 10
; number-bits-most-frequent : 1
; number-bits-least-frequent : 10 

'((S1 1) (S2 2) (S3 4) (S4 16)
  (S5 64) (S6 128) (S7 512)
  (S8 1024) (S9 2048) (S10 4056))

'((((((((((leaf S1 1)
     (leaf S2 2)
     (S1 S2)
     3)
    (leaf S3 4)
    (S1 S2 S3)
    7)
   (leaf S4 16)
   (S1 S2 S3 S4)
   23)
  (leaf S8 64)
  (S1 S2 S3 S4 S5)
  87)
  (leaf S6 128)
  (S1 S2 S3 S4 S5 S6) 215)
  (leaf S7 512)
  (S1 S2 S3 S4 S5 S6 S7) 727)
  (leaf S8 1024)
  (S1 S2 S3 S4 S5 S6 S7 S8) 1751)
  (leaf S9 2048)
  (S1 S2 S3 S4 S5 S6 S7 S8 S9) 3799)
  (leaf S10 4056) 7855)

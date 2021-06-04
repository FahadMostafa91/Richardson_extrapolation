!     Last change:  95   21 Jul 2013   11:06 pm
PROGRAM PB45
F(X)=X*EXP(X)
DIMENSION R(100,100)
OPEN(1,FILE='MP.DAT')
OPEN(2,FILE='NP.DAT')
READ(1,*)XO,P,N
R(1,1)=(1.0/(2*P))*(F(XO+P)-F(XO-P))
R(2,1)=((1.0/P)*(F(XO+(P/2))-F(XO-(P/2))))
DO J=3,N
R(J,1)=(1.0/(2*(P/(2**(J-1)))))*(F(XO+(P/(2**(J-1))))-F(XO-(P/(2**(J-1)))))
END DO
DO I=2,N
DO J=2,I
R(I,J)=R(I,J-1)+((R(I,J-1)-R(I-1,J-1))/(4**(J-1)-1))
END DO
END DO
WRITE(2,*)'==============RICHARDSON EXTRAPOLATION METHOD================='
WRITE(2,*)
DO I=1,N
WRITE(2,10)(R(I,J),J=1,I)
10 FORMAT(12(2X,F10.6))
END DO
WRITE(2,*)
WRITE(2,*)'=============FINALLY WE GET THE VALUE OF DERIVATIVE OF F(X) AT DEFINED POINT AS======='
WRITE(2,*)
WRITE(2,*)R(N,N)
END

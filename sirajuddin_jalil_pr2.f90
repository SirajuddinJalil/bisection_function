!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 	
!!	Tugas 2: Program roots of a function	!!
!! 	Last Edited: 30 September 2019		!!
!!	Nama   : Sirajuddin Jalil		!!
!! 	Mata Kuliah : komfis2019		!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program akar_fungsi 
    implicit none
    double precision x1,x2,x3,x4,kecil
    kecil=0.000001d0    !nilai ini adalah batas akurasi
    
    do
        write(*,*) "program ini untuk mengetahui akar dari fungsi y=x^3-2x"
        write(*,*) "masukkan dua batas awal, DUA BATAS AWAL HARUS NEGATIF"
        read(*,*) x1,x2
        if (f(x1)*f(x2) < 0.d0) exit
    end do
    
    call akar(x1,x2,x3,x4)
    
    !ingin menampilkan akar pertama dilayar
    write(*,*) "nilai akarnya yg pertama=" , x3
    !setelah akar pertama diperoleh, dilanjutkan dengan akar yang kedua 
    !nilai batas x1 sekarang ditambah 1
    !nilai batas x2 sekarang ditambah 2
    x1 = x1+1.d0         
    x2 = x2+2.d0         
    call akar(x1,x2,x3,x4)
    write(*,*) "nilai akarnya yg kedua=", x3
    !setelah akar kedua diperoleh, dilanjutkan dengan akar yang ketiga
    x1 = x1+1.d0
    x2 = x2+2.d0
    call akar(x1,x2,x3,x4)
    write(*,*) "nilai akarnya yg ketiga=",x3

    stop
    
contains

subroutine akar(x1,x2,x3,x4)
    implicit none
    double precision x1,x2,x3,x4,kesrel,kecil
    kecil=0.0001d0  
!subroutin untuk mencari akar dengan metode bisection
    do 
    x3=(x1+x2)/2.d0
        if (f(x3) == 0.d0) then
            return
        else if (f(x3)*f(x2) < 0.d0) then
            x1 = x3
        else 
            x2 = x3
        end if
        x4 = (x1+x2)/2.d0
        kesrel = abs((x3-x4)/x4)
        if (kesrel<kecil) exit
        x3=x4
    end do
end subroutine akar

function f(x) result (y)
    implicit none
    double precision y
    double precision, intent(in) :: x
        y=x**3.d0-2.d0*x
    return
end function f

end program akar_fungsi
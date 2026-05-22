program Tesis	
implicit none
integer, parameter :: ip = selected_int_kind(6)
integer, parameter :: rp = selected_real_kind(6, 37) !(33, 4931)
integer(IP), parameter :: Na=50000_ip, Nit=300000000_ip, Npaises=3_ip, Nidiomas=3_ip, Nintereses=100_ip
real(RP), parameter :: L=100_rp, R=0.02_rp*L
integer(IP), parameter :: Nvecinos=500_ip

!Vector agente
integer(IP) :: agente_pais(Na)
real(RP) :: agente_posicion(Na,2)
integer(IP) :: agente_idiomas(Na, Nidiomas), agente_economico(Na), agente_educacion(Na)
real(RP) :: agente_intereses(Na, Nintereses), agente_percepcion(Na, Nvecinos)
real(RP) :: noticia_pais(Npaises)

!constantes
real(RP) :: kid, kec, ked, kin, kper, gamma, influencia_medios, p_migrantes
integer(IP) :: frec_medios

!Para el funcionamiento
real(RP) :: Id_ij(Na,Nvecinos), Ec_ij(Na,Nvecinos), Ed_ij(Na,Nvecinos)
real(RP) :: In_ij, d_ij, A_ij(Nvecinos), sumatoria
real(RP) :: mu, sigma, rn, rn_1
integer :: i, j, k, it, auxi_1, elegido_1, elegido_2, n_vecinos_1, posicion_elegido_2
integer(IP) :: agente_vecinos(Na, Nvecinos), agente_numero_vecinos(Na), vecinos_1(Nvecinos),&
 agente_vecinos_pais(Na,Npaises), memoria_intereses(Nvecinos)
real(RP) :: promedio_percepcion(Npaises),desviacion_percepcion(Npaises), probabilidad(Nvecinos), agente_adaptacion(Na)
integer(IP) :: personas_pais_contacto(Npaises), tipo_medios
logical :: activar_medios

!-----------------------------------CONDICIONES INICIALES---------------------------------

!Parámetros:

!Número de migrantes
p_migrantes=0.15_rp

!Pesos
kid = 0.10_rp 
kec = 0.25_rp
ked = 0.05_rp
kin = 0.50_rp
kper = 0.10_rp

!Aumento de percepción
gamma = 0.3_rp

!Medios de comunicación
activar_medios = .false.
influencia_medios = 0.1_rp
frec_medios = 10000_ip
tipo_medios = 0_ip
!0 Efecto medios con percepción
!1 Efecto medios aleatorio

!Noticia_País: Probabilidad buena noticia
noticia_pais(1) = 0.70_rp	!1 Rico
noticia_pais(2) = 0.40_rp	!2 Medio
noticia_pais(3) = 0.30_rp	!3 Pobre

!!-----------------------------------Posición y País---------------------------------

!Países
    !1 Rico
    !2 Medio
    !3 Pobre
    
do i=1, Na,1
    call random_number(rn)
    agente_posicion(i,1)=rn*L !posición x del agente i
    call random_number(rn)
    agente_posicion(i,2)=rn*L !posición y del agente i
    !País rico
    if (i .le. int(p_migrantes*Na/2)) agente_pais(i) = 1_ip
	!País pobre
	if (i .gt. int(p_migrantes*Na/2) .and. i .le. int(p_migrantes*Na)) agente_pais(i) = 3_ip
	!País medio (receptor)
	if (i .gt. int(p_migrantes*Na)) agente_pais(i) = 2_ip
enddo

agente_vecinos(1:Na, 1:Nvecinos) = 0
agente_numero_vecinos(1:Na) = 0
do i=1, Na, 1
    do j=1, Na, 1
        if (i .eq. j) cycle
        call distancia(agente_posicion(i,1), agente_posicion(j,1), agente_posicion(i,2), agente_posicion(j,2), d_ij)
        if (d_ij .le. R) then
            agente_numero_vecinos(i) = agente_numero_vecinos(i) + 1
            agente_vecinos(i, agente_numero_vecinos(i)) = j
        end if
    end do
end do

agente_vecinos_pais(1:Na, 1:Npaises) = 0
personas_pais_contacto(1:Npaises) = Na
do i=1, Na, 1
	do j=1, agente_numero_vecinos(i), 1
		if (agente_pais(agente_vecinos(i,j)).eq. 1) agente_vecinos_pais(i,1)=agente_vecinos_pais(i,1) + 1
		if (agente_pais(agente_vecinos(i,j)).eq. 2) agente_vecinos_pais(i,2)=agente_vecinos_pais(i,2) + 1
		if (agente_pais(agente_vecinos(i,j)).eq. 3) agente_vecinos_pais(i,3)=agente_vecinos_pais(i,3) + 1
	end do
	if (agente_vecinos_pais(i,1) .eq. 0) personas_pais_contacto(1) = personas_pais_contacto(1) - 1
	if (agente_vecinos_pais(i,2) .eq. 0) personas_pais_contacto(2) = personas_pais_contacto(2) - 1
	if (agente_vecinos_pais(i,3) .eq. 0) personas_pais_contacto(3) = personas_pais_contacto(3) - 1
end do

!-----------------------------------Características---------------------------------
!Idiomas [0:10]
	!1 Español
	!2 Inglés
	!3 Alemán
	
do j=1, Nidiomas, 1
    do i=1, Na, 1
        do
            call leer_valor_mu_sigma_rasgos (agente_pais(i), j, "Nivel_Idioma.txt", mu, sigma)
            call random_number_normal_Box_Muller(mu, sigma, rn)
            agente_idiomas(i,j) = int(rn + 0.5_rp)
            if (agente_idiomas(i,j) .le. 10 .and. agente_idiomas(i,j) .ge. 0) exit
        end do
    end do
end do

!Económico [0:10]

do i=1, Na, 1
    do 
        call leer_valor_mu_sigma (agente_pais(i), "Nivel_Economico.txt", mu, sigma)
        call random_number_normal_Box_Muller(mu, sigma, rn)
        agente_economico(i) = int(rn + 0.5_rp)
        if (agente_economico(i) .le. 10 .and. agente_economico(i) .ge. 0) exit
    end do
end do

!Educación [0:10]

do i=1, Na, 1
    do 
        call leer_valor_mu_sigma (agente_pais(i), "Nivel_Educacion.txt", mu, sigma)
        call random_number_normal_Box_Muller(mu, sigma, rn)
        agente_educacion(i) = int(rn + 0.5_rp)
        if (agente_educacion(i) .le. 10 .and. agente_educacion(i) .ge. 0) exit
    end do
end do

!Intereses [-10,10]

do i=1, Na, 1
    do j=1, Nintereses, 1
        call random_number(rn)
        agente_intereses(i, j) = 10._rp*(2*rn - 1._rp)
    end do
end do

!Percepción[-10,10]

do i=1, Na, 1
    do j=1, agente_numero_vecinos(i), 1
        do 
        	call leer_valor_percepcion_paises(agente_pais(i), agente_pais(agente_vecinos(i, j))&
        	, "Nivel_Percepcion.txt", mu)
        	call random_number_normal_Box_Muller(mu, 2.0_rp, rn)
        	agente_percepcion(i, j) = int(rn + 0.5_rp)
        	if (agente_educacion(i) .le. 10 .and. agente_educacion(i) .ge. -10) exit
    	end do
    end do
end do

!Factores Idiomas, Económico, Educación

do i=1, Na, 1
    n_vecinos_1 = agente_numero_vecinos(i)
    vecinos_1(1:Nvecinos) = agente_vecinos(i, 1:Nvecinos)
    do j=1, n_vecinos_1, 1
        !Factor Idiomas:
        call factor_idiomas_2(Nidiomas, i, vecinos_1(j), agente_idiomas, Id_ij(i,j))
        !Factor Económico:
        call factor_edu_eco_2(i, vecinos_1(j), agente_economico, Ec_ij(i,j))
        !Factor Educación:
        call factor_edu_eco_2(i, vecinos_1(j), agente_educacion, Ed_ij(i,j))
    end do
end do

!-----------------------------------COMENZAMOS A ITERAR-----------------------------------

!Iteración 0:
open(unit=700, file='percepcion_vs_t.dat', status='unknown')
open(unit=801, file='mapa1.dat', status='unknown')
open(unit=802, file='mapa2.dat', status='unknown')
open(unit=806, file='mapa6.dat', status='unknown')

call media_desv_percepcion(agente_numero_vecinos, agente_vecinos, agente_pais, agente_vecinos_pais,&
 personas_pais_contacto, agente_percepcion, promedio_percepcion, desviacion_percepcion)
write(700,*) 0, promedio_percepcion(1:Npaises), desviacion_percepcion(1:Npaises)

!---------------------------------CÁLCULO DE PROBABILIDAD--------------------------------------            
do it=1, Nit, 1
    
    !Hallamos al agente elegido i
    call hallar_elegido(agente_vecinos, agente_numero_vecinos, elegido_1, n_vecinos_1, vecinos_1)
    sumatoria = 0._rp
    do i=1, n_vecinos_1, 1
        call factor_intereses_2(Nintereses, elegido_1, vecinos_1(i), agente_intereses, In_ij,memoria_intereses(i))
        !Factor Aij : A_ij(i)
        A_ij(i) = kid*Id_ij(elegido_1, i) + kec*Ec_ij(elegido_1, i) + ked*Ed_ij(elegido_1, i) &
        + kin*In_ij + kper*agente_percepcion(elegido_1, i)
        !Sumatoria |A_ij(i)|
        sumatoria = sumatoria + abs(A_ij(i))
    end do
    !Probabilidad aij
    probabilidad(1:n_vecinos_1) = abs(A_ij(1:n_vecinos_1))/sumatoria   
    
    !Hallamos al agente vecino j
    call hallar_elegido2(probabilidad, vecinos_1, n_vecinos_1, elegido_2, posicion_elegido_2)
    
!------------------------------------ACTUALIZACIONES-------------------------------------
    
    !Actualizamos Percepción
    agente_percepcion(elegido_1, posicion_elegido_2) = agente_percepcion(elegido_1, posicion_elegido_2)&
    + gamma*A_ij(posicion_elegido_2)
    if (agente_percepcion(elegido_1, posicion_elegido_2) .gt.  10._rp) &
    agente_percepcion(elegido_1, posicion_elegido_2) =  10._rp
    if (agente_percepcion(elegido_1, posicion_elegido_2) .lt. -10._rp) &
    agente_percepcion(elegido_1, posicion_elegido_2) = -10._rp

    !Actualizamos Intereses
    In_ij = agente_intereses(elegido_1,memoria_intereses(posicion_elegido_2))&
    *agente_intereses(elegido_2,memoria_intereses(posicion_elegido_2))/10._rp   
	call actualizar_interes((In_ij), agente_intereses(elegido_1, memoria_intereses(posicion_elegido_2)),&
	agente_intereses(elegido_2,memoria_intereses(posicion_elegido_2)))
    
    !Actualización por Medios de Comunicación
    if (activar_medios .eqv. .true.) then	
        if (mod(it,frec_medios) .eq. 0) then
        	do i=1, Npaises, 1
                call random_number(rn_1)
                if (rn_1 .lt. noticia_pais(i)) then	!Noticia buena
                    call sumar_valor_medios(tipo_medios, influencia_medios, i, agente_pais, agente_numero_vecinos, &
                    agente_vecinos, agente_percepcion)
                else								!Noticia mala
                    call sumar_valor_medios(tipo_medios, -1*influencia_medios, i, agente_pais, agente_numero_vecinos, &
                    agente_vecinos, agente_percepcion)
                end if
            end do
        end if
    end if
!------------------------------------GUARDAR VALORES-------------------------------------    
    
    if (mod(it,2000000) .eq. 0) then  !200000 lo que buscamos
    	!Valores medios percepcion:
        call media_desv_percepcion(agente_numero_vecinos, agente_vecinos, agente_pais,&
        agente_vecinos_pais, personas_pais_contacto, agente_percepcion, promedio_percepcion, desviacion_percepcion)
		write(700,*) it, promedio_percepcion(1:Npaises), desviacion_percepcion(1:Npaises)
    end if
    
    !Guardamos el mapa de la iteración inicial        
	if (it .eq. 1_ip)then
		call  adaptacion(agente_numero_vecinos, agente_vecinos, agente_pais, agente_percepcion, agente_adaptacion)
		do i=1, int(p_migrantes*Na/2), 1
			write(801,*) agente_posicion(i,1), agente_posicion(i,2), agente_adaptacion(i), 1
		end do
		do i=int(p_migrantes*Na/2) + 1, int(p_migrantes*Na), 1
			write(801,*) agente_posicion(i,1), agente_posicion(i,2), agente_adaptacion(i), 3
		end do		
	end if
	
	!Guardamos el mapa de agentes para la iteración 1.3*10**7
	if (it .eq. int(1.3*10**(7)))then
		call  adaptacion(agente_numero_vecinos, agente_vecinos, agente_pais, agente_percepcion, agente_adaptacion)
		do i=1, int(p_migrantes*Na/2), 1
			write(802,*) agente_posicion(i,1), agente_posicion(i,2), agente_adaptacion(i), 1
		end do
		do i=int(p_migrantes*Na/2) + 1, int(p_migrantes*Na), 1
			write(802,*) agente_posicion(i,1), agente_posicion(i,2), agente_adaptacion(i), 3
		end do		
	end if
	
	!Guardamos el mapa de agentes para la iteración Nit
	if (it .eq. Nit)then
		call  adaptacion(agente_numero_vecinos, agente_vecinos, agente_pais, agente_percepcion, agente_adaptacion)
		do i=1, int(p_migrantes*Na/2), 1
			write(806,*) agente_posicion(i,1), agente_posicion(i,2), agente_adaptacion(i), 1
		end do
		do i=int(p_migrantes*Na/2) + 1, int(p_migrantes*Na), 1
			write(806,*) agente_posicion(i,1), agente_posicion(i,2), agente_adaptacion(i), 3
		end do		
	end if
end do

close(700)
close(801)
close(802)
close(806)

contains

subroutine distancia(x1, x2, y1, y2, dis)
    implicit none
    real(RP), intent(in) :: x1, x2, y1, y2
    real(RP), intent(out) :: dis
    dis = sqrt((x2-x1)**2+(y2-y1)**2)
end subroutine distancia

subroutine leer_valor_mu_sigma_rasgos (fila, rasgo, filename, mu, sigma)
    implicit none
    integer(IP), intent(in) :: fila, rasgo
    character(len=*), intent(in) :: filename
    real(RP), intent(out) :: mu, sigma
    real(RP) :: temporal(2*rasgo-2)
    character(20) :: aux
    integer :: i
    open(unit=200, file=filename, status='old', action='read')
    do i=0, fila-1, 1
        read(200, *)   
    end do
    read(200, *) aux, temporal(1:2*rasgo-2), mu, sigma
    close(200)
end subroutine leer_valor_mu_sigma_rasgos

subroutine random_number_normal_Box_Muller(mu, sigma, rn)
    implicit none
    real(RP), intent(in) :: mu, sigma
    real(RP), intent(out) :: rn
    real(RP) :: u1, u2
    
    call random_seed()
    call random_number(u1)
    call random_number(u2)
    
    rn = sqrt(-2.0_rp*log(u1))*cos(2.0_rp*3.14159265*u2)
    rn = mu + sigma*rn
end subroutine random_number_normal_Box_Muller

subroutine leer_valor_mu_sigma (fila, filename, mu, sigma)
    implicit none
    integer(IP), intent(in) :: fila
    character(len=*), intent(in) :: filename
    real(RP), intent(out) :: mu, sigma
    real(RP) :: temporal(2)
    character(20) :: aux
    integer :: i
    open(unit=200, file=filename, status='old', action='read')
    do i=0, fila-1, 1
        read(200, *)   
    end do
    read(200, *) aux, mu, sigma
    close(200)
end subroutine leer_valor_mu_sigma

subroutine leer_valor_percepcion_paises(pais1, pais2, filename, valor)
    implicit none
    integer(IP), intent(in) :: pais1, pais2
    character(len=*), intent(in) :: filename
    real(RP), intent(out) :: valor
    real(RP) :: temporal(Npaises)
    character(20) :: aux
    integer :: i
    open(unit=200, file=filename, status='old', action='read')
    do i=0, pais1-1, 1
        read(200, *)
    end do
    read(200, *) aux, temporal(1:pais2-1), valor
    close(200)
end subroutine leer_valor_percepcion_paises

subroutine hallar_elegido(vecinos_1, n_vecinos_1, elegido_1, n_vecinos_elegido_1, vecinos_elegido_1)
    implicit none
    integer(IP), intent(in) :: vecinos_1(Na, Nvecinos), n_vecinos_1(Na) 
    integer(IP), intent(out) :: elegido_1
    integer(IP), intent(inout) :: vecinos_elegido_1(Nvecinos), n_vecinos_elegido_1
    real(RP) :: rn
    do
        call random_number(rn)
        elegido_1 = int(Na*rn)+1
        if (n_vecinos_1(elegido_1) .eq. 0) cycle !Si no tiene vecinos
        call random_number(rn)
        elegido_2 = vecinos_1(elegido_1, int(n_vecinos_1(elegido_1)*rn)+1)
        exit
    end do
    n_vecinos_elegido_1 = n_vecinos_1(elegido_1)
    vecinos_elegido_1(1:Nvecinos) = vecinos_1(elegido_1, 1:Nvecinos)
end subroutine hallar_elegido

subroutine factor_idiomas_2(Nidiomas, ag_1, ag_2, ag_idiomas, factor)
    implicit none
    integer(IP), intent(in) :: Nidiomas, ag_1, ag_2
    integer(IP), intent(in) :: ag_idiomas(Na, Nidiomas)
    real(RP), intent(out) :: factor
    real(RP) :: aux
    integer :: i
    aux = 0._rp
    do i=1, Nidiomas, 1
        call factor_edu_eco_2(ag_1, ag_2, ag_idiomas, factor)
        if (factor .gt. aux) aux = factor
    end do
    factor = aux
end subroutine factor_idiomas_2

subroutine factor_edu_eco_2(ag_1, ag_2, ag_edu_eco, factor)
    implicit none
    integer(IP), intent(in) :: ag_1, ag_2
    integer(IP), intent(in) :: ag_edu_eco(Na)
    real(RP), intent(out) :: factor
    factor = 10._rp - abs(ag_edu_eco(ag_2)-ag_edu_eco(ag_1)) &
    - (10 - ag_edu_eco(ag_2)) + (ag_edu_eco(ag_2)-ag_edu_eco(ag_1))
end subroutine factor_edu_eco_2

subroutine media_desv_percepcion(agente_numero_vecinos, agente_vecinos, agente_pais, agente_vecinos_pais,&
	personas_pais_contacto, agente_percepcion, promedio_percepcion, desviacion_percepcion)
	implicit none
	integer(IP), intent(in) :: agente_numero_vecinos(Na), agente_vecinos(Na, Nvecinos), &
	agente_pais(Na), agente_vecinos_pais(Na,Npaises), personas_pais_contacto(Npaises)
	real(RP), intent(in) :: agente_percepcion(Na, Nvecinos) 
	real(RP), intent(out) :: promedio_percepcion(Npaises), desviacion_percepcion(Npaises)
	real(RP) :: media_personal(Na, Npaises)
	integer :: i, j, auxi_1
	!Promedio
	promedio_percepcion(1:Npaises) = 0._rp
	media_personal(1:Na, 1:Npaises) = 0._rp
	do i=1, Na, 1
	    do j=1, agente_numero_vecinos(i), 1
	        auxi_1 = agente_pais(agente_vecinos(i,j)) !agente en cuestión
	        media_personal(i, auxi_1) = media_personal(i, auxi_1) + agente_percepcion(i, j)
	    end do
	    do j=1, Npaises, 1
	    	if (agente_vecinos_pais(i,j) .eq. 0) cycle
	    	media_personal(i, j) = media_personal(i, j)/agente_vecinos_pais(i,j)
	    end do
	    promedio_percepcion(1:Npaises) = promedio_percepcion(1:Npaises) + media_personal(i, 1:Npaises)
	end do
	promedio_percepcion(1:Npaises) = promedio_percepcion(1:Npaises)/personas_pais_contacto(1:Npaises)
	!Desviación estándar
	desviacion_percepcion(1:Npaises) = 0._rp
	do i=1, Na, 1
	    do j=1, Npaises, 1
	    	if (agente_vecinos_pais(i,j) .eq. 0) cycle
	    	desviacion_percepcion(j) = desviacion_percepcion(j) + &
			(media_personal(i, j)-promedio_percepcion(j))**2
	    end do	
	end do
	desviacion_percepcion(1:Npaises) = sqrt(desviacion_percepcion(1:Npaises)/personas_pais_contacto(1:Npaises))	
        	
end subroutine media_desv_percepcion

subroutine adaptacion(agente_numero_vecinos, agente_vecinos, agente_pais, agente_percepcion, agente_adaptacion)
	implicit none
	integer(IP), intent(in) :: agente_numero_vecinos(Na), agente_vecinos(Na, Nvecinos), agente_pais(Na)
	real(RP), intent(in) :: agente_percepcion(Na, Nvecinos) 
	real(RP), intent(out) :: agente_adaptacion(Na)
	integer :: i, j, k
	agente_adaptacion(1:Na) = 0._rp
	do i=1, Na, 1
		do j=1, agente_numero_vecinos(i), 1
			do k=1, agente_numero_vecinos(agente_vecinos(i, j)), 1
				if (agente_vecinos(agente_vecinos(i, j), k) .eq. i) agente_adaptacion(i) = &
				agente_adaptacion(i) + agente_percepcion(agente_vecinos(i, j),k)
			end do
		end do
		agente_adaptacion(i) = agente_adaptacion(i)/agente_numero_vecinos(i)
	end do
end subroutine adaptacion

subroutine factor_intereses_2(Nintereses, ag_1, ag_2, ag_intereses, factor, posicion)
    implicit none
    integer(IP), intent(in) :: Nintereses, ag_1, ag_2
    real(RP), intent(in) :: ag_intereses(Na, Nintereses)
    real(RP), intent(out) :: factor
	integer(IP), intent(out) :: posicion
    real(RP) :: aux
    
    call random_number(aux)
    posicion = int(Nintereses*aux) + 1
	factor = ag_intereses(ag_1,posicion)*ag_intereses(ag_2,posicion)/10._rp
end subroutine factor_intereses_2

subroutine actualizar_interes(aumento, interes_1, interes_2)
    implicit none
    real(RP), intent(in) :: aumento
    real(RP), intent(inout) :: interes_1, interes_2
    
    if (interes_1*interes_2 .lt. 0) then		!Diferente signo
        if (interes_1 .gt. 0) then				!interes_1 > 0 y interes_2 < 0
            interes_1 = interes_1 - aumento
            interes_2 = interes_2 + aumento
            if (interes_1 .gt. 10._rp) interes_1 = 10._rp
            if (interes_2 .lt. -10._rp) interes_2 = -10._rp
        else							!interes_1 < 0 y interes_2 > 0
            interes_1 = interes_1 + aumento
            interes_2 = interes_2 - aumento
            if (interes_1 .lt. -10._rp) interes_1 = -10._rp
            if (interes_2 .gt. 10._rp) interes_2 = 10._rp
        end if
    else						!Mismo signo o alguno de los intereses vale 0.
        if (interes_1 .gt. 0) then				!interes_1 > 0 y interes_2 >= 0
            interes_1 = interes_1 + aumento
            interes_2 = interes_2 + aumento
            if (interes_1 .gt. 10._rp) interes_1 = 10._rp
            if (interes_2 .gt. 10._rp) interes_2 = 10._rp
        else if (interes_1 .lt. 0) then				!interes_1 < 0 y interes_2 <= 0
            interes_1 = interes_1 - aumento
            interes_2 = interes_2 - aumento
            if (interes_1 .lt. -10._rp) interes_1 = -10._rp
            if (interes_2 .lt. -10._rp) interes_2 = -10._rp
        else
            if (interes_2 .gt. 0) then				!interes_1 = 0 y interes_2 > 0
                interes_1 = interes_1 + aumento
                interes_2 = interes_2 + aumento
                if (interes_1 .gt. 10._rp) interes_1 = 10._rp
                if (interes_2 .gt. 10._rp) interes_2 = 10._rp
            else if ( interes_2 .lt. 0) then			!interes_1 = 0 y interes_2 < 0
                interes_1 = interes_1 - aumento
                interes_2 = interes_2 - aumento
                if (interes_1 .lt. -10._rp) interes_1 = -10._rp
                if (interes_2 .lt. -10._rp) interes_2 = -10._rp
            end if 
        end if
    end if
end subroutine actualizar_interes

subroutine sumar_valor_medios(opcion, valor, pais, agente_pais, agente_numero_vecinos, agente_vecinos, agente_percepcion)
    implicit none
    real(RP), intent(in) :: valor 
    integer(IP), intent(in) :: opcion, pais, agente_pais(Na), agente_vecinos(Na, Nvecinos), agente_numero_vecinos(Na)
    real(RP), intent(inout) :: agente_percepcion(Na, Nvecinos)
    real(RP) :: promedio, rn
    integer :: i, j, auxi_1, contador
    
    if (opcion .eq. 0) then				!Cambio en función de la percepción 
    	do i=1, Na, 1
        	!Media
       		contador = 0_ip
       		promedio = 0._rp
    		do j=1, agente_numero_vecinos(i), 1
        		auxi_1 = agente_pais(agente_vecinos(i,j)) !agente en cuestión
        		if (auxi_1 .eq. pais) then
        		    promedio = promedio + agente_percepcion(i, j)
        		    contador = contador + 1
        		end if
    		end do
    		if (contador .ne. 0)promedio = promedio/contador
        	do j=1, agente_numero_vecinos(i), 1
        	    if (agente_pais(agente_vecinos(i, j)) .eq. pais) then
        	        agente_percepcion(i, j) = agente_percepcion(i, j) + valor/2 + abs(valor/2)*(promedio/10)
        	        if(agente_percepcion(i, j) .gt. 10._rp) agente_percepcion(i, j) = 10._rp
    		        if(agente_percepcion(i, j) .lt. -10._rp) agente_percepcion(i, j) = -10._rp 
        	    end if
        	end do
    	end do
    end if
    if (opcion .eq. 1) then					!Cambio uniforme
    	do i=1, Na, 1
        	do j=1, agente_numero_vecinos(i), 1
        	    if (agente_pais(agente_vecinos(i, j)) .eq. pais) then
        	    	call random_number(rn)
        	        agente_percepcion(i, j) = agente_percepcion(i, j) + rn*valor
        	        if(agente_percepcion(i, j) .gt. 10._rp) agente_percepcion(i, j) = 10._rp
    		        if(agente_percepcion(i, j) .lt. -10._rp) agente_percepcion(i, j) = -10._rp 
        	    end if
        	end do
    	end do
    end if
end subroutine sumar_valor_medios

subroutine hallar_elegido2(proba, vecinos_1, n_vecinos_elegido_1, elegido_2, pos_elegido_2)
    implicit none
    real(RP), intent(in) :: proba(Nvecinos)
    integer(IP), intent(in) :: n_vecinos_elegido_1, vecinos_1(Nvecinos)
    integer(IP), intent(out) :: elegido_2, pos_elegido_2
    real(RP) :: rn, aux
    integer :: i
    call random_number(rn)
    aux = 0._rp
    do i=1, n_vecinos_elegido_1, 1
        aux = aux + proba(i)
        if ((rn-aux) .lt. 0._rp) then
            elegido_2 = vecinos_1(i)
            pos_elegido_2 = i
            exit
        end if
    end do
        
end subroutine hallar_elegido2
	
end program Tesis

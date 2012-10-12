program main

    use interfaces
    implicit none
    
    integer, parameter :: meqn = 3
    integer, parameter :: nvar = 2
    real, dimension(nvar) :: x
    
    external resid
  
    call timestamp()
    
    x = [3.0, 1.0, 0.5]
        
    call minimize(meqn, nvar, x, resid )
    
    call timestamp()
    
end

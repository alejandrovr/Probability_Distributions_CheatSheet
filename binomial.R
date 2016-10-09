my_binomial <-function (get_x_success, out_of_y_tries,prob_success){
  #get_x_success, for example: make 2 shoots
  #out_of_y_tries, for example: 6 opportunities
  #prob_success, your probability of scoring a shoot is 0.7 (better than o'neal)
  #Note, X=failed shoot, V=succesful shoot.
  
  ###############################################################
  # ORDER DOES NOT MATTER! VVXXXX, VXVXXX, XXVVXX,VXXXXV, (...) #
  ###############################################################
                         #
                         #
                         #
  
  #number_of_combinations_giving_2shoots_out_of_6opportunities * 0.7^2 * 0.3^4
  # VVXXXX=0.7*0.7*0.3*0.3*0.3*0.3=0.7^2*0.3^2
  # VXVXXX=0.7*0.3*0.7*0.3*0.3*0.3=0.7^2*0.3^2
  # (...) there are 15 combinations of V and X that give 2 successes in 6 chances
  # 6! / 2! * (6-2)! -> num_fact / den_fact_0 * den_fact_1
  i=2
  num_fact=1
  while (i<=out_of_y_tries){
    num_fact=num_fact*i
    i=i+1
  }
  i=2
  den_fact_0=1
  while (i<=get_x_success){
    den_fact_0=den_fact_0*i
    i=i+1
  } 
  den_fact_1=1
  i=2
  while(i<=out_of_y_tries-get_x_success){
    den_fact_1=den_fact_1*i
    i=i+1
  }

  return ( (num_fact / (den_fact_0*den_fact_1) ) * (prob_success^get_x_success) * ( (1-prob_success)^(out_of_y_tries-get_x_success) ) )
}

#To PLOT the distribution!:
#plot(1:6,sapply(1:6,my_binomial,out_of_y_tries=6,prob_success=0.7),ylab = 'Probability',xlab='number of hits')


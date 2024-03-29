require(png)
library(magick)

read_image = function(file){
  x = readPNG(file, native = FALSE)
  x = x*20 - 10
  
  t(x)[,nrow(x):1]
}

display_image = function(x, col=gray(seq(0,1,1/256)))
{
  w = dim(x)[1]
  h = dim(x)[2]
  par(mai = c(0,0,0,0))
  image(x, asp=h/w, col=col)
}

rand_generator = function()
{
  rand = runif(1)
  if(rand < 0.5)
    return (1)
  else 
    return (-1)
}

checkerboard = function(row, col){
  check = matrix(0, row, col)
  for(i in 1:row){
    for(j in 1:col){
      if(check[i,j] == ((i+j) %% 2)){
        check[i,j] = 0
      }
      else{
        check[i,j] = 1
      }
    }
  }
  return(check)
}


# gen_ising_prior_term = function(img, alpha, beta){
#   num_col = dim(img)[2]
#   num_row = dim(img)[1]
# 
#   #generating the new image, initializing it to 0 for all the values
#   img2 = matrix(0, num_row, num_col)
# 
#   for(i in 1:num_row){
#     for(j in 1:num_col){
#       sum = 0
#       if(i != num_row){
#         sum = sum + img[i+1, j]
#       }
#       if(i != 1){
#         sum = sum + img[i-1, j]
#       }
#       if(j != num_col){
#         sum = sum + img[i, j+1]
#       }
#       if(j != 1){
#         sum = sum + img[i, j-1]
#       }
#       u_positive = alpha + beta*sum
#       u_negative = -alpha - beta*sum
#       probability_negative = exp(u_negative)
#       probability_positive = exp(u_positive)
# 
#       final_probability = probability_positive / (probability_negative + probability_positive)
#       rand = runif(1)
#       if(final_probability < rand){
#         img2[i,j] = -1
#       }
#       else{
#         img2[i,j] = 1
#       }
#     }
#   }
#   return(img2);
# }

gen_ising_prior_term = function(img, alpha, beta){
  num_col = dim(img)[2]
  num_row = dim(img)[1]
  
  #generating the new image, initializing it to 0 for all the values
  img2 = matrix(0, num_row, num_col)
  checker = checkerboard(num_row, num_col)
  for(i in 1:num_row){
    for(j in 1:num_col){
      sum = 0
      if(i != num_row){
        sum = sum + img[i+1, j]
      }
      if(i != 1){
        sum = sum + img[i-1, j]
      }
      if(j != num_col){
        sum = sum + img[i, j+1]
      }
      if(j != 1){
        sum = sum + img[i, j-1]
      }
      u_positive = alpha + beta*sum
      u_negative = -alpha - beta*sum
      probability_negative = exp(u_negative)
      probability_positive = exp(u_positive)
      
      final_probability = probability_positive / (probability_negative + probability_positive)
      rand = runif(1)
      if(final_probability < rand){
        img2[i,j] = -1
      }
      else{
        img2[i,j] = 1
      }
    }
  }
  
  
  # for(i in 1:num_row){
  #   for(j in 1:num_col){
  #     img2[i,j] = (img2[i,j] * checker[i,j]) + (img[i,j] * (1 - checker[i,j]))
  #   }
  # }
  return(img2); 
}


gen_ising_prior_posterior_term = function(img, alpha, beta, sig){
  num_col = dim(img)[2]
  num_row = dim(img)[1]
  
  #generating the new image, initializing it to 0 for all the values
  img2 = matrix(0, num_row, num_col)  
  
  for(i in 1:num_row){
    for(j in 1:num_col){
      sum = 0
      if(i != num_row){
        sum = sum + img[i+1, j]
      }
      if(i != 1){
        sum = sum + img[i-1, j]
      }
      if(j != num_col){
        sum = sum + img[i, j+1]
      }
      if(j != 1){
        sum = sum + img[i, j-1]
      }
      t1 = 1/(2*sig^2)
       num_positive = alpha + beta*sum - (t1*((1 - (1 - img[i,j])^2)))
       num_negative = -alpha - beta*sum - (t1*((-1 - (1 - img[i,j])^2)))
      #num_positive = alpha*img2[i,j] + beta*sum*img2[i,j] - (t1*((img2[i,j] - img[i,j])^2))
      #num_negative = -alpha*img2[i,j] - beta*sum*img2[i,j] - (t1*((img2[i,j] - img[i,j])^2))
  
      probability_negative = exp(num_negative)
      probability_positive = exp(num_positive)

      final_probability = probability_positive / (probability_negative + probability_positive);
      #print(final_probability)
      rand = runif(1);
      
      if(final_probability > rand){
        img2[i,j] = 1
      } else{
        img2[i,j] = -1
      }
    }
  }

  return(img2);
}

# gibbs_sampling_prior = function(img, alpha, beta, sig, iterations, burn){
#   num_col = dim(img)[2]
#   num_row = dim(img)[1]
#   num = 0
#   #generating the new image, initializing it to 0 for all the values
#   img2 = img
#   it = iterations + burn
#   for(i in 1:it){
#     img2 = gen_ising_prior_term(img2, alpha, beta)
#   }
#   return(img2)
# }

gibbs_sampling_prior = function(img, alpha, beta, sig, iterations, burn){
  num_col = dim(img)[2]
  num_row = dim(img)[1]
  num = 0
  #generating the new image, initializing it to 0 for all the values
  img2 = img
  img_orig = img
  checker = checkerboard(num_row, num_col)
  it = iterations + burn
  for(i in 1:it){
    img2 = gen_ising_prior_term(img2, alpha, beta)
    img2 = img2*checker + (1 - checker)*img
    img = img2
    checker = 1 - checker
  }
  return(img2)
}

gibbs_sampling_prior_posterior = function(img, alpha, beta, sig, iterations, burn){
  num_col = dim(img)[2]
  num_row = dim(img)[1]
  num = 0
  #generating the new image, initializing it to 0 for all the values
  it = iterations + burn
  img2 = img
  for(i in 1:it){
    img3 = gen_ising_prior_posterior_term(img2, alpha, beta, sig)
    if(i > burn){
      num = num + 1
      img2 = (img2 * (num - 1) + img3)/num
    }
  }
  return(img2)
}

gen_estimated_variance = function(img, alpha, beta, sig, iterations, burn){
  num_col = dim(img)[2]
  num_row = dim(img)[1]
  num = 0
  sample_sig = matrix(0, iterations, 1)
  #img2 = matrix(0, num_row, num_col)
  img2 = img
  img_orig = img
  it = iterations + burn
  img3 = img
  for(i in 1:it){
    img3 = gen_ising_prior_posterior_term(img2, alpha, beta, sig)
    if(i > burn){
      num = num + 1
      img2 = (img2 * (num - 1) + img3)/num
      new_var = 0
      for(j in 1:num_row){
        for(k in 1:num_col){
          new_var = new_var + (img2[j,k] - img_orig[j,k])^2;
        }
      }
      sig = new_var/(num_col*num_row);
      #can be used for plotting
      sample_sig[num,] = sig
    }
  }
  cat("The sigma is ", sig, "\n")
  return(img2)
}
# 
# get_estimated_variance_prior_posterior = function(img, alpha, beta, sig, iterations, burn){
#   num_col = dim(img)[2]
#   num_row = dim(img)[1]
#   num = 0
#   #generating the new image, initializing it to 0 for all the values
#   img2 = matrix(0, num_row, num_col)  
#   img_orig = img
#   sample_sig = matrix(0, iterations, 1)
#   it = iterations + burn
#   img3 = img
#   for(i in 1:it){
#     img3 = gen_ising_prior_posterior_term(img, alpha, beta, sig)
#     if(i > burn){
#       num = num + 1
#       img2 = (img2 * (num - 1) + img3)/num
#       #display_image(img2)
#       new_var = 0
#         for(j in 1:num_row){
#           for(k in 1:num_col){
#             new_var = new_var + (img2[j,k] - img_orig[j,k])^2
#           }
#         }
#       #sample_sig[num,] = sig
#       sig = new_var/(num_row * num_col)
#     }
#     img = img3
#   }
#   #print(sum(sample_sig)/iterations)
#   cat("The sigma is ", sig)
#   return(img2)
# }







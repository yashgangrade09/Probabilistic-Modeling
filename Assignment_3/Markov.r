require(png)

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
  rand = runif(1);
  if(rand < 0.5)
    return (1)
  else 
    return (-1)
}


## Testing

f <- function(x,b,a){
  print('thisis b')
  print(b)
  print('thisis a')
  print(a)
  return (x*x+a*b)
}

integ <-function(v,c){
  result = integrate(f,0,5,3,2,subdivisions = 50, stop.on.error = TRUE)  
  return (result)
}

part1 = integ(0,2)
part1 = integ(0,3)
b = as.numeric(part1)+part2

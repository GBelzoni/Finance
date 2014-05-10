
#Error handler example
e = simpleWarning('you have a warning')
for( i in 1:3){
  tr = tryCatch(read.table(i), error = function(e){
    e} )
  print(class(tr)[1]=='simpleError')
  print(i*5)
}

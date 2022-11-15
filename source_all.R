
## Source and aggregate data to create all figures

fig<-list.files('scripts')
for(i in 1:length(fig)){
  print(paste('Running', fig[i]))
  suppressMessages(
    source(paste0('scripts/', fig[i]))
  )
}


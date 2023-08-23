

model = "model
{
    theta ~ dunif(0,1)
    x ~ dbin(theta,N)
}
"

data = list(x=2,N=5)    
variable_names = c('theta')
burn_in = 1000
steps = 10000
thin = 1

library(rjags)

fileConn=file("model.temp")
writeLines(model, fileConn)
close(fileConn)
if(all(is.na(data)))
{
    m = jags.model(file="model.temp")
} else
{
    m = jags.model(file="model.temp", data=data)
}
update(m, burn_in)
draw = jags.samples(m, steps, thin=thin, variable.names = variable_names) # Convert to a list
make_list <- function(draw)
{
    results = list()
    for(name in names(draw))
    {
        # Extract "chain 1"
        results[[name]] = as.array(draw[[name]][,,1])
        # Transpose 2D arrays
        if(length(dim(results[[name]])) == 2)
            results[[name]] = t(results[[name]])
    }
    return(results)
}
results = make_list(draw)


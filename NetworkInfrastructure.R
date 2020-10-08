lambda <- 1
mi <- 4 / 3
servers <- 20
packets <- 200
time = rexp(1, rate = lambda)
interval_time <- c(time, rep(0, times = packets - 1))
arrival_time <- c(time, rep(0, times = packets - 1))
enter_service_time <- c(rep(0, times = packets))
compilation_time <- c(rep(0, times = packets))
service_time <- c(rep(0, times = packets))
lastPackets <- c(rep(0, times = servers))
for (i in 2:packets) {
  time = rexp(1, rate = lambda)
  arrival_time[i] = arrival_time[i - 1] + time
  interval_time[i] = time
}
remainingPackets <- packets
k <- 1
i <- k
remainingServers <- servers
while (remainingServers > 0) {
  remainingServers <- remainingServers - 1
  enter_service_time[k] <- arrival_time[k]
  k <- k + 1
}
remainingServers <- servers
k <- i
while (remainingServers > 0) {
  remainingServers <- remainingServers - 1
  time <- rexp(1, rate = mi)
  compilation_time[k] <- enter_service_time[k] + time
  lastPackets[k] = compilation_time[k]
  service_time[k] <- time
  k <- k + 1
}
i <- k
tmp <- sort(lastPackets)
remainingPackets <- remainingPackets - servers
while (remainingPackets > 0) {
  remainingPackets <- remainingPackets - servers
  j <- 1
  i <- k
  remainingServers <- servers
  while (remainingServers > 0) {
    remainingServers <- remainingServers - 1
    enter_service_time[k] <- max(arrival_time[k], tmp[j])

    k <- k + 1
    j <- j + 1
  }
  k <- i
  j <- 1
  remainingServers <- servers
  while (remainingServers > 0) {
    remainingServers <- remainingServers - 1
    time <- rexp(1, rate = mi)
    compilation_time[k] <- enter_service_time[k] + time
    lastPackets[j] <- compilation_time[k]
    service_time[k] <- time
    k <- k + 1
    j <- j + 1
  }
  tmp <- sort(lastPackets)
}
table <-
  cbind(
    interval_time[1:packets],
    arrival_time[1:packets],
    enter_service_time[1:packets],
    service_time[1:packets],
    compilation_time[1:packets]
  )
colnames(table) <-
  c("interval_time",
    "arrival",
    "enter_service",
    "serivce",
    "completion")
rownames(table) <- c(paste0("Packet ", 1:packets))
table
system_time <- compilation_time[1:packets] - arrival_time[1:packets]
queue_time <-
  enter_service_time[1:packets] - arrival_time[1:packets]
P <- c(rep(0, times = packets))
P0 <- 0
ro <- lambda / (mi)
for (k in 0:servers) {
  P0 <- P0 + (ro ^ k / factorial(k))
}
tmp <- 0
for (k in servers + 1:packets) {
  tmp <- tmp + ((ro / servers) ^ k)
}
tmp <- tmp * (servers ^ servers / factorial(servers))
P0 <- (P0 + tmp) ^ (-1)
for (k in 1:servers) {
  P[k] <- ((ro ^ k) / factorial(k)) * P0
}
for (k in servers + 1:packets) {
  P[k] <-
    ((servers ^ servers) / factorial(servers)) * ((ro / servers) ^ k) *
    P0
}

#Karakteristiki
mean(system_time) #prosecno vreme potrosheno vo sistemot W
mean(queue_time) #prosecno vreme potrosheno vo redicata  Wq
L <-
  sum(c(1:(packets + servers)) * P)
Lq <- 0
for (k in servers + 1:packets) {
  Lq <- Lq + (k - servers) * P[k]
}
L # prosecen broj na klienti vo sistemot L
Lq  #prosecen broj na klienti vo redicata

#Proverka so gotova biblioteka
library(queueing)
input <- NewInput.MMCK(lambda = lambda,
                       mu = mi,
                       c = servers,
                       k = packets)
output <- QueueingModel(input)
Report(output)
summary(output)
#Vizuelizacija na brojot na paketi vo sistemot, intervalno vreme, servis vreme
curve(
  dpois(x, input$lambda),
  from = 0,
  to = packets,
  type = "b",
  lwd = 3,
  xlab = "Number of packets",
  ylab = "Probability",
  main = "Poisson Distribution for Arrival Process",
  ylim = c(0, 1),
  n = 21
)
curve(
  dexp(x, rate = 1 / input$lambda),
  from = 0,
  to = max(interval_time),
  type = "l",
  lwd = 3,
  xlab = "Interarrival Time",
  ylab = "Probaility",
  main = "Exponential Distribution for Interarrival Time",
  ylim = c(0, 1)
)
curve(
  dexp(x, rate = input$mu),
  from = 0,
  to = max(service_time),
  type = "l",
  lwd = 3,
  xlab = "Service Waiting Time",
  ylab = "Probaility",
  main = "Exponential Distribution for Service Process",
  ylim = c(0, 1)
)

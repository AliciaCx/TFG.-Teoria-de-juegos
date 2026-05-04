#-------------------------------------------------------------------------------
# Ejemplo 3.7. Juego básico de Cournot
#-------------------------------------------------------------------------------

dimx <- c(1, 1)

#------------------------------------------------
# Gradiente de la utilidad u_i
#------------------------------------------------
grobj <- function(x, i, j)
{
  q1 <- x[1]
  q2 <- x[2]
  
  if(i == 1){
    # u1 = q1(1 - q1 - q2)
    grad <- c(1 - 2*q1 - q2, -q1)
  }
  if(i == 2){
    # u2 = q2(1 - q1 - q2)
    grad <- c(-q2, 1 - q1 - 2*q2)
  }
  
  grad[j]
}

#------------------------------------------------
# Hessiano de la utilidad u_i
#------------------------------------------------
heobj <- function(x, i, j, k)
{
  if(i == 1){
    H <- matrix(c(-2, -1,
                  -1,  0), nrow=2, byrow=TRUE)
  }
  if(i == 2){
    H <- matrix(c( 0, -1,
                   -1, -2), nrow=2, byrow=TRUE)
  }
  
  H[j, k]
}

#------------------------------------------------
# No hay restricciones de acoplamiento
#------------------------------------------------
dimlam <- c(1, 1)

g <- function(x, i)
  0

grg <- function(x, i, j)
  0

heg <- function(x, i, j, k)
  0

#------------------------------------------------
# Punto inicial
#------------------------------------------------
x0 <- c(0, 0)

z0 <- c(
  x0,
  2, 2,
  max(10, 5 - g(x0, 1)),
  max(10, 5 - g(x0, 2))
)

#------------------------------------------------
# Equilibrio de Nash (Cournot)
#------------------------------------------------
# teórico: (1/3, 1/3)

GNE.ceq(z0, dimx, dimlam,
        grobj = grobj,
        heobj = heobj,
        constr = g,
        grconstr = grg,
        heconstr = heg,
        method = "PR",
        control = list(trace = 0, maxit = 50))

GNE: 0.3333333 0.3333333 1.286307 1.286307 1.005564e-07 1.005564e-07 
with optimal norm 2.316984e-07 
after  9 iterations with exit code 1 .
Output message: Function criterion near zero 
Function/grad/hessian calls: 9 9 
Optimal (vector) value: 0 0 1.005564e-07 1.005564e-07 1.293464e-07 1.293464e-07 

GNE.ceq(z0, dimx, dimlam,
        grobj = grobj,
        heobj = heobj,
        constr = g,
        grconstr = grg,
        heconstr = heg,
        method = "AS",
        global = "pwldog",
        xscalm = "auto",
        control = list(trace = 0, maxit = 100))

GNE: 0.3333333 0.3333333 0.2154527 0.2154527 1.888004e-08 1.888004e-08 
with optimal norm 2.751236e-08 
after  4 iterations with exit code 1 .
Output message: Function criterion near zero 
Function/grad/hessian calls: 4 4 
Optimal (vector) value: 2.337144e-09 2.337144e-09 1.888004e-08 1.888004e-08 4.067756e-09 4.067756e-09

library(ggplot2)

q <- seq(0, 1, length.out = 100)

R1 <- (1 - q) / 2
R2 <- (1 - q) / 2

df1 <- data.frame(q1 = R1, q2 = q, tipo = "R1(q2) = (1 - q2)/2")
df2 <- data.frame(q1 = q, q2 = R2, tipo = "R2(q1) = (1 - q1)/2")

df <- rbind(df1, df2)

q_star <- 1/3

ggplot(df, aes(x = q1, y = q2, linetype = tipo)) +
  geom_line(linewidth = 1) +
  
  annotate("point", x = q_star, y = q_star,
           color = "red", size = 3) +
  
  # Etiqueta desplazada a la derecha
  annotate("text",
           x = q_star + 0.05,  # 👈 desplazamiento horizontal
           y = q_star,
           label = "(1/3, 1/3)",
           color = "red",
           hjust = 0) +       # alineación hacia la derecha del punto
  
  geom_vline(xintercept = q_star, linetype = "dotted") +
  geom_hline(yintercept = q_star, linetype = "dotted") +
  
  labs(
    x = expression(q[1]),
    y = expression(q[2]),
    linetype = "Funciones de reacción",
    title = "Modelo de Cournot"
  ) +
  
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal()





















library(ggplot2)

q <- seq(0, 1, length.out = 100)

R1 <- (1 - q) / 2
R2 <- (1 - q) / 2

df <- rbind(
  data.frame(q1 = R1, q2 = q, tipo = "R1(q2)"),
  data.frame(q1 = q, q2 = R2, tipo = "R2(q1)")
)

q_star <- 1/3

ggplot(df, aes(x = q1, y = q2, linetype = tipo)) +
  geom_line(linewidth = 1) +
  annotate("point", x = q_star, y = q_star, color = "red", size = 3) +
  annotate("text", x = q_star + 0.05, y = q_star,
           label = "(1/3, 1/3)", color = "red", hjust = 0) +
  geom_vline(xintercept = q_star, linetype = "dotted") +
  geom_hline(yintercept = q_star, linetype = "dotted") +
  labs(
    x = expression(q[1]),
    y = expression(q[2]),
    linetype = "Funciones de mejor respuesta",
    title = "Equilibrio de Nash en el modelo de Cournot"
  ) +
  xlim(0, 1) + ylim(0, 1) +
  theme_minimal()










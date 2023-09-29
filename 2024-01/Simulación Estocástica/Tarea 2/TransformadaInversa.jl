using Plots
using Random
using Distributions

Random.seed!(1234)
u = rand(Uniform(0, 1), 1000) # usé el mismo conjunto de aleatorios
# Ejercicio 1.a
# Distribución de Laplace
function f1(x, μ, b) 
    @assert b>0
    exp(-abs(x-μ)/b) / (2*b)
end
# Inversa generalizada
function F1inv(x, μ, b)
    @assert b>0
    if x < 1/2
        b*log(2x) + μ
    else
        μ - b*log(2-2x)
    end
end
# Obtenemos la simulación con ley de probabilidad f.
X1 = F1inv.(u, 0, 1)
# Histograma de X
histogram(X1, normalize = true, ylab="Densidad",
          label="Densidad simulada", 
          title="Densidad de Laplace con Transformada Inversa,\nμ=0, b = 1\n",
          dpi = 200)
# Gráfica de la densidad teórica.
t1 = minimum(X1):0.01:maximum(X1)
p1 = plot!(t1, f1.(t1, 0, 1), linewidth = 2, label="Densidad teórica")
savefig(p1, "D:/Edgar Trejo/Repositorios/Universidad/2024-01/Simulación Estocástica/Tarea 2/Laplace.png")

# Ejercicio 1.b
# Distribución de Pareto
function f2(x, x0, α)
    @assert x0>0 
    @assert α>0
    α*x0^α/x^(α+1)
end
# Inversa generalizada
function F2inv(x, x0, α)
    @assert x0>0 
    @assert α>0
    x0*(1-x)^(-1/α)
end
# Obtenemos la simulación con ley de probabilidad f.
x0 = 1
X2 = F2inv.(u, x0, 5)
histogram(X2, normalize = true, ylab="Densidad",
          label="Densidad simulada", 
          title="Densidad de Pareto con Transformada Inversa,\nx0 = 1, α = 5\n",
          dpi = 200)
t2 = x0:0.01:maximum(X2)
p2 = plot!(t2, f2.(t2, 1, 5), linewidth = 2, label="Densidad teórica")
savefig(p2, "D:/Edgar Trejo/Repositorios/Universidad/2024-01/Simulación Estocástica/Tarea 2/Pareto.png")

# Ejercicio 1.c
# Distribución de Weibull
function f3(x, k, λ)
    @assert k>0
    @assert λ>0
    k/λ * (x/λ)^(k-1) * exp(-(x/λ)^k)
end
# Inversa generalizada
function F3inv(x, k, λ)
    @assert k>0
    @assert λ>0
    λ*(-log(1-x))^(1/k)
end
# Obtenemos la simulación con ley de probabilidad f.
X3 = F3inv.(u, 1.5, 1)
histogram(X3, normalize = true, ylab="Densidad",
          label="Densidad simulada", 
          title="Densidad Weibull con Transformada Inversa,\nλ = 1, k = 1.5\n",
          dpi = 200)
t3 = 0:0.01:maximum(X3)
p3 = plot!(t3, f3.(t3, 1.5, 1), linewidth = 2, label="Densidad teórica")
savefig(p3, "D:/Edgar Trejo/Repositorios/Universidad/2024-01/Simulación Estocástica/Tarea 2/Weibull.png")
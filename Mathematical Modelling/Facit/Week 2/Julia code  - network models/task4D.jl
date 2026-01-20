using JuMP, HiGHS

c_matrix =
[    0.0    450.0    300.0   2200.0   6500.0  10500.0  19500.0  20000.0  16000.0   8800.0   6200.0
   450.0      0.0    500.0   2400.0   6600.0  10700.0  19700.0  20200.0  16200.0   9000.0   6400.0
   300.0    500.0      0.0   2100.0   6400.0  10400.0  19400.0  19900.0  15800.0   8600.0   6100.0
  2200.0   2400.0   2100.0      0.0   5200.0   8500.0  16500.0  17000.0  15000.0   8200.0   6500.0
  6500.0   6600.0   6400.0   5200.0      0.0   5800.0  10200.0  10800.0  13500.0  15000.0  11000.0
 10500.0  10700.0  10400.0   8500.0   5800.0      0.0   3800.0   4600.0  14100.0  16700.0  15500.0
 19500.0  19700.0  19400.0  16500.0  10200.0   3800.0      0.0    900.0  10400.0  12500.0  19000.0
 20000.0  20200.0  19900.0  17000.0  10800.0   4600.0    900.0      0.0   9800.0  12000.0  18500.0
 16000.0  16200.0  15800.0  15000.0  13500.0  14100.0  10400.0   9800.0      0.0   4800.0   6300.0
  8800.0   9000.0   8600.0   8200.0  15000.0  16700.0  12500.0  12000.0   4800.0      0.0   3500.0
  6200.0   6400.0   6100.0   6500.0  11000.0  15500.0  19000.0  18500.0   6300.0   3500.0      0.0]

N = size(c_matrix, 1)

ports = [
    "ROT", "HAM", "FEL", "ALG", "DXB", "SIN", "SHA", "BUS", "LAX", "PAN", "NYC"
]

ship_capacity = 800.0  # TEU

# ------------------------------------------------------------
# Demands (commodities): (origin, destination, volume TEU/week)
# ------------------------------------------------------------
demands = [
    (2, 7, 480.0),   # k1: Hamburg -> Shanghai
    (1, 6, 400.0),   # k2: Rotterdam -> Singapore
    (3, 5,  320.0),  # k3: Felixstowe -> Jebel Ali
    (7, 11, 560.0),  # k4: Shanghai -> New York
    (6, 10,  360.0), # k5: Singapore -> Panama
    (1, 11, 440.0),  # k6: Rotterdam -> New York
    (2, 9, 520.0),    # k7: Hamburg -> Los Angeles
    (8, 4, 300.0)   # k8: Busan -> Algeciras
]
K = length(demands)

ship_types = ["small", "large"]
ship_capacity = [400, 800]
ship_cost_per_km = [6, 10]
S = length(ship_types)


model = Model(HiGHS.Optimizer)
#set_silent(model)

@variable(model, f[1:N, 1:N, 1:K] >= 0)
@variable(model, x[1:N, 1:N, 1:S], Bin)
@objective(model, Min, sum(0.01 * c_matrix[i,j] * f[i,j,k] for i in 1:N, j in 1:N, k in 1:K) + sum(c_matrix[i,j] * ship_cost_per_km[s] * x[i,j,s] for i in 1:N, j in 1:N, s in 1:S))
# Flow conservation constraints
for k in 1:K
    o, d, q = demands[k]
    @constraint(model, [i in 1:N; i!=o && i!=d], sum(f[i,j,k] for j in 1:N) - sum(f[j,i,k] for j in 1:N) == 0)
    @constraint(model, sum(f[o,j,k] for j in 1:N) - sum(f[j,o,k] for j in 1:N) == q)
    @constraint(model, sum(f[d,i,k] for i in 1:N) - sum(f[i,d,k] for i in 1:N) == -q)
end
# Capacity constraints
@constraint(model, [i in 1:N, j in 1:N], sum(f[i,j,k] for k in 1:K) <= sum(ship_capacity[s] * x[i,j,s] for s in 1:S))

@constraint(model, [i in 1:N, s in 1:S], sum(x[i,j,s] for j in 1:N) - sum(x[j,i,s] for j in 1:N) == 0)

# no self-loops
@constraint(model, sum(x[i,i,s] for i in 1:N, s in 1:S) == 0)

optimize!(model)

println("Termination status: ", termination_status(model))
if termination_status(model) == MOI.OPTIMAL
    for k in 1:K
        o, d, q = demands[k]
        println("Commodity $k: from $o to $d, demand $q")
        for i in 1:N, j in 1:N
            if value(f[i,j,k]) > 0.1
                println("  Flow on arc ($i,$j): ", value(f[i,j,k]))
            end
        end
    end

    # Print arc utilizations
    println("\nArc utilizations (total flow / capacity):")
    for i=1:N, j=1:N
        used = sum(value(f[i,j,k]) for k in 1:K)
        if used > 1e-6
            println("  $(ports[i]) -> $(ports[j]):  ", round(used, digits=2))
        end
    end
    

    println("Used arcs:")
    for i in 1:N, j in 1:N, s in 1:S
        if value(x[i,j,s]) > 0.1
            println("  Arc ($i,$j) with ship type $s is used.")
        end
    end
end
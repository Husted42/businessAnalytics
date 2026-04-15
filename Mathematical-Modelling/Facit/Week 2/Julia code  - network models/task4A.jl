using JuMP, HiGHS

c_matrix =
[0	4.5	3	22	65	105	195	200	160	88	62
4.5	0	5	24	66	107	197	202	162	90	64
3	5	0	21	64	104	194	199	158	86	61
22	24	21	0	52	85	165	170	150	82	65
65	66	64	52	0	58	102	108	135	150	110
105	107	104	85	58	0	38	46	141	167	155
195	197	194	165	102	38	0	9	104	125	190
200	202	199	170	108	46	9	0	98	120	185
160	162	158	150	135	141	104	98	0	48	63
88	90	86	82	150	167	125	120	48	0	35
62	64	61	65	110	155	190	185	63	35	0]

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

model = Model(HiGHS.Optimizer)
#set_silent(model)

@variable(model, f[1:N, 1:N, 1:K] >= 0)
@variable(model, x[1:N, 1:N], Bin)
@objective(model, Min, sum(c_matrix[i,j] * f[i,j,k] for i in 1:N, j in 1:N, k in 1:K) + sum(c_matrix[i,j] * 1000 * x[i,j] for i in 1:N, j in 1:N))
# Flow conservation constraints
for k in 1:K
    o, d, q = demands[k]
    @constraint(model, [i in 1:N; i!=o && i!=d], sum(f[i,j,k] for j in 1:N) - sum(f[j,i,k] for j in 1:N) == 0)
    @constraint(model, sum(f[o,j,k] for j in 1:N) - sum(f[j,o,k] for j in 1:N) == q)
    @constraint(model, sum(f[d,i,k] for i in 1:N) - sum(f[i,d,k] for i in 1:N) == -q)
end
# Capacity constraints
@constraint(model, [i in 1:N, j in 1:N], sum(f[i,j,k] for k in 1:K) <= ship_capacity * x[i,j])

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
            println("  $(ports[i]) -> $(ports[j]):  ", round(used, digits=2), " / ", ship_capacity)
        end
    end
    

    println("Used arcs:")
    for i in 1:N, j in 1:N
        if value(x[i,j]) > 0.1
            println("  Arc ($i,$j) is used.")
        end
    end   
end
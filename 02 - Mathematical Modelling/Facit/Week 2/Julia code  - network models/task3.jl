using JuMP, HiGHS

# ============================================================
# Multi-Commodity Flow (MCF) LP for container shipping
# ============================================================
# Ports (nodes) are indexed 1..m:
#  1 ROT (Rotterdam)     2 HAM (Hamburg)      3 FEL (Felixstowe)
#  4 ALG (Algeciras)     5 DXB (Jebel Ali)    6 SIN (Singapore)
#  7 SHA (Shanghai)      8 BUS (Busan)        9 LAX (Los Angeles)
# 10 PAN (Panama)       11 NYC (New York)
# ============================================================

ports = [
    "ROT", "HAM", "FEL", "ALG", "DXB", "SIN", "SHA", "BUS", "LAX", "PAN", "NYC"
]
m = length(ports)

# ------------------------------------------------------------
# arcs: defined by ship rotation legs (directed)
# ------------------------------------------------------------

# Define arcs by rotation (as directed legs)
arcs = [(1,2), (2,3), (3,4), (4,1),                  # ROT->HAM->FEL->ALG->ROT
        (4,5), (5,6), (6,4),                         # ALG->DXB->SIN->ALG
        (6,7), (7,8), (8,6),                         # SIN->SHA->BUS->SIN
        (7,9), (9,10), (10,11), (11,7),              # SHA->LAX->PAN->NYC->SHA  
        (1,11), (11,10), (10,1),                     # ROT->NYC->PAN->ROT
        (2,5), (5,7), (7,6), (6,2)]                  # HAM->DXB->SHA->SIN->HAM

# Capacity per directed arc a 
cap = Dict{Tuple{Int,Int}, Float64}()
# ROT->HAM->FEL->ALG->ROT
cap[(1,2)] = 800.0
cap[(2,3)] = 800.0
cap[(3,4)] = 800.0
cap[(4,1)] = 800.0

# ALG->DXB->SIN->ALG
cap[(4,5)] = 1100.0
cap[(5,6)] = 1100.0
cap[(6,4)] = 1100.0

# SIN->SHA->BUS->SIN
cap[(6,7)] = 1400.0
cap[(7,8)] = 1400.0
cap[(8,6)] = 1400.0

# SHA->LAX->PAN->NYC->SHA
cap[(7,9)] = 1000.0
cap[(9,10)] = 1000.0
cap[(10,11)] = 1000.0
cap[(11,7)] = 1000.0

# ROT->NYC->PAN->ROT
cap[(1,11)] = 900.0
cap[(11,10)] = 900.0
cap[(10,1)] = 900.0

# HAM->DXB->SHA->SIN->HAM
cap[(2,5)] = 1200.0
cap[(5,7)] = 1200.0
cap[(7,6)] = 1200.0
cap[(6,2)] = 1200.0

# cost matrix:
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

# ------------------------------------------------------------
# Demands (commodities): (origin, destination, volume TEU/week)
# ------------------------------------------------------------
demands = [
    (2, 7, 480.0),  # k1: Hamburg -> Shanghai
    (1, 6, 400.0),  # k2: Rotterdam -> Singapore
    (3, 5,  320.0),  # k3: Felixstowe -> Jebel Ali
    (7, 11, 560.0), # k4: Shanghai -> New York
    (6, 10,  360.0), # k5: Singapore -> Panama
    (1, 11, 440.0), # k6: Rotterdam -> New York
    (2, 9, 520.0),   # k7: Hamburg -> Los Angeles
    (8, 4, 300.0)   # k8: Busan -> Algeciras
]
K = length(demands)

# ------------------------------------------------------------
# Build LP model
# ------------------------------------------------------------
model = Model(HiGHS.Optimizer)
set_silent(model)

# Flow variables: f[k, a] = TEU of commodity k shipped on arc a
@variable(model, f[a in arcs, 1:K] >= 0)

# Objective: minimize total cost
@objective(model, Min, sum(c_matrix[a[1], a[2]] * f[a,k] for k in 1:K, a in arcs))

# Capacity constraints: total flow on arc <= capacity
@constraint(model, [a in arcs], sum(f[a,k] for k in 1:K) <= cap[a])

# Flow conservation for each commodity
for k in 1:K
    o, d, q = demands[k]
    @constraint(model, [n in 1:m; n != o && n != d], sum(f[a,k] for a in arcs if a[2] == n) == sum(f[a,k] for a in arcs if a[1] == n))
    @constraint(model, sum(f[a,k] for a in arcs if a[1] == o) - sum(f[a,k] for a in arcs if a[2] == o) == q)
    @constraint(model, sum(f[a,k] for a in arcs if a[1] == d) - sum(f[a,k] for a in arcs if a[2] == d) == -q)
end

optimize!(model)

println("Termination status: ", termination_status(model))
if termination_status(model) == MOI.OPTIMAL
    println("Objective value: ", objective_value(model))

    # Print arc utilizations
    println("\nArc utilizations (total flow / capacity):")
    for a in sort(arcs)
        used = sum(value(f[a,k]) for k in 1:K)
        if used > 1e-6
            println("  $(ports[a[1]]) -> $(ports[a[2]]):  ",
                    round(used, digits=2), " / ", cap[a])
        end
    end

    # Print per-commodity flows (only positive arcs)
    println("\nPer-commodity routed arcs:")
    for k in 1:K
        ok, dk, qk = demands[k]
        println("Commodity $k: $(ports[ok]) -> $(ports[dk])  (", qk, " TEU)")
        for a in sort(arcs)
            val = value(f[a,k])
            if val > 1e-6
                println("  $(ports[a[1]]) -> $(ports[a[2]]): ", round(val, digits=2))
            end
        end
    end
else
    println("Model not optimal (maybe infeasible).")
end

using JuMP, HiGHS

m = 6  # Number of nodes

# Node indices
# 1: CPH, 2: FRA, 3: DXB, 4: SIN, 5: DOH, 6: SYD
S = 1  # Copenhagen
T = 6  # Sydney

# Directed arcs
arcs = [
    (1,2),  # CPH -> FRA
    (1,3),  # CPH -> DXB
    (2,3),  # FRA -> DXB
    (2,4),  # FRA -> SIN
    (2,5),  # FRA -> DOH
    (3,4),  # DXB -> SIN
    (3,5),  # DXB -> DOH
    (5,4),  # DOH -> SIN
    (5,6),  # DOH -> SYD
    (4,6)   # SIN -> SYD
]

# ----------------------------
# Arc costs (hundreds of euros)
# ----------------------------
c = Dict{Tuple{Int,Int}, Int}()

c[(1,2)] = 2    # CPH -> FRA
c[(1,3)] = 6    # CPH -> DXB

c[(2,3)] = 5    # FRA -> DXB
c[(2,4)] = 2    # FRA -> SIN
c[(2,5)] = 5    # FRA -> DOH

c[(3,4)] = 5    # DXB -> SIN
c[(3,5)] = 2    # DXB -> DOH

c[(5,4)] = 6    # DOH -> SIN
c[(5,6)] = 10   # DOH -> SYD

c[(4,6)] = 5    # SIN -> SYD

# ----------------------------
# Model
# ----------------------------
model = Model(HiGHS.Optimizer)
set_silent(model)

@variable(model, x[a in arcs] >= 0)

@objective(model, Min, sum(c[a] * x[a] for a in arcs))

# 1) Source (Copenhagen): outflow - inflow = 1
@constraint(model,
    sum(x[a] for a in arcs if a[1] == S) -
    sum(x[a] for a in arcs if a[2] == S) == 1
)

# 2) Destination (Sydney): inflow - outflow = 1
@constraint(model,
    sum(x[a] for a in arcs if a[2] == T) -
    sum(x[a] for a in arcs if a[1] == T) == 1
)

# 3) Intermediate nodes: inflow = outflow
@constraint(model, [k in 1:m; k != S && k != T],
    sum(x[a] for a in arcs if a[2] == k) ==
    sum(x[a] for a in arcs if a[1] == k)
)

optimize!(model)

println("Termination status: ", termination_status(model))

if termination_status(model) == MOI.OPTIMAL
    println("Objective value (hundreds of €): ", objective_value(model))
    println("Approximate cost (€): ", 100 * objective_value(model))
    println("\nFlows on arcs:")
    for a in arcs
        println("  Flow on arc $a is ", value(x[a]))
    end
else
    println("Model not optimal (maybe infeasible).")
end
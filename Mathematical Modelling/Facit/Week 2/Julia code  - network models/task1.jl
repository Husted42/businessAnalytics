using JuMP
using HiGHS

# ----------------------------
# Nodes (fixed order)
# 1: CPH  (Copenhagen)
# 2: FRA  (Frankfurt)
# 3: DXB  (Dubai)
# 4: SIN  (Singapore)
# 5: DOH  (Doha)
# 6: SYD  (Sydney)
# ----------------------------
nodes = ["CPH", "FRA", "DXB", "SIN", "DOH", "SYD"]
n = 6

S = 1  # CPH
T = 6  # SYD

# ----------------------------
# Cost matrix c[i,j]
# c[i,j] = cost in hundreds of euros
# c[i,j] = 0  ==> arc (i,j) does NOT exist
# ----------------------------
c = [
 #  CPH FRA DXB SIN DOH SYD   
    0   2   6   0   0   0;   # CPH
    0   0   5   2   5   0;   # FRA
    0   0   0   5   2   0;   # DXB
    0   0   0   0   0   5;   # SIN
    0   0   0   6   0  10;   # DOH
    0   0   0   0   0   0    # SYD
]

# ----------------------------
# Model
# ----------------------------
model = Model(HiGHS.Optimizer)
set_silent(model)

# Flow variables on ALL pairs
@variable(model, x[1:n, 1:n] >= 0)

# Fix infeasible arcs to zero (where c[i,j] = 0)
for i in 1:n, j in 1:n
    if c[i,j] == 0
        fix(x[i,j], 0.0; force = true)
    end
end

# Objective: minimize total cost
@objective(model, Min,
    sum(c[i,j] * x[i,j] for i in 1:n, j in 1:n)
)

# ----------------------------
# Flow conservation constraints
# ----------------------------

# Source (Copenhagen): outflow - inflow = 1
@constraint(model,
    sum(x[S,j] for j in 1:n) - sum(x[i,S] for i in 1:n) == 1
)

# Destination (Sydney): inflow - outflow = 1
@constraint(model,
    sum(x[i,T] for i in 1:n) - sum(x[T,j] for j in 1:n) == 1
)

# Intermediate nodes: inflow = outflow
for v in 1:n
    if v != S && v != T
        @constraint(model,
            sum(x[i,v] for i in 1:n) == sum(x[v,j] for j in 1:n)
        )
    end
end

# ----------------------------
# Solve
# ----------------------------
optimize!(model)
println("Termination status: ", termination_status(model))
if termination_status(model) == MOI.OPTIMAL
    println("Termination status: ", termination_status(model))
    println("Optimal objective (hundreds of €): ", objective_value(model))
    println("Approximate cost (€): ", 100 * objective_value(model))

    println("\nChosen arcs:")
    for i in 1:n, j in 1:n
        if value(x[i,j]) > 1e-6
            println("  ", nodes[i], " -> ", nodes[j],
                    "   (x = ", value(x[i,j]), ", c = ", c[i,j], ")")
        end
    end
else
    println("Model not optimal (maybe infeasible).")
end
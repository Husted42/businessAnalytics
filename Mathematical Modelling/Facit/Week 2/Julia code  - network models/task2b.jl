using JuMP, HiGHS

m = 10  # Number of nodes


# Directed arcs
arcs = [
    (1,4), (1,5),
    (3,1), (3,6), (3,8), (3,10),
    (4,3), (4,5), (4,9),            # (4,9) is added here. Dummy arc.
    (5,9),
    (6,9),
    (7,1), (7,2), (7,9),
    (8,4), (8,10),
    (9,1), (9,10),
    (10,7)
]

# ----------------------------
# Arc costs (hundreds of euros)
# ----------------------------
c = Dict{Tuple{Int,Int}, Int}()

c[(1,4)] = 4
c[(1,5)] = 5
c[(3,1)] = 3
c[(3,6)] = 9
c[(3,8)] = 8
c[(3,10)] = 9
c[(4,3)] = 8
c[(4,5)] = 5
c[(4,9)] = 0    # Dummy arc with zero cost
c[(5,9)] = 7
c[(6,9)] = 3
c[(7,1)] = 8
c[(7,2)] = 1
c[(7,9)] = 1
c[(8,4)] = 3
c[(8,10)] = 5
c[(9,1)] = 7
c[(9,10)] = 6
c[(10,7)] = 7

# Option 1
demands = [-30, -30, -50, 250, -20, -30, -30, -40, 0, -20]

# ----------------------------
# Model
# ----------------------------
model = Model(HiGHS.Optimizer)
set_silent(model)

@variable(model, x[a in arcs] >= 0)

@objective(model, Min, sum(c[a] * x[a] for a in arcs))

@constraint(model, [k in 1:m],
    sum(x[a] for a in arcs if a[1] == k) - sum(x[a] for a in arcs if a[2] == k) == demands[k]
)

optimize!(model)

println("Termination status: ", termination_status(model))

if termination_status(model) == MOI.OPTIMAL
    println("Objective value: ", JuMP.objective_value(model))
    for a in arcs
        println("Flow on arc (", a[1], ",", a[2], ") is ", JuMP.value.(x[a]))
    end
end
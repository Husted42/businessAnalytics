using JuMP, HiGHS

########## ---------- Models ---------- ##########
model = Model(HiGHS.Optimizer)
set_optimizer_attribute(model, "log_to_console", false)

include("BurgerBarData.jl")



########## ---------- Variables ---------- ##########
# 1 if on a given day, hour, an employee are working
@variable(model, x[1:D, 1:H, 1:E], Bin)
# 1 if employee starts working at time h
@variable(model, y[1:D, 1:H, 1:E], Bin) 


########## ---------- Objectives ---------- ##########
@objective(model, Min,
    sum(y[d,h,e] for d in 1:D, h in 1:H, e in 1:E)
)

########## ---------- Constraints ---------- ##########
# The demand is exactly covered
@constraint(model, [d in 1:D, h in 1:H],
    sum(x[d,h,e] for e in 1:E) == WorkerDemand[d,h]
)

# Each Employee has to working within target hours in total
@constraint(model, [e in 1:E],
    sum(x[d,h,e] for d in 1:D, h in 1:H) >= Target[e] - 2
)
@constraint(model, [e in 1:E],
    sum(x[d,h,e] for d in 1:D, h in 1:H) <= Target[e] + 2
)


###### ------ Constraints : Adding consecutivess hours ----- #####
# Can only start work once a day
@constraint(model, [d in 1:D, e in 1:E],
    sum(y[d, h, e] for h in 1:H) <= 1
)

# Can only work if he worked the previous hour or if just started working
@constraint(model, [d=1:D, h=1:H, e=1:E],
    x[d,h,e] <= (h > 1 ? x[d, h - 1, e] : 0) + y[d,h,e]
)

# Has to work 2 consecutive hours
@constraint(model, [e in 1:E, d in 1:D],
    sum(x[d, h, e] for h in 1:H) >= 2 * sum(y[d,h,e] for h in 1:H)
)

###### ------ Constraints : Each employee must only work 1 day in the weekeend ----- #####
# Ecah student most at most work 1 day in the weekeend
@constraint(model, [e in 1:E],
    sum(y[6,h,e]  for h in 1:H) + sum(y[7,h,e]  for h in 1:H) <= 1
)

########## ---------- Optimize ---------- ##########
optimize!(model)

println("Optimal solution:")
println("z = ", (objective_value(model)))

println("Schedule")
for d in 1:D
    println("\n Day : ", d)
    println("Hours for a given Employee")
    for h in 1:H
        println(" h = ", h, value.(x[d, h, :]))
    end
    println("Starting working at")
    for h in 1:H
        println(value.(y[d, h, :]))
    end
end

println("Schedule")
for d in 1:D
    println("\n Day : ", d)
    for e in 1:E
        println(sum(value.(x[d, h, e]) for h in 1:H))
    end
end

println("no. of houes")
println(Target)
for e in 1:E
    print("\nE = ", e, " ")
    print(sum(value.(x[d,h,e]) for d in 1:D, h in 1:H), "t = ", Target[e])
end

println("no. of day in weekeend")
println(Target)
for e in 1:E
    print("\nE = ", e, " ")
    print(sum(value.(y[d,h,e]) for d in 6:7, h in 1:H), "t = ", 1)
end



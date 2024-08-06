"""
David Oprea's Set Cover Code
"""

using Graphs, GraphRecipes, Plots   # for plotting graphs
using StatsBase                     # for sample
using Combinatorics                 # for combinations
using Colors                        # to access RGB colors
using DataStructures
using Random

mutable struct MySet
    nums::Vector{Int}
    cons::Int 
    entropy::Int 
    key::Int 
end

ans = Vector{MySet}()
sets = Vector{MySet}()
lookup = Dict{Int, Vector{MySet}}()

"Creates a random universe, or reads from an already created one."
function setup(num_sets, range, lower, higher, filename)
    if filename == ""
        next_available_filename = get_filename()
        println("Using this input file: $next_available_filename")
        make_random_set(next_available_filename, num_sets, range, lower, higher)
    else 
        next_available_filename = filename 
    end
    set = read_data(next_available_filename)
    return set
end

"Write a new set of sets which create a universe within a range."
function make_random_set(filename, num_sets, range, lower, higher)
    open(pwd() * "/input_sets/" * filename, "w") do f
        vals = [Set{Int}() for _ in 1:num_sets]
        for val in 1:range
            iters = rand(lower:higher)
            for _ in 1:iters
                add = rand(1:num_sets)
                push!(vals[add], val)
            end
        end
        for i in 1:num_sets
            if isempty(vals[i])
                push!(vals[i], rand(1:range))
            end
            for val in vals[i]
                write(f, "$(val) ")
            end
            write(f, "\n")
        end
    end
end

"Read a universe from a file"
function read_data(filename)
    sets = Vector{Vector{Int}}()
    io = open(pwd() * "/input_sets/" * filename, "r");
    global lookup
    for line in eachline(io)
        nums = [parse(Int, ss) for ss in split(line)]
        push!(sets, nums)
    end
    for i in 1:maximum(map(maximum, sets))
        lookup[i] = []
    end
    return sets
end

"Return the first available filename of the form s#.txt from the input_sets folder."
function get_filename()
    biggest_number = 1
    while isfile(pwd() * "/input_sets/" * string('s') * string(biggest_number) * ".txt")
        biggest_number = biggest_number + 1
    end
    return string('s') * string(biggest_number) * ".txt"
    # return "s1.txt" # use this for debugging
end

function makeLookup()
    global lookup
    global sets
    for s in sets
        for num in s.nums
            push!(lookup[num], s)
        end
    end
end

"Creates an array of MySet structs as a way of representing the given universe."

function create_set_vector(set)
    sets = Vector{MySet}()
    i = 1
    for s in set
        new_set = MySet(s, 0, length(s), i)
        i += 1
        for num in s
            push!(lookup[num], new_set)
        end
        push!(sets, new_set) 
    end
    for s in sets
        cons = 0
        for num in s.nums
            cons += length(lookup[num])
        end
        s.cons = cons
    end
    return sets
end

function observe()
    global sets
    return sets[argmax([myset.cons / (myset.entropy^1.5) for myset in sets])]
end

function collapse(set)
    global ans
    global sets
    push!(ans, set)
    for num in set.nums
        for other in lookup[num]
            other.cons -= length(lookup[num])
            other.entropy -= 1
            if other.entropy == 0
                filter!(x -> !isequal(x, other), sets)
            end
        end
        lookup[num] = []
    end
    filter!(x -> !isequal(x, set), sets)
end

function propagate(set)
    global sets
    global lookup
    for num in set.nums
        filter!(x -> !isequal(x, set), lookup[num])
        for other in lookup[num]
            other.cons -= 1
        end
        if length(lookup[num]) == 1
            collapse(lookup[num][1])
        end
    end
    filter!(x -> !isequal(x, set), sets)
end

"My 'WFC' algorithm for now, but doesn't entirely work like one"
function WFC(sets)
    global sets
    global ans
    global lookup
    for (key, value) in lookup
        if length(value) == 1
            collapse(value[1])
        end
    end
    sort!(sets, by = x -> x.entropy)
    while !isempty(sets)
        bad = observe()
        propagate(bad)
        # @show ans
    end
    return ans
end

function Greedy(sets)
    global sets
    global ans
    global lookup
    sort!(sets, by = x -> x.cons)
    while !isempty(sets)
        add = sets[argmax([myset.entropy for myset in sets])]
        filter!(x -> !isequal(x, add), sets)
        if add.entropy == 0
            continue
        end
        push!(ans, add)
        for num in add.nums
            for other in lookup[num]
                other.entropy -= 1
                if other.entropy == 0
                    filter!(x -> !isequal(x, other), sets)
                end
            end
            lookup[num] = []
        end
    end
    return ans
end

function BigGreedy(sets, p)
    global sets
    global ans
    global lookup
    while !isempty(sets)
        maxVal = 0
        next = []
        for comb in combinations(sets,p)
            val = 0
            for num in Set([item for sublist in comb for item in sublist.nums])
                if !isempty(lookup[num])
                    val += 1
                end
            end
            if val > maxVal
                maxVal = val
                next = comb
            end
        end
        for s in next
            filter!(x -> !isequal(x, s), sets)
        end
        if maxVal == 0
            continue
        end
        sort(next, by = x -> x.entropy)
        for s in next
            if s.entropy == 0
                continue
            end
            push!(ans, s)
            for num in s.nums
                for other in lookup[num]
                    other.entropy -= 1
                    if other.entropy == 0
                        filter!(x -> !isequal(x, other), sets)
                    end
                end
                lookup[num] = []
            end
        end
    end
    return ans
end

#=
1. Generate the initial solution
2. Initialize the tabu list and the UPPERBOUND parameter
3. Generate the neighborhood of the current solution as described in section 2.2
(apply only ADD SET and REMOVE SET moves)
4. Evaluate the neighborhood solutions
5. Select the solution for the next iteration (see section 2.6)
5. Update the tabu list and UPPERBOUND parameter
7. Go to step 3 if the stopping criteria is not fulfilled, otherwise go in step 8
8. Return the best legal solution
=#

function tabuSearch(sets, range, curSolution)
    global sets
    global ans
    global lookup
    upperBound = length(curSolution)
    tabu = [MySet([], 0, 0, 0) for i in 1:length(curSolution)]
    inTabu = fill(false, length(sets))
    counts = fill(0, range)
    cur = []
    next = sets
    curFitness = 0
    for i in 1:length(sets)
        bestFitness = Inf
        bestSet = next[1]
        for s in next
            if s in cur
                sub = count(counts[num] == 1 for num in s.nums)
                if curFitness - 1 + sub < bestFitness
                    bestSet = s
                    bestFitness = curFitness - 1 + sub
                elseif !inTabu[s.key] && bestSet in cur && (curFitness - 1 + sub == bestFitness) && rand() < 0.5
                    bestSet = s
                    bestFitness = curFitness - 1 + sub
                end
            elseif length(cur) < upperBound - 1
                add = count(counts[num] == 0 for num in s.nums)
                if curFitness + 1 - add < bestFitness
                    bestSet = s
                    bestFitness = curFitness + 1 - add
                elseif !inTabu[s.key] && curFitness + 1 - add == bestFitness && rand() < 0.5
                    bestSet = s
                    bestFitness = curFitness + 1 - add
                end
            end
        end
        if tabu[i].key != 0
            inTabu[tabu[i].key] = false 
        end
        curFitness = bestFitness
        add = 1
        inTabu[bestSet.key] = true
        next = sets
        if bestSet in cur 
            add = -1
            filter!(x -> !isequal(x, bestSet), cur)
            next = Set()
            for num in bestSet.nums
                for other in lookup[num]
                    push!(next, other)
                end
            end
            next = collect(next)
        else
            push!(cur, bestSet)
        end
        for num in bestSet.nums
            counts[num] += add
        end
        push!(tabu, bestSet)
        if check(range, cur)
            curSolution = deepcopy(cur)
            upperBound = length(curSolution)
        end
    end
    return curSolution
end

function getFitness(gene, s, num_sets, range)
    global sets
    cur = []
    for i in 1:num_sets
        if gene[i] == 1
            push!(cur, s[i])
        end
    end
    if check(range, cur)
        return sum(gene)
    end
    return Inf
end

function crossover(parent1, parent2)
    crossoverPoint = rand(1:length(parent1))
    return vcat(parent1[1:crossoverPoint], parent2[(crossoverPoint+1):end]), vcat(parent2[1:crossoverPoint], parent1[(crossoverPoint+1):end])
end

function selectParents(population, fitness_values, num_parents)
    selected_parents = []
    for _ in 1:num_parents
        parent1, parent2 = shuffle(collect(1:length(population)))[1:2]
        if fitness_values[parent1] < fitness_values[parent2]
            push!(selected_parents, population[parent1])
        else
            push!(selected_parents, population[parent2])
        end
    end
    return selected_parents
end

function mutate(off, rate)
    for i in 1:length(off)
        if rand() < rate
            off[i] = abs(off[i] - 1)
        end
    end
    return off
end

function GenAlgo(s, population_size, num_sets, range, steps)
    global ans
    global sets 
    population = []
    best_ans = []
    lowest_fitness = Inf
    for _ in 1:population_size
        idx = rand(1:num_sets)
        randSet = s[idx]
        ans = []
        sets = deepcopy(s)
        makeLookup()
        collapse(randSet)
        ans = WFC(sets)
        gene = zeros(num_sets)
        for set in ans
            gene[set.key] = 1
        end
        if getFitness(gene, s, num_sets, range) < lowest_fitness
            best_ans = gene
            lowest_fitness = getFitness(gene, s, num_sets, range)
        end
        push!(population, gene)
    end
    # @show best_ans
    # @show population
    sort!(s, by = x -> x.key)
    for _ in 1:steps
        fitness_values = [getFitness(gene, s, num_sets, range) for gene in population]

        if argmin(fitness_values) == Inf
            continue
        end

        new_pop = []

        parents = selectParents(population, fitness_values, population_size)
        for i in 1:2:population_size
            off1, off2 = crossover(parents[i], parents[i+1])
            push!(new_pop, mutate(off1, 0.02))
            push!(new_pop, mutate(off2, 0.02))
        end

        population = new_pop
        if getFitness(population[argmin(fitness_values)], s, num_sets, range) < lowest_fitness
            best_ans = population[argmin(fitness_values)]
            @show best_ans
            lowest_fitness = getFitness(population[argmin(fitness_values)], s, num_sets, range)
        end
    end
    ans = []
    for i in 1:num_sets
        if best_ans[i] == 1
            push!(ans, s[i])
        end
    end
    return ans
end


"checks to see if a set of sets works when they become a union"
function check(range, ans)
    uniqs = Set{Int}()
    for s in ans
        for num in s.nums
            push!(uniqs, num)
        end
    end
    return length(uniqs) == range
end

"Brute force algorithm to see if I found optimal answer, try this when num_sets is low"
function seeIfBest(range, sets, r)
    for comb in combinations(sets, r)
        if check(range, comb)
            @show length(comb)
            @show comb
            @show check(range, comb)
            return false
        end
    end
    return true
end

"Main control for this program. 
num_sets is the number of sets in the universe 
range is the range of numbers that will appear from 1:range
lower is the lower bound for how many of each number will appear in the sets
higher is the higher bound for how many of each number will appear in the sets"
function main(num_sets = 0, range = 0, lower = 1, higher = 10, filename = "s14.txt")
    global sets
    global ans
    set = setup(num_sets, range, lower, higher, filename)
    sets = create_set_vector(set)
    orig_sets = deepcopy(sets)
    num_sets = length(sets)
    range = maximum([maximum(set.nums) for set in sets])
    #@show sets
    #@show lookup
    println("WFC")
    @time begin
        vals = WFC(sets)
    end
    @show length(vals)
    @show vals
    @show check(range, ans)

    println("Genetic Algorithm + WFC")
    @time begin
        GenAlgo(orig_sets, num_sets ÷ 4, num_sets, range, 100)
    end
    @show length(ans)
    @show ans
    @show check(range, ans)

    #=
    @show seeIfBest(range, orig_sets, length(ans)-1)
    # report(graph, edges, nodes, "WFC")

    ans = []
    sets = deepcopy(orig_sets)
    makeLookup()

    println("Big Greedy")
    @time begin
        BigGreedy(sets, 2)
    end
    @show length(ans)
    @show ans
    @show check(range, ans)
    =#

    ans = []
    sets = deepcopy(orig_sets)
    makeLookup()

    println("Greedy")
    @time begin
        Greedy(sets)
    end
    @show length(ans)
    @show ans
    @show check(range, ans)


    sets = deepcopy(orig_sets)
    makeLookup()

    println("Tabu Search")
    @time begin
        ans = tabuSearch(sets, range, ans)
    end
    @show length(ans)
    @show ans
    @show check(range, ans)

end

main()

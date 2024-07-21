"""
David Oprea's Set Cover Code
"""

using Graphs, GraphRecipes, Plots   # for plotting graphs
using StatsBase                     # for sample
using Combinatorics                 # for combinations
using Colors                        # to access RGB colors
using DataStructures

mutable struct MySet
    nums::Vector{Int}
    cons::Int
    entropy::Int
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

"Creates an array of MySet structs as a way of representing the given universe."

function create_set_vector(set)
    sets = Vector{MySet}()
    for s in set
        new_set = MySet(s, 0, length(s))
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
    return sets[argmax([myset.cons / myset.entropy for myset in sets])]
end

function collapse(set)
    global ans
    global sets
    push!(ans, set)
    for num in set.nums
        for other in lookup[num]
            other.cons -= length(lookup[num])
            other.entropy -= 1
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
    sort!(sets, by = x -> x.entropy, rev = true)
    while !isempty(sets)
        bad = observe()
        propagate(bad)
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
function main(num_sets = 100, range = 100, lower = 10, higher = 20, filename = "s7.txt")
    global sets
    set = setup(num_sets, range, lower, higher, filename)
    sets = create_set_vector(set)
    orig_sets = deepcopy(sets)
    #@show sets
    #@show lookup
    @time begin
        vals = WFC(sets)
    end
    @show length(vals)
    @show vals
    @show check(range, ans)
    @show seeIfBest(range, orig_sets, length(vals)-1)
    # report(graph, edges, nodes, "WFC")
end

main()

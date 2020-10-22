if ARGV.size > 1
  n = ARGV[1].to_i
else
  n = 75000
end

MAX_WEIGHT = 1000000000

edges = []
(n - 1).times { |i|
  edges.push([rand(1...i + 2), i + 2])
}

rand(3..n).times {
  edges.push([rand(1..n), rand(1..n)])
}

puts n.to_s + " " + edges.size.to_s
edges.each { |g|
  puts g[0].to_s + " " + g[1].to_s + " " + rand(-MAX_WEIGHT..MAX_WEIGHT).to_s
}
-- amortize syscalls: send 16 pipelined GETs per write, so throughput reflects the
-- framework's request-processing cost rather than per-request read/write overhead
init = function(args)
  local r = {}
  for i = 1, 16 do r[i] = wrk.format(nil, wrk.path) end
  req = table.concat(r)
end
request = function() return req end

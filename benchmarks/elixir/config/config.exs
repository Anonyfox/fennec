import Config

# keep the logger quiet (no per-request noise) so it doesn't skew throughput
config :logger, level: :warning

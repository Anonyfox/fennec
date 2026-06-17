defmodule PawBench.Router do
  # Idiomatic Plug.Router — the composable building block. No Plug.Logger (no per-request logging).
  use Plug.Router

  plug(:match)
  plug(:dispatch)

  get "/plaintext" do
    conn |> put_resp_content_type("text/plain") |> send_resp(200, "Hello, World!")
  end

  get "/json" do
    conn |> put_resp_content_type("application/json") |> send_resp(200, ~s({"message":"Hello, World!"}))
  end

  get "/user/:id" do
    conn |> put_resp_content_type("text/plain") |> send_resp(200, id)
  end

  match _ do
    send_resp(conn, 404, "")
  end
end

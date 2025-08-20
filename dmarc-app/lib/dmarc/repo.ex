defmodule Dmarc.Repo do
  use Ecto.Repo,
    otp_app: :dmarc,
    adapter: Ecto.Adapters.Postgres
end

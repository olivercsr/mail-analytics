defmodule Auth.Jwt do
  use Joken.Config

  # TODO: implement:
  #  - expiry (copy value from 3rd party token?)
  #  - ...

  @impl true
  def token_config do
    default_claims()
      |> add_claim("sub", nil, fn sub -> String.length(sub) > 5 end)
  end
end


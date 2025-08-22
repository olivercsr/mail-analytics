defmodule Auth.Jwt do
  use Joken.Config

  defp is_email_address(str) do
    re = ~r"^[^@\s]+@[^@\s]+\.[^@\s]+$"
    [email] = Regex.run(re, str)
    case email do
      email when email != nil -> true
      _ -> false
    end
  end

  @impl true
  def token_config do
    default_claims()
      |> add_claim("sub", nil, fn sub -> is_binary(sub) and String.length(sub) > 0 end)
      |> add_claim("email", nil, &is_email_address/1)
      |> add_claim("provider", nil, fn provider -> is_binary(provider) and String.length(provider) > 0 end)
  end
end

